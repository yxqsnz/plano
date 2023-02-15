open Plano
open Printf
type plano_meta = {
    mutable name: string;
    mutable srcs: string list; 
    mutable incs: string list;
    mutable deps: string list;
    mutable cc: string;
}

exception Error of string

let join sep sl =
    sl |> List.fold_left ( fun x y -> x ^ sep  ^ y ) ""

let parse_instruction line =
  String.split_on_char ' ' line 
      |> List.map Parser.lex
        

let lex_file f = 
    let try_read () = try Some (input_line f) with End_of_file -> None in
    let rec parse_line acc = match try_read () with
        | Some line -> String.trim line |> parse_instruction |> List.append acc |> parse_line
        | None -> close_in f; acc;
    in parse_line [] 


let parse_file name = 
    let build_file = open_in name
    in try 
      let lexems = lex_file build_file 
        in Parser.parse_lexem [] lexems 
    with e -> 
        close_in_noerr build_file;
        raise e

let check_file tokens =
    let meta = { name = "a Plano project";
                 cc = "find_path:gcc";
                 srcs = []; 
                 incs = []; 
                 deps = [] }
    in let evaluate meta token = 
        match token with
            | Parser.AppendProp (name, value) -> (match name with 
                                                    | "name" -> raise (Error "cant append to `name`")
                                                    | "cc" -> raise (Error "cant append to `cc`")
                                                    | "srcs" -> meta.srcs <- value :: meta.srcs
                                                    | "incs" -> meta.incs <- value :: meta.incs
                                                    | "deps" -> meta.deps <- value :: meta.deps
                                                    | _ -> raise (Error "cant append to a unknown field"))

            | Parser.SetProp (name, value) -> (match name with 
                                                | "name" -> meta.name <- value
                                                | "cc" -> meta.cc <- value
                                                | "srcs" -> meta.srcs <- [value]
                                                | "deps" -> meta.deps <- [value]
                                                | "incs" -> meta.incs <- [value]
                                                | _ -> raise (Error "cant set to a unknown field"))
            | Parser.Invalid error -> raise (Error error)
        

    in let _ = tokens |> List.map (evaluate meta)
    in meta


let prepare_meta meta =
    let cc = match String.split_on_char ':' meta.cc with
        | [ "find_path"; value ] -> Calls.find_path value
        | [ res ] -> res
        | _ -> raise (Error "You must specify a compiler")
    in let deps = meta.deps |> List.map (fun dep -> 
                match String.split_on_char ':' dep with
                | [ "pkgconf"; pkg ] -> Calls.pkgconf pkg
                | [ res ] -> res
                | _ -> raise (Error "You must specify a dependency"))
    in meta.cc <- cc; 
    meta.deps <- deps;
    meta

let file_info x =
    let j = join " " 
     in printf "Name: %s\nCompiler: %s\nSources: %s\nIncludes: %s\nDeps: %s\n" x.name 
            x.cc 
            (j x.srcs) (j x.incs) (j x.deps)

let build_output_object x = (Str.global_replace (Str.regexp "/") "_"  x) ^ ".o"

let execute proj_folder meta =
    Sys.chdir proj_folder;
    let _ = try let _ = Sys.is_directory "build" in print_endline "[oo] not creating build directory.";
            with Sys_error _ -> Sys.mkdir "build" 0o0766;
    in
    let _ = meta.srcs |> List.map (fun src ->
        printf "[--] building source: %s\n" src;
        let command = sprintf "%s -c -o %s %s %s %s" meta.cc 
                    ("build/" ^ (build_output_object src)) 
                    (join " " meta.deps) (join " -I" meta.incs) src

       in printf "$ %s\n" command;
        flush_all ();

        match Sys.command command with
            | 0  -> ()
            | status -> printf "[!!] build failed with status %d. Aborting!\n" status;
                        exit 1
    )
    in let link_command = sprintf "%s -o %s %s %s %s" meta.cc 
                    ("build/" ^ meta.name ^ ".exe") 
                    (join " " (List.map (fun x -> "build/" ^ (build_output_object x)) meta.srcs))
                    (join " " meta.deps) 
                    (join " -I" meta.incs)
    in print_endline "[oo] linking executable..";
        printf "$ %s\n" link_command;
        flush_all ();

      match Sys.command link_command with
        | 0 -> ()
        | status -> printf "[!!] link failed with status %d. Aborting!\n" status;
                    exit 1


let () = if Array.length Sys.argv = 2 then
            let proj_folder = Sys.argv.(1) in 
            let result = parse_file (proj_folder ^ "/Plano") |> check_file |> prepare_meta
            in file_info result;
            let _ = execute proj_folder result
            in ()
    else
        print_endline "error: you must tell me a folder or invalid usage."
 
  