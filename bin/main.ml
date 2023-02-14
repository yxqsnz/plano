open Plano
type plano_meta = {
    mutable name: string;
    mutable srcs: string list; 
    mutable incs: string list;
    mutable deps: string list;
    mutable cc: string;
}

exception Error of string

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
                                                    | "incs" -> meta.incs <- value :: meta.srcs
                                                    | "deps" -> meta.deps <- value :: meta.srcs
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


let display_slist sl =
    sl |> List.fold_left ( fun x y -> y ^ ", " ^ x ) ""

let file_info x = 
    Printf.printf "Name: %s\nCompiler: %s\nSources: %s\nIncludes: %s\nDeps: %s\n" x.name 
            x.cc 
            (display_slist x.srcs) (display_slist x.incs) (display_slist x.deps)

let () = 
    let result = parse_file "example/Plano" |> check_file
    in file_info result

  