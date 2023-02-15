exception UnableToFindLibrary of string
exception UnableToFindInPath of string

let find_path it =
    try Sys.getenv("PATH")
    |> String.split_on_char ':' 
    |> List.map (fun x -> Array.to_list (Sys.readdir x))
    |> List.flatten
    |> List.find (fun x -> Filename.check_suffix x it)
    with Not_found -> raise (UnableToFindInPath ("can't find " ^ it ^ " in $PATH."))


let pkgconf pkg = 
    let output = Os.capture_output_of ("pkgconf --cflags --libs " ^ pkg)
    in match output with
     | "" ->  raise (UnableToFindLibrary ("uname to find library " ^ pkg))
     | out -> out