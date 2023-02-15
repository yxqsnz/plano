exception UnameToFindLibrary of string

let find_path it =
    Sys.getenv("PATH")
    |> String.split_on_char ':' 
    |> List.map (fun x -> Array.to_list (Sys.readdir x))
    |> List.flatten
    |> List.find (fun x -> Filename.check_suffix x it)

let pkgconf pkg = 
    let output = Os.capture_output_of ("pkgconf --cflags --libs " ^ pkg)
    in match output with
     | "" ->  raise (UnameToFindLibrary ("uname to find library " ^ pkg))
     | out -> out