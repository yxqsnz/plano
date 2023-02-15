open Unix

let channel_to_string channel =
  let rec loop buf =
    try let line =(input_line channel)
      in loop (buf ^ line)
      with End_of_file -> close_in channel; buf

  in loop ""


let capture_output_of command =
  let (stdout, stdin, _) = open_process_full command [||] in 
    close_out stdin;
    channel_to_string stdout