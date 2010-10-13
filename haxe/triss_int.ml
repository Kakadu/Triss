exception ExitInteractive
let interactive ctx = 
  let loop () =
    while true do
      match read_line () with
        | "#quit;;" -> raise ExitInteractive
        | s    -> print_endline s
    done
  in 
  try 
    loop ()
  with ExitInteractive -> ()
  
