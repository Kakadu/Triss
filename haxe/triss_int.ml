open Common
open Type
open Core_extended
open Core_extended.Function

(* TODO: add Core_extended.Readline support
open Core_extended
open Core
let compl ~left:s1 ~right:__ =
        if  Core_string.is_prefix ":show" ~prefix:s1 then [":show"; ":show2"] 
        else ( []) ;;

let s = Readline.input_line ~tab_completion:compl ();; 
print_endline ("`" ^ s ^ "'");;  *)

type str_pair = string*string deriving (Show)
type str_option = string option deriving (Show)

let show_module_type = function
  | TClassDecl cd -> print_endline "TClassDecl"
  | TEnumDecl ed -> print_endline "TEnumDecl"
  | TTypeDecl td -> print_endline ("TTypeDecl " (*^ show_tdef td*))

let module_def_show def = 
  String.concat "" ["{ mpath="; Show.show<Type.path> def.mpath; "mtypes=<not implemented>}"]
 
exception ExitInteractive
let interactive (ctx : Common.context ) = 
  let wrap (f:string -> bool) s : bool= 
    try f s with Scanf.Scan_failure _ | End_of_file -> false in

  let type_n s : bool = begin
    let func s1 s2 n = if s1=":show" && s2="type" then (print_endline (Show.show<Type.module_type> (List.nth ctx.types n)); true)
      else false in
    wrap (fun s -> Scanf.sscanf s "%s %s %d" func) s end in

  let comap_n s : bool = begin
    let func s1 n = if s1=":comap" then begin 
      let ch = open_out "dump.comap" in
      let m = List.nth ctx.modules n in
      let xml = Type_comap.comap_module_def m in
      Type_comap.to_ch xml ch;
      close_out ch;
      print_endline "see output in dump.comap";
      true
    end else false in
    wrap (fun s -> Scanf.sscanf s "%s %d" func) s end in

  let module_n s : bool = begin
    let func s1 s2 n = if s1=":show" && s2="module" then begin
      let cur_mod = List.nth ctx.modules n in
      print_endline ("showing module "^ (Show.show<Type.path> cur_mod.mpath) );
      let cldecl = List.hd cur_mod.mtypes in
      match cldecl with
	| TClassDecl cd -> begin
	  let u1 = (List.hd cd.cl_ordered_statics).cf_type in 
	  (match u1 with 
            | TLazy u2 -> let u3 = !u2 () in ()
	    | _ -> ());
	  true
	  end
	| _ -> true
      end
      else false in
    wrap (fun s -> Scanf.sscanf s "%s %s %d" func) s end in

  let loop () =
    while true do
      let test_module = List.filter (fun m ->  snd m.mpath = "Test") ctx.modules in
      let cldecl = List.hd test_module in
(*      let s = Show.show<module_def> cldecl in *)
      print_string "> ";
      let s = ExtLib.String.strip (read_line ()) in
      if type_n s then () else
      if module_n s then () else
      if comap_n s then () 
      else match s with      
        | ":q" | ":quit" -> raise ExitInteractive
        | ":show file" -> print_endline ctx.file
	| ":show types" -> List.iter show_module_type ctx.types
	| ":show modules" -> (*List.iter (fun x -> print_endline (module_def_show x))  ctx.modules *)
	               List.iter (print_endline $ Show.show<Type.module_def> ) ctx.modules;
	| ":show module_names" ->
	  List.iter (fun x -> print_endline  (Show.show<Type.path> x.mpath)) ctx.modules
	| ":show resources" -> 
	  let func a b = print_endline (Show.show<str_pair> (a,b)) in
	  print_endline ("Total resources count: "^ (string_of_int (Hashtbl.length ctx.resources)));
	  Hashtbl.iter func ctx.resources
        | _    -> print_endline "wrong command"
    done
  in 
  try 
    loop ()
  with ExitInteractive -> ()
  
