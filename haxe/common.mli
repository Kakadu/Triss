type package_rule = Forbidden | Directory of string | Remap of string
type platform = Cross | Flash | Js | Neko | Flash9 | Php | Cpp
type pos = Ast.pos
type basic_types = {
  mutable tvoid : Type.t;
  mutable tint : Type.t;
  mutable tfloat : Type.t;
  mutable tbool : Type.t;
  mutable tnull : Type.t -> Type.t;
  mutable tstring : Type.t;
  mutable tarray : Type.t -> Type.t;
}
type context = {
  version : int;
  mutable debug : bool;
  mutable verbose : bool;
  mutable foptimize : bool;
  mutable platform : platform;
  mutable std_path : string list;
  mutable class_path : string list;
  mutable main_class : Type.path option;
  mutable defines : (string, unit) PMap.t;
  mutable package_rules : (string, package_rule) PMap.t;
  mutable error : string -> pos -> unit;
  mutable warning : string -> pos -> unit;
  mutable js_namespace : string option;
  mutable load_extern_type : (Type.path -> pos -> Ast.package option) list;
  mutable file : string;
  mutable flash_version : int;
  mutable modules : Type.module_def list;
  mutable types : Type.module_type list;
  mutable resources : (string, string) Hashtbl.t;
  mutable php_front : string option;
  mutable swf_libs :
    (string * (unit -> Swf.swf) *
     (unit -> (string list * string, As3hl.hl_class) Hashtbl.t))
    list;
  mutable basic : basic_types;
  mutable lines : Lexer.line_index;
}
exception Abort of string * Ast.pos
val display : bool ref
val create : int -> context
val clone : context -> context
val platforms : platform list
val platform_name : platform -> string
val defined : context -> string -> bool
val define : context -> string -> unit
val init_platform : context -> platform -> unit
val error : string -> Ast.pos -> 'a
val platform : context -> platform -> bool
val find_file : context -> string -> string
val get_full_path : string -> string
type timer_infos = {
  name : string;
  mutable start : float;
  mutable total : float;
}
val get_time : unit -> float
val htimers : (string, timer_infos) Hashtbl.t
val new_timer : string -> timer_infos
val curtime : timer_infos list ref
val close : timer_infos -> unit
val timer : string -> unit -> unit
val close_time : unit -> unit
