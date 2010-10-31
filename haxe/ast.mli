type pos = { pfile : string; pmin : int; pmax : int; }
module rec Show_pos :
  sig
    type a = pos
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type keyword =
    Function
  | Class
  | Var
  | If
  | Else
  | While
  | Do
  | For
  | Break
  | Continue
  | Return
  | Extends
  | Implements
  | Import
  | Switch
  | Case
  | Default
  | Static
  | Public
  | Private
  | Try
  | Catch
  | New
  | This
  | Throw
  | Extern
  | Enum
  | In
  | Interface
  | Untyped
  | Cast
  | Override
  | Typedef
  | Dynamic
  | Package
  | Callback
  | Inline
  | Using
module rec Show_keyword :
  sig
    type a = keyword
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type binop =
    OpAdd
  | OpMult
  | OpDiv
  | OpSub
  | OpAssign
  | OpEq
  | OpNotEq
  | OpGt
  | OpGte
  | OpLt
  | OpLte
  | OpAnd
  | OpOr
  | OpXor
  | OpBoolAnd
  | OpBoolOr
  | OpShl
  | OpShr
  | OpUShr
  | OpMod
  | OpAssignOp of binop
  | OpInterval
module rec Show_binop :
  sig
    type a = binop
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type unop = Increment | Decrement | Not | Neg | NegBits
module rec Show_unop :
  sig
    type a = unop
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type constant =
    Int of string
  | Float of string
  | String of string
  | Ident of string
  | Type of string
  | Regexp of string * string
module rec Show_constant :
  sig
    type a = constant
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type token =
    Eof
  | Const of constant
  | Kwd of keyword
  | Comment of string
  | CommentLine of string
  | Binop of binop
  | Unop of unop
  | Semicolon
  | Comma
  | BrOpen
  | BrClose
  | BkOpen
  | BkClose
  | POpen
  | PClose
  | Dot
  | DblDot
  | Arrow
  | IntInterval of string
  | Macro of string
  | Question
  | At
module rec Show_token :
  sig
    type a = token
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type unop_flag = Prefix | Postfix
module rec Show_unop_flag :
  sig
    type a = unop_flag
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type while_flag = NormalWhile | DoWhile
module rec Show_while_flag :
  sig
    type a = while_flag
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type type_path = {
  tpackage : string list;
  tname : string;
  tparams : type_param_or_const list;
  tsub : string option;
}
and type_param_or_const = TPType of complex_type | TPConst of constant
and anonymous_field =
    AFVar of complex_type
  | AFProp of complex_type * string * string
  | AFFun of (string * bool * complex_type) list * complex_type
and complex_type =
    CTPath of type_path
  | CTFunction of complex_type list * complex_type
  | CTAnonymous of (string * bool option * anonymous_field * pos) list
  | CTParent of complex_type
  | CTExtend of type_path *
      (string * bool option * anonymous_field * pos) list
module rec Show_type_path :
  sig
    type a = type_path
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
and Show_type_param_or_const :
  sig
    type a = type_param_or_const
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
and Show_anonymous_field :
  sig
    type a = anonymous_field
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
and Show_complex_type :
  sig
    type a = complex_type
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type func = {
  f_args : (string * bool * complex_type option * expr option) list;
  f_type : complex_type option;
  f_expr : expr;
}
and expr_def =
    EConst of constant
  | EArray of expr * expr
  | EBinop of binop * expr * expr
  | EField of expr * string
  | EType of expr * string
  | EParenthesis of expr
  | EObjectDecl of (string * expr) list
  | EArrayDecl of expr list
  | ECall of expr * expr list
  | ENew of type_path * expr list
  | EUnop of unop * unop_flag * expr
  | EVars of (string * complex_type option * expr option) list
  | EFunction of func
  | EBlock of expr list
  | EFor of string * expr * expr
  | EIf of expr * expr * expr option
  | EWhile of expr * expr * while_flag
  | ESwitch of expr * (expr list * expr) list * expr option
  | ETry of expr * (string * complex_type * expr) list
  | EReturn of expr option
  | EBreak
  | EContinue
  | EUntyped of expr
  | EThrow of expr
  | ECast of expr * complex_type option
  | EDisplay of expr * bool
  | EDisplayNew of type_path
  | ETernary of expr * expr * expr
and expr = expr_def * pos
module rec Show_func :
  sig
    type a = func
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
and Show_expr_def :
  sig
    type a = expr_def
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
and Show_expr :
  sig
    type a = expr_def * pos
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type type_param = string * type_path list
module rec Show_type_param :
  sig
    type a = string * type_path list
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type documentation = string option
module rec Show_documentation :
  sig
    type a = string option
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type metadata = (string * expr list) list
module rec Show_metadata :
  sig
    type a = (string * expr list) list
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type access = APublic | APrivate | AStatic | AOverride | ADynamic | AInline
module rec Show_access :
  sig
    type a = access
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type class_field =
    FVar of string * documentation * metadata * access list *
      complex_type option * expr option
  | FFun of string * documentation * metadata * access list *
      type_param list * func
  | FProp of string * documentation * metadata * access list * string *
      string * complex_type
module rec Show_class_field :
  sig
    type a = class_field
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type enum_flag = EPrivate | EExtern
module rec Show_enum_flag :
  sig
    type a = enum_flag
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type class_flag =
    HInterface
  | HExtern
  | HPrivate
  | HExtends of type_path
  | HImplements of type_path
module rec Show_class_flag :
  sig
    type a = class_flag
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type enum_constructor =
    string * documentation * metadata * (string * bool * complex_type) list *
    pos
module rec Show_enum_constructor :
  sig
    type a =
        string * documentation * metadata *
        (string * bool * complex_type) list * pos
    val format : Format.formatter -> a -> unit
    val format_list : Format.formatter -> a list -> unit
    val show : a -> string
    val show_list : a list -> string
  end
type ('a, 'b) definition = {
  d_name : string;
  d_doc : documentation;
  d_params : type_param list;
  d_meta : metadata;
  d_flags : 'a list;
  d_data : 'b;
}
type class_def = (class_flag, (class_field * pos) list) definition
type enum_def = (enum_flag, enum_constructor list) definition
type type_def =
    EClass of class_def
  | EEnum of enum_def
  | ETypedef of (enum_flag, complex_type) definition
  | EImport of type_path
  | EUsing of type_path
type type_decl = type_def * pos
type package = string list * type_decl list
val pos : 'a * 'b -> 'b
val is_postfix : expr_def * 'a -> unop -> bool
val is_prefix : unop -> bool
val base_class_name : 'a * 'b -> 'b
val null_pos : pos
val punion : pos -> pos -> pos
val s_type_path : string list * string -> string
val parse_path :
  Core.Core_string.t -> Core.Core_string.t list * Core.Core_string.t
val s_escape : string -> string
val s_constant : constant -> string
val s_keyword : keyword -> string
val s_binop : binop -> string
val s_unop : unop -> string
val s_token : token -> string
val unescape : string -> string
