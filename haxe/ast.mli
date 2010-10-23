type pos = { pfile : string; pmin : int; pmax : int; }
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
type unop = Increment | Decrement | Not | Neg | NegBits
type constant =
    Int of string
  | Float of string
  | String of string
  | Ident of string
  | Type of string
  | Regexp of string * string
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
type unop_flag = Prefix | Postfix
type while_flag = NormalWhile | DoWhile
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
type type_param = string * type_path list
type documentation = string option
type metadata = (string * expr list) list
type access = APublic | APrivate | AStatic | AOverride | ADynamic | AInline
type class_field =
    FVar of string * documentation * metadata * access list *
      complex_type option * expr option
  | FFun of string * documentation * metadata * access list *
      type_param list * func
  | FProp of string * documentation * metadata * access list * string *
      string * complex_type
type enum_flag = EPrivate | EExtern
type class_flag =
    HInterface
  | HExtern
  | HPrivate
  | HExtends of type_path
  | HImplements of type_path
type enum_constructor =
    string * documentation * metadata * (string * bool * complex_type) list *
    pos
type ('a, 'b) definition = {
  d_name : string;
  d_doc : documentation;
  d_params : type_param list;
  d_meta : metadata;
  d_flags : 'a list;
  d_data : 'b;
}
type type_def =
    EClass of (class_flag, (class_field * pos) list) definition
  | EEnum of (enum_flag, enum_constructor list) definition
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
val parse_path : string -> string list * string
val s_escape : string -> string
val s_constant : constant -> string
val s_keyword : keyword -> string
val s_binop : binop -> string
val s_unop : unop -> string
val s_token : token -> string
val unescape : string -> string
