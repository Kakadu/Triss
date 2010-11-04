(* big functions for showing cross-rec types *)
open Xml
open Type

let rec mkn ?s:(s=[]) ?lst:(lst=[]) txt = 
  let text_node = Element ("text",[],[PCData txt]) in
  let str_nodes = List.map (fun s -> mkn s) s in
  Element ("node",[],text_node :: str_nodes @ lst)

let none_node = mkn ~lst:[]  "None"
let some_node n = mkn ~lst:[n] "Some"
let bool_node b = mkn (string_of_bool b)
let opt_node  func = function
  | None -> mkn "None"
  | Some z -> func z
let comap_path (ps,s) = mkn ~s:[String.concat "." ps; s] "path"

let comap_pos (p:Ast.pos) = begin
  let module Q = struct
	open Ast
	let f = mkn ~s:["\""^p.pfile^"\""; string_of_int p.pmin; string_of_int p.pmax ] "pos"
  end in
  Q.f
end

let comap_field_kind fk = mkn (Show.show<Type.field_kind> fk)

let depth = ref 0

let wrp func arg = if (!depth < 50) then
    (incr depth; let res = func arg in decr depth; res)
  else mkn "<depth limit>"

let comap_PMap f1 f2 s t =
  let lst = ref [] in
  PMap.iter (fun a b -> let item = mkn s ~lst:[wrp f1 a; wrp f2 b] in
                          lst := item :: !lst
  );
  !lst

let loaded_paths = ref []
let future_loaded_types = ref []
let rec comap_t = function
  | TMono a -> let son = opt_node (wrp comap_t) !a in
	       mkn ~lst:[son] "TMono"
  | TEnum (e,p) -> mkn  ~lst:[wrp comap_tenum  e; wrp comap_tparams p] "TEnum";
  | TInst (c,p) -> mkn  ~lst:[wrp comap_tclass c; wrp comap_tparams p] "TInst"
  | TType (d,p) -> mkn  ~lst:[wrp comap_tdef   d; wrp comap_tparams p] "TType"
  | TFun (ls,t) -> 
       let ll = List.map (fun (s,b,t) -> mkn "" ~s:[s;string_of_bool b] ~lst:[wrp comap_t t]) ls in
       mkn "TFun" ~lst:((wrp comap_t t) ::  ll)
  | TAnon a     -> mkn  ~lst:[wrp comap_tanon a] "TAnon"
  | TDynamic d  -> mkn  ~s:["...."] "TDynamic"
  | TLazy  r    -> mkn  "TLazy" ~lst:[wrp comap_t (!r ())] 

and comap_tparams lst = mkn ~lst:(List.map comap_t lst) "tparams"

and comap_tfunc f = 
  let args = List.map (fun (s,op,t) -> 
    let op_str = match op with None -> "None" | Some z -> Show.show<tconstant> z in
    let t = wrp comap_texpr f.tf_expr in
    mkn "item?" ~s:[s; op_str] ~lst:[t] ) f.tf_args in
  let typ  = mkn "tf_type" ~lst:[wrp comap_t f.tf_type] in
  let e = wrp comap_texpr f.tf_expr in
  mkn "tfunc" ~lst:(e :: typ :: args)

and comap_anon_status = function
  | Closed           -> mkn "Closed"
  | Opened           -> mkn "Opened"
  | Type.Const       -> mkn "Const"
  | Statics s        -> mkn ~lst:[wrp comap_tclass s] "Statics"
  | EnumStatics e    -> mkn ~lst:[wrp comap_tenum e] "EnumStatics"

and comap_tanon a = 
  let st = wrp comap_anon_status !(a.a_status) in
  let ll = comap_PMap mkn (wrp comap_tclass_field) "item" a.a_fields in 
  mkn "tanon" ~s:["????"] ~lst:ll

and comap_texpr_expr = function
  | TConst c         -> mkn "TConstant" ~s:[Show.show<tconstant> c]
  | TLocal l         -> mkn "TLocal"  ~s:[l]
  | TEnumField (e,s) -> mkn "TEnumField" ~s:[s] ~lst:[wrp comap_tenum e]
  | TArray  (e1,e2)  -> mkn "TArray" ~lst:[wrp comap_texpr e1; wrp comap_texpr e2]
  | TBinop (op,e1,e2) -> 
       mkn "TBinOp" ~s:[Show.show<Ast.binop> op] ~lst:[wrp comap_texpr e1; wrp comap_texpr e2]
  | TField (e,s) -> mkn "TField" ~s:[s] ~lst:[wrp comap_texpr e]
  | TClosure (e,s) -> mkn "TClosure" ~s:[s] ~lst:[wrp comap_texpr e]
  | TTypeExpr _ -> mkn "TTypeExpr"
  | TParenthesis e -> mkn "TParentehesis" ~lst:[wrp comap_texpr e]
  | TObjectDecl lst -> mkn "TObjectDecl"
  | TArrayDecl ls -> mkn "TArrayDecl"
  | TCall (e,es) -> 
    let ll = List.map (wrp comap_texpr) es in    
    mkn "TCall" ~lst:[wrp comap_texpr e; mkn "???" ~lst:ll ]
  | TNew (c,p,es) -> 
      let lst = List.map (wrp comap_texpr) es in
      mkn "TNew" ~lst:((wrp comap_tclass c) :: (wrp comap_tparams p) :: lst  )
  | TUnop (op,f,e) -> 
    mkn "TUnOp" ~s:[Show.show<Ast.unop> op; Show.show<Ast.unop_flag> f] ~lst:[wrp comap_texpr e]
  | TFunction f -> mkn "TFunction" ~lst:[wrp comap_tfunc f]
  | TVars lst ->
    let ll = List.map (fun (s,t,eo) -> 
      mkn "???" ~s:[s] ~lst:[wrp comap_t t; opt_node (wrp comap_texpr) eo]
    ) lst in
    mkn "TVars" ~lst:ll
  | TBlock es -> mkn "TBlock" ~lst:(List.map (wrp comap_texpr) es)
  | TFor (s,t,e1,e2) -> mkn "TFor"
  | TIf (e1,e2,eo) -> mkn "TIf"
  | TWhile (e1,e2,f) -> mkn "TWhile"
  | TSwitch (e1,kst,eo) -> mkn "TSwitch"
  | TMatch (e,(en,p),lst,eo) -> mkn "TMatch"
  (*of texpr * (tenum* tparams) * (int list * (string option * t) list option * texpr) list * texpr option *)
  | TTry (e,ls) -> 
    let ll = List.map (fun (s,t,e) -> mkn "??" ~s:[s] ~lst:[wrp comap_t t; wrp comap_texpr e]) ls in
    mkn "TTry" ~lst:((wrp comap_texpr e):: ll)
  | TReturn e -> mkn "TReturn" ~lst:[opt_node (wrp comap_texpr) e]
  | TBreak    -> mkn "TBreak"
  | TContinue -> mkn "TContinue"
  | TThrow  e -> mkn "TThrow" ~lst:[wrp comap_texpr e]
  | TCast (e,m) -> mkn "TCast" ~s:["?????"] 

and comap_texpr e =
  let e1 = mkn ~lst:[wrp comap_texpr_expr e.eexpr] "eexpr" in
  let e2 = mkn ~lst:[wrp comap_pos e.epos] "epos" in
  let e3 = mkn ~lst:[wrp comap_t e.etype] "etype" in
  mkn "texpr" ~lst:[e1; e2; e3]
and comap_tclass_kind = function
  | KNormal            -> mkn "KNormal"
  | KTypeParameter     -> mkn "KTypeParameter"
  | KGeneric           -> mkn "KGeneric"
  | KExtension (tc,tp) -> mkn ~lst:[wrp comap_tclass tc; wrp comap_tparams tp] "KExtension"
  | KGenericInstance (tc,tp) -> mkn ~lst:[wrp comap_tclass tc; wrp comap_tparams tp] "KGenericInstance"
  | KConstant tk             -> mkn ~lst:[mkn (Show.show<Type.tconstant> tk)] "KConstant"

and comap_tclass_field cf  =  
  let pub = mkn ~lst:[bool_node cf.cf_public] "cf_public" in
  let kin = wrp comap_field_kind cf.cf_kind in
  let met = wrp comap_metadata (cf.cf_meta ()) in
  let exp = opt_node (wrp comap_texpr) cf.cf_expr in
  mkn ~s:[cf.cf_name] ~lst:[pub; kin; met; exp] "tclass_field"
and comap_tclass ?force:(f=false) c =   
  let pa = wrp comap_path c.cl_path in
  let po = comap_pos c.cl_pos in
  let full () = (
    loaded_paths := c.cl_path :: !loaded_paths;
    let ord_s = mkn "cl_ordered_statics" ~lst:(List.map (wrp comap_tclass_field) c.cl_ordered_statics) in
    let ord_f = mkn "cl_ordered_fields"  ~lst:(List.map (wrp comap_tclass_field) c.cl_ordered_fields) in
    let over  = mkn "cl_overrides"       ~s:c.cl_overrides in
    let init  = mkn "cl_init"            ~lst:[wrp (opt_node comap_texpr) c.cl_init ] in
    let constr = mkn "cl_constructor"    ~lst:[wrp (opt_node comap_tclass_field) c.cl_constructor ] in
    mkn "tclass" ~lst:[pa; po; ord_s; ord_f; over; init; constr]
  ) in
  let brief () = mkn "tclass" ~lst:[po;pa] ~s:["this class is in the current module"] in
  if f then full () else
  if List.mem c.cl_path !future_loaded_types then brief() else
  if List.mem c.cl_path !loaded_paths then  brief () else 
  full ()
  

and comap_tenum_field e = 
  let pos = comap_pos e.ef_pos in
  let typ = mkn "ef_type" ~lst:[wrp comap_t e.ef_type] in
  mkn "tenum_field" ~s:[e.ef_name; string_of_int e.ef_index] ~lst:[pos;typ]
and comap_tenum ?force:(f=false) e =   
  let pa = wrp comap_path e.e_path in
  let pos = comap_pos e.e_pos in
  let brief () = mkn "tenum" ~lst:[pa;pos] ~s:["this enum is in the current module"] in
  let full () = (
    loaded_paths := e.e_path :: !loaded_paths; 
    let types = List.map (fun (s,t) -> mkn "e_types" ~s:[s] ~lst:[wrp comap_t t]) e.e_types in
    let constr= comap_PMap mkn comap_t "e_constr" e.e_constrs in
    mkn "tenum" ~lst:(pa :: pos :: types @ constr)
  ) in
  if f then full () else
  if List.mem e.e_path !future_loaded_types then brief() else
  if List.mem e.e_path !loaded_paths then  brief () else 
  full ()

and comap_tdef ?force:(f=false) td =   
  let pa = wrp comap_path td.t_path in
  let po = wrp comap_pos td.t_pos in
  let brief () = mkn "tdef" ~lst:[pa;po] ~s:["this definition is in the current module"] in
  let full ()  = (
    loaded_paths := td.t_path :: !loaded_paths; 
    let me = wrp comap_metadata (td.t_meta ()) in
    let types = List.map (fun (s,t) -> mkn "t_type" ~s:[s] ~lst:[wrp comap_t t]) td.t_types in
    mkn "tdef" ~lst:(pa :: po :: types) 
  ) in
  if f then full () else
  if List.mem td.t_path !future_loaded_types then brief() else
  if List.mem td.t_path !loaded_paths then  brief () else 
  full ()
and comap_module_type  = function
  | TClassDecl cd -> mkn ~lst:[wrp comap_tclass cd] "TClassDecl"
  | TEnumDecl  ed -> mkn ~lst:[wrp comap_tenum ed] "TEnumDecl"
  | TTypeDecl  td -> mkn ~lst:[wrp comap_tdef td] "TTypeDecl"
and comap_metadata ms = 
  mkn ~lst:(List.map (fun (s,es) -> mkn s ~lst:(List.map (wrp comap_texpr) es)) ms) "metadata"

let comap_module_def  md = 
  loaded_paths := [];

  let extr_path = function 
    | TClassDecl cd -> cd.cl_path
    | TEnumDecl  ed -> ed.e_path
    | TTypeDecl  td -> td.t_path in
  future_loaded_types := List.map (extr_path) md.mtypes;

  let func = function
  | TClassDecl cd -> mkn ~lst:[wrp (comap_tclass ~force:true) cd] "TClassDecl"
  | TEnumDecl  ed -> mkn ~lst:[wrp (comap_tenum  ~force:true) ed] "TEnumDecl"
  | TTypeDecl  td -> mkn ~lst:[wrp (comap_tdef   ~force:true) td] "TTypeDecl" in

  let xml_path  = comap_path md.mpath in
  let xml_mtypes = mkn "mtypes" ~lst:(List.map func md.mtypes) in
  mkn "module_def" ~lst:[xml_path; xml_mtypes]

(*  Element ("module_def",[], [Element("mpath",[], [path_xml]); 
			     Element("mtypes",[],(List.map show_module_type md.mtypes))] ); *)

let to_ch node ch = 
  Printf.fprintf ch "%s\n" "<mindmap escape=\"false\">  <metadata>    <id><![CDATA[65226]]></id>    <name><![CDATA[bestOS]]></name>    <owner>      <id><![CDATA[22777]]></id>      <name><![CDATA[Kakadu Hafanana]]></name>      <email><![CDATA[KakaduHafanana@yandex.ru]]></email>    </owner>    <access><![CDATA[Shared]]></access>    <source><![CDATA[Server]]></source>    <collaborators><![CDATA[borech@tercom.ru]]></collaborators>    <watchers><![CDATA[]]></watchers>    <isTemplate><![CDATA[false]]></isTemplate>    <params/>  </metadata>   <presentation/>"; 
  Printf.fprintf ch "%s" (to_string node);
  Printf.fprintf ch "\n</mindmap>\n"

