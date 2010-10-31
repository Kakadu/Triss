module PMap = struct 
  include PMap

  module Show_6tIqeLG1rvuaGYGyldGGIUC6Cs8caje4
      (V_a : Show.Show) (V_b : Show.Show) = struct
        open Show
        
        module rec Show_t : Show.Show with type a = (V_a.a, V_b.a) PMap.t =
                     Show.Defaults (Defaults  (struct
                             type a = (V_a.a, V_b.a) PMap.t
                             
                             let format (formatter:Format.formatter) (mapa:a) : unit =			       
(*			       let lst = ref [] in
			       PMap.iter (fun a b -> lst := (V_a.show a,V_b.show b) :: !lst) mapa;
			       let foo (a,b) = String.concat "" ["(";a;",";b;")"] in
			       let s = String.concat ";" (List.map foo !lst) in
			       Format.fprintf formatter "%s" s *)
			       Format.fprintf formatter "<PMap>"
                           end))
          
   end
      
    module Show_t (V_a : Show.Show) (V_b : Show.Show) =
      struct
        module P = Show_6tIqeLG1rvuaGYGyldGGIUC6Cs8caje4(V_a)(V_b)          
        include P.Show_t          
      end
end

type d = (int,int) PMap.t deriving (Show)
let m = PMap.create compare in
ignore (Show.show<d> m);
  
