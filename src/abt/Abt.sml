
structure Abt = struct
  
  type var = string

  datatype abt =
    Op of string * param list
  and param =
    String of string
  | Abt of abt
  | Bind of var * abt

  (* fun comma_join slist = *)
  (*   let *)
  (*     fun comma slist = *)
  (*       case slist of *)
  (*         nil => nil *)
  (*       | [one] => [one] *)
  (*       | h :: t => h :: "," *)
    
  (* fun printabt (t : abt) = *)
  (*   case t of *)
  (*     Op (name, params) => *)
  (*       String.concat *)
  (*       [name , "(" , List.map printparams params , ")"] *)
end
