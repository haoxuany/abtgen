structure AbtRep = struct
  
  type var = string

  datatype abt =
    Op of string * param list
  | Var of var
  and param =
    String of string
  | Abt of abt
  | Bind of var * abt
  | List of param list
  | Option of param option

end
