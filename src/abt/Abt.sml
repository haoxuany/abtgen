(* Theory of single sorted Abstract Binding Trees per Harper [2015], specifically *)
(* for both parsing and printing. It comes with a few modifications: *)
(* - Strings are built into ABTs (aka theory of symbols/labels, but can be used *)
(*   for other things in client code). *)
(* - Operators can have arbitrary arity through the List and Option param *)
(*   constructs. This is useful for dealing with n-ary sum eliminations. *)
(* This is really more of an intermediate layer for representation, not *)
(* even the most efficient representation. It specifically handles the need
* to not need to handwrite parsing every single time. *)
(* An example predicate P of sort exp with arity exp.exp * exp list * exp option will
* be printed and parsed with representation (assuming Q has arity unit)
* "P(x.Q(), [Q(), Q()], Q())" or "P(x.Q(), [Q(), Q()], None)" *)

signature ABT = sig 
  type var

  datatype abt =
    Op of string * param list
  | Var of var
  and param =
    String of string
  | Abt of abt
  | Bind of var * abt
  | List of param list
  | Option of param option

  val print_abt : TextIO.outstream -> abt -> unit
end

structure Abt : ABT = struct
  
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

  local
    open PrettyPrint
  in
    
    fun printabt (t : abt) stream =
      case t of
        Op (name, params) =>
        (
          openBox stream Consistent 2;
          print stream name;
          print stream "(";
          printparams params stream;
          print stream ")";
          closeBox stream
        )
      | Var v => print stream v
      and printparams (l : param list) stream =
        case l of
           nil => ()
        | [one] =>
            ( printparam one stream;
              break stream 1
            )
        | h :: tail =>
            ( printparam h stream;
              print stream ",";
              break stream 1
            )
      and printparam (t : param) stream =
        case t of
          String s =>
            ( print stream "\"";
              print stream (String.toString s);
              print stream "\""
            )
        | Abt abt =>
            printabt abt stream
        | Bind (v , abt) =>
            ( print stream v;
              print stream ".";
              printabt abt stream
            )
        | List params =>
            ( openBox stream Consistent 2;
              print stream "[";
              printparams params stream;
              print stream "]";
              closeBox stream
            )
        | Option param =>
            (case param of
                NONE => print stream "None"
              | SOME p => printparam p stream
            )

    val print_abt =
      fn stream => fn abt =>
        printabt abt (makeStream stream 80)
  end

end
