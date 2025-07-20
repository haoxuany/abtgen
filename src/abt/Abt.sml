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

  val print : TextIO.outstream -> abt -> unit
  
  type stream
  exception ParseError of string

  val streamifyInstream : TextIO.instream -> stream 
  val streamifyString : (unit -> string) -> stream

  val parse : stream -> (abt * stream)
  val parseString : (unit -> string) -> abt 
  val parseInstream : TextIO.instream -> abt
end

structure Abt : ABT = struct 
  open AbtRep

  local
    open PrettyPrint
  in
    
    fun printabt (t : abt) stream =
      case t of
        Op (name, params) =>
        (
          openBox stream Consistent 2;
          print stream name;
          (
            case params of 
              nil => ()
            | _ => (
              print stream "(";
              printparams params stream;
              print stream ")"
            )
          );
          closeBox stream
        )
      | Var v => print stream v
      and printparams (l : param list) stream =
        case l of
           nil => ()
        | [one] =>
            ( printparam one stream )
        | h :: tail =>
            ( printparam h stream;
              print stream ",";
              break stream 1;
              printparams tail stream
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

    val print =
      fn stream => fn abt =>
      let 
        val stream = makeStream stream 80
      in
        (printabt abt stream; flush stream)
      end

  end

  structure Parse = AbtParseFn(AbtLex)

  type stream = AbtLex.strm

  val streamifyInstream = AbtLex.streamifyInstream
  val streamifyString = AbtLex.streamify

  exception ParseError of string

  local open AbtTokens in
    fun quote s = String.concat ["\"" , s , "\""]
    fun tokenToString tok =
      case tok of
        Name s => quote s 
      | Var s => quote s 
      | String s => quote s 
      | _ => AbtTokens.toString tok
  end


  fun parse strm =
    let 
      val sm = AntlrStreamPos.mkSourcemap ()
      val lex = AbtLex.lex sm 
      val (result , strm , errors) = Parse.parse lex strm 
    in 
      case errors of
        nil => (Option.valOf result , strm)
      | _ =>
          raise ParseError
          (String.concatWith "\n"
          (List.map (AntlrRepair.repairToString tokenToString sm)
          errors))
    end

  fun parseString s = #1 (parse (streamifyString s))
  fun parseInstream s = #1 (parse (streamifyInstream s))
end
