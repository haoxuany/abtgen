structure AbtTokens =
  struct
    datatype token
      = Name of string
      | LParen
      | RParen
      | Comma
      | Var of string
      | String of string
      | Dot
      | LSqBrac
      | RSqBrac
      | None
      | EOF
    val allToks = [
            LParen, RParen, Comma, Dot, LSqBrac, RSqBrac, None, EOF
           ]
    fun toString tok =
(case (tok)
 of (Name(_)) => "Name"
  | (LParen) => "("
  | (RParen) => ")"
  | (Comma) => ","
  | (Var(_)) => "Var"
  | (String(_)) => "String"
  | (Dot) => "."
  | (LSqBrac) => "["
  | (RSqBrac) => "]"
  | (None) => "None"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (Name(_)) => false
  | (LParen) => false
  | (RParen) => false
  | (Comma) => false
  | (Var(_)) => false
  | (String(_)) => false
  | (Dot) => false
  | (LSqBrac) => false
  | (RSqBrac) => false
  | (None) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* AbtTokens *)

functor AbtParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
AbtTokens
    structure UserCode =
      struct

fun operator_PROD_1_ACT (paramlist, LParen, RParen, Name, paramlist_SPAN : (Lex.pos * Lex.pos), LParen_SPAN : (Lex.pos * Lex.pos), RParen_SPAN : (Lex.pos * Lex.pos), Name_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AbtRep.Op ( Name , paramlist ) )
fun operator_PROD_2_ACT (Name, Name_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AbtRep.Op ( Name , [] ) )
fun operator_PROD_3_ACT (Var, Var_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AbtRep.Var Var )
fun paramlist_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( [] )
fun paramlist_PROD_2_ACT (head, SR2, tail, head_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), tail_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( head :: tail )
fun param_PROD_1_ACT (s, s_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AbtRep.String s )
fun param_PROD_2_ACT (operator, Dot, Var, operator_SPAN : (Lex.pos * Lex.pos), Dot_SPAN : (Lex.pos * Lex.pos), Var_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AbtRep.Bind ( Var , operator ) )
fun param_PROD_3_ACT (operator, operator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AbtRep.Abt operator )
fun param_PROD_4_ACT (paramlist, LSqBrac, RSqBrac, paramlist_SPAN : (Lex.pos * Lex.pos), LSqBrac_SPAN : (Lex.pos * Lex.pos), RSqBrac_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AbtRep.List paramlist )
fun param_PROD_5_ACT (None, None_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( AbtRep.Option NONE )
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchName strm = (case (lex(strm))
 of (Tok.Name(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchLParen strm = (case (lex(strm))
 of (Tok.LParen, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRParen strm = (case (lex(strm))
 of (Tok.RParen, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchComma strm = (case (lex(strm))
 of (Tok.Comma, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchVar strm = (case (lex(strm))
 of (Tok.Var(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchString strm = (case (lex(strm))
 of (Tok.String(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchDot strm = (case (lex(strm))
 of (Tok.Dot, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLSqBrac strm = (case (lex(strm))
 of (Tok.LSqBrac, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRSqBrac strm = (case (lex(strm))
 of (Tok.RSqBrac, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNone strm = (case (lex(strm))
 of (Tok.None, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (operator_NT) = 
let
fun operator_NT (strm) = let
      fun operator_PROD_1 (strm) = let
            val (Name_RES, Name_SPAN, strm') = matchName(strm)
            val (LParen_RES, LParen_SPAN, strm') = matchLParen(strm')
            val (paramlist_RES, paramlist_SPAN, strm') = paramlist_NT(strm')
            val (RParen_RES, RParen_SPAN, strm') = matchRParen(strm')
            val FULL_SPAN = (#1(Name_SPAN), #2(RParen_SPAN))
            in
              (UserCode.operator_PROD_1_ACT (paramlist_RES, LParen_RES, RParen_RES, Name_RES, paramlist_SPAN : (Lex.pos * Lex.pos), LParen_SPAN : (Lex.pos * Lex.pos), RParen_SPAN : (Lex.pos * Lex.pos), Name_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun operator_PROD_2 (strm) = let
            val (Name_RES, Name_SPAN, strm') = matchName(strm)
            val FULL_SPAN = (#1(Name_SPAN), #2(Name_SPAN))
            in
              (UserCode.operator_PROD_2_ACT (Name_RES, Name_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun operator_PROD_3 (strm) = let
            val (Var_RES, Var_SPAN, strm') = matchVar(strm)
            val FULL_SPAN = (#1(Var_SPAN), #2(Var_SPAN))
            in
              (UserCode.operator_PROD_3_ACT (Var_RES, Var_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.Var(_), _, strm') => operator_PROD_3(strm)
          | (Tok.Name(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.LParen, _, strm') => operator_PROD_1(strm)
                | (Tok.RParen, _, strm') => operator_PROD_2(strm)
                | (Tok.Comma, _, strm') => operator_PROD_2(strm)
                | (Tok.RSqBrac, _, strm') => operator_PROD_2(strm)
                | (Tok.EOF, _, strm') => operator_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
and paramlist_NT (strm) = let
      fun paramlist_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.paramlist_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun paramlist_PROD_2 (strm) = let
            val (head_RES, head_SPAN, strm') = param_NT(strm)
            fun paramlist_PROD_2_SUBRULE_1_NT (strm) = let
                  val (Comma_RES, Comma_SPAN, strm') = matchComma(strm)
                  val (param_RES, param_SPAN, strm') = param_NT(strm')
                  val FULL_SPAN = (#1(Comma_SPAN), #2(param_SPAN))
                  in
                    ((param_RES), FULL_SPAN, strm')
                  end
            fun paramlist_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.Comma, _, strm') =>
                        (case (lex(strm'))
                         of (Tok.Name(_), _, strm') => true
                          | (Tok.Var(_), _, strm') => true
                          | (Tok.String(_), _, strm') => true
                          | (Tok.LSqBrac, _, strm') => true
                          | (Tok.None, _, strm') => true
                          | _ => false
                        (* end case *))
                    | _ => false
                  (* end case *))
            val (tail_RES, tail_SPAN, strm') = EBNF.closure(paramlist_PROD_2_SUBRULE_1_PRED, paramlist_PROD_2_SUBRULE_1_NT, strm')
            fun paramlist_PROD_2_SUBRULE_2_NT (strm) = let
                  val (Comma_RES, Comma_SPAN, strm') = matchComma(strm)
                  val FULL_SPAN = (#1(Comma_SPAN), #2(Comma_SPAN))
                  in
                    ((), FULL_SPAN, strm')
                  end
            fun paramlist_PROD_2_SUBRULE_2_PRED (strm) = (case (lex(strm))
                   of (Tok.Comma, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR2_RES, SR2_SPAN, strm') = EBNF.optional(paramlist_PROD_2_SUBRULE_2_PRED, paramlist_PROD_2_SUBRULE_2_NT, strm')
            val FULL_SPAN = (#1(head_SPAN), #2(SR2_SPAN))
            in
              (UserCode.paramlist_PROD_2_ACT (head_RES, SR2_RES, tail_RES, head_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), tail_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.Name(_), _, strm') => paramlist_PROD_2(strm)
          | (Tok.Var(_), _, strm') => paramlist_PROD_2(strm)
          | (Tok.String(_), _, strm') => paramlist_PROD_2(strm)
          | (Tok.LSqBrac, _, strm') => paramlist_PROD_2(strm)
          | (Tok.None, _, strm') => paramlist_PROD_2(strm)
          | (Tok.RParen, _, strm') => paramlist_PROD_1(strm)
          | (Tok.RSqBrac, _, strm') => paramlist_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and param_NT (strm) = let
      fun param_PROD_1 (strm) = let
            val (s_RES, s_SPAN, strm') = matchString(strm)
            val FULL_SPAN = (#1(s_SPAN), #2(s_SPAN))
            in
              (UserCode.param_PROD_1_ACT (s_RES, s_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun param_PROD_2 (strm) = let
            val (Var_RES, Var_SPAN, strm') = matchVar(strm)
            val (Dot_RES, Dot_SPAN, strm') = matchDot(strm')
            val (operator_RES, operator_SPAN, strm') = operator_NT(strm')
            val FULL_SPAN = (#1(Var_SPAN), #2(operator_SPAN))
            in
              (UserCode.param_PROD_2_ACT (operator_RES, Dot_RES, Var_RES, operator_SPAN : (Lex.pos * Lex.pos), Dot_SPAN : (Lex.pos * Lex.pos), Var_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun param_PROD_3 (strm) = let
            val (operator_RES, operator_SPAN, strm') = operator_NT(strm)
            val FULL_SPAN = (#1(operator_SPAN), #2(operator_SPAN))
            in
              (UserCode.param_PROD_3_ACT (operator_RES, operator_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun param_PROD_4 (strm) = let
            val (LSqBrac_RES, LSqBrac_SPAN, strm') = matchLSqBrac(strm)
            val (paramlist_RES, paramlist_SPAN, strm') = paramlist_NT(strm')
            val (RSqBrac_RES, RSqBrac_SPAN, strm') = matchRSqBrac(strm')
            val FULL_SPAN = (#1(LSqBrac_SPAN), #2(RSqBrac_SPAN))
            in
              (UserCode.param_PROD_4_ACT (paramlist_RES, LSqBrac_RES, RSqBrac_RES, paramlist_SPAN : (Lex.pos * Lex.pos), LSqBrac_SPAN : (Lex.pos * Lex.pos), RSqBrac_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun param_PROD_5 (strm) = let
            val (None_RES, None_SPAN, strm') = matchNone(strm)
            val FULL_SPAN = (#1(None_SPAN), #2(None_SPAN))
            in
              (UserCode.param_PROD_5_ACT (None_RES, None_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.None, _, strm') => param_PROD_5(strm)
          | (Tok.Name(_), _, strm') => param_PROD_3(strm)
          | (Tok.String(_), _, strm') => param_PROD_1(strm)
          | (Tok.Var(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.Dot, _, strm') => param_PROD_2(strm)
                | (Tok.RParen, _, strm') => param_PROD_3(strm)
                | (Tok.Comma, _, strm') => param_PROD_3(strm)
                | (Tok.RSqBrac, _, strm') => param_PROD_3(strm)
                | _ => fail()
              (* end case *))
          | (Tok.LSqBrac, _, strm') => param_PROD_4(strm)
          | _ => fail()
        (* end case *))
      end
in
  (operator_NT)
end
val operator_NT =  fn s => unwrap (Err.launch (eh, lexFn, operator_NT , true) s)

in (operator_NT) end
  in
fun parse lexFn  s = let val (operator_NT) = mk lexFn in operator_NT s end

  end

end
