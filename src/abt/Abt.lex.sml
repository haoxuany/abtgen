structure AbtLex  = struct

    datatype yystart_state = 
STRING | INITIAL
    local

    structure UserDeclarations = 
      struct

 
  structure T = AbtTokens
  type lex_result = T.token 
  fun eof () = T.EOF
  val strResult : string list ref = ref []
  fun addStr s = strResult := s :: !strResult

      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case ULexBuffer.getu strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.Name yytext 
      end
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   T.Var yytext 
      end
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LParen )
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RParen )
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;   T.Comma )
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;   T.Dot )
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;   T.LSqBrac )
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;   T.RSqBrac )
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;   T.None )
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
        YYBEGIN STRING ; strResult := [] ; continue () )
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;   skip () )
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
        addStr "\"" ; continue () )
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
        YYBEGIN INITIAL ; T.String (String.concat (List.rev (!strResult))) )
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;   addStr yytext ; continue () 
      end
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx40
              then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < 0wx40
              then if inp = 0wx28
                  then yyAction1(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyAction1(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction1(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction1(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx40
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx40
              then if inp = 0wx28
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx40
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx40
              then if inp = 0wx28
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx28
                  then if inp = 0wx27
                      then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx27
                      then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp <= 0wx3F
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ19(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx27
                      then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp <= 0wx3F
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ18(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction0(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx27
                      then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp <= 0wx3F
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2E
              then yyQ11(strm', lastMatch)
            else if inp < 0wx2E
              then if inp = 0wx23
                  then if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yystuck(lastMatch)
                else if inp < 0wx23
                  then if inp = 0wx20
                      then yyQ6(strm', lastMatch)
                    else if inp < 0wx20
                      then if inp = 0wx9
                          then yyQ6(strm', lastMatch)
                        else if inp < 0wx9
                          then if ULexBuffer.eof(!(yystrm))
                              then let
                                val yycolno = ref(yygetcolNo(!(yystrm)))
                                val yylineno = ref(yygetlineNo(!(yystrm)))
                                in
                                  (case (!(yyss))
                                   of _ => (UserDeclarations.eof())
                                  (* end case *))
                                end
                              else yystuck(lastMatch)
                        else if inp <= 0wxA
                          then yyQ6(strm', lastMatch)
                        else if ULexBuffer.eof(!(yystrm))
                          then let
                            val yycolno = ref(yygetcolNo(!(yystrm)))
                            val yylineno = ref(yygetlineNo(!(yystrm)))
                            in
                              (case (!(yyss))
                               of _ => (UserDeclarations.eof())
                              (* end case *))
                            end
                          else yystuck(lastMatch)
                    else if inp = 0wx21
                      then if ULexBuffer.eof(!(yystrm))
                          then let
                            val yycolno = ref(yygetcolNo(!(yystrm)))
                            val yylineno = ref(yygetlineNo(!(yystrm)))
                            in
                              (case (!(yyss))
                               of _ => (UserDeclarations.eof())
                              (* end case *))
                            end
                          else yystuck(lastMatch)
                      else yyQ7(strm', lastMatch)
                else if inp = 0wx2A
                  then if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yystuck(lastMatch)
                else if inp < 0wx2A
                  then if inp = 0wx28
                      then yyQ8(strm', lastMatch)
                    else if inp = 0wx29
                      then yyQ9(strm', lastMatch)
                    else if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yystuck(lastMatch)
                else if inp = 0wx2C
                  then yyQ10(strm', lastMatch)
                else if ULexBuffer.eof(!(yystrm))
                  then let
                    val yycolno = ref(yygetcolNo(!(yystrm)))
                    val yylineno = ref(yygetlineNo(!(yystrm)))
                    in
                      (case (!(yyss))
                       of _ => (UserDeclarations.eof())
                      (* end case *))
                    end
                  else yystuck(lastMatch)
            else if inp = 0wx5D
              then yyQ15(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx4F
                  then yyQ12(strm', lastMatch)
                else if inp < 0wx4F
                  then if inp = 0wx41
                      then yyQ12(strm', lastMatch)
                    else if inp < 0wx41
                      then if ULexBuffer.eof(!(yystrm))
                          then let
                            val yycolno = ref(yygetcolNo(!(yystrm)))
                            val yylineno = ref(yygetlineNo(!(yystrm)))
                            in
                              (case (!(yyss))
                               of _ => (UserDeclarations.eof())
                              (* end case *))
                            end
                          else yystuck(lastMatch)
                    else if inp = 0wx4E
                      then yyQ13(strm', lastMatch)
                      else yyQ12(strm', lastMatch)
                else if inp = 0wx5B
                  then yyQ14(strm', lastMatch)
                else if inp = 0wx5C
                  then if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yystuck(lastMatch)
                  else yyQ12(strm', lastMatch)
            else if inp = 0wx60
              then if ULexBuffer.eof(!(yystrm))
                  then let
                    val yycolno = ref(yygetcolNo(!(yystrm)))
                    val yylineno = ref(yygetlineNo(!(yystrm)))
                    in
                      (case (!(yyss))
                       of _ => (UserDeclarations.eof())
                      (* end case *))
                    end
                  else yystuck(lastMatch)
            else if inp < 0wx60
              then if inp = 0wx5E
                  then if ULexBuffer.eof(!(yystrm))
                      then let
                        val yycolno = ref(yygetcolNo(!(yystrm)))
                        val yylineno = ref(yygetlineNo(!(yystrm)))
                        in
                          (case (!(yyss))
                           of _ => (UserDeclarations.eof())
                          (* end case *))
                        end
                      else yystuck(lastMatch)
                  else yyQ16(strm', lastMatch)
            else if inp <= 0wx7A
              then yyQ16(strm', lastMatch)
            else if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyQ5(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of _ => (UserDeclarations.eof())
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx23
              then yyQ2(strm', lastMatch)
            else if inp < 0wx23
              then if inp = 0wx22
                  then yyQ3(strm', lastMatch)
                  else yyQ2(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ4(strm', lastMatch)
              else yyQ2(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of STRING => yyQ0(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ1(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()-1), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end

