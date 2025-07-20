%name AbtLex;

%defs (
  structure T = AbtTokens
  type lex_result = T.token 
  fun eof () = T.EOF
  val strResult : string list ref = ref []
  fun addStr s = strResult := s :: !strResult
);

%states INITIAL;
%states STRING;

%let namechar = [a-zA-Z0-9_@'];
%let opname = [A-Z]({namechar})*;
%let varname = [_a-z]({namechar})*;

<INITIAL> {opname} => ( T.Name yytext );
<INITIAL> {varname} => ( T.Var yytext );
<INITIAL> "(" => ( T.LParen );
<INITIAL> ")" => ( T.RParen );
<INITIAL> "," => ( T.Comma );
<INITIAL> "." => ( T.Dot );
<INITIAL> "[" => ( T.LSqBrac );
<INITIAL> "]" => ( T.RSqBrac );
<INITIAL> "None" => ( T.None );
<INITIAL> \" => ( YYBEGIN STRING ; strResult := [] ; continue () );
<INITIAL> " " | \n | \t => ( skip () );

<STRING> "\\\"" => ( addStr "\"" ; continue () );
<STRING> \" => ( YYBEGIN INITIAL ; T.String (String.concat (List.rev (!strResult))) );
<STRING> . => ( addStr yytext ; continue () );

