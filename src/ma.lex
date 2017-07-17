type pos = string -> Coord.t
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
type arg = string

val pos = ref Coord.init
val eof = fn (fname : string) => Tokens.EOF (!pos, !pos)

fun incPos n = pos := (Coord.addchar n o (!pos))

fun posTuple n =
  let
    val l  = !pos
    val () = incPos n
    val r  = !pos
  in
    (l, r)
  end

fun posTupleWith n x =
  let
    val (l, r) = posTuple n
  in
    (x, l, r)
  end

%%
%arg (fileName : string);
%header (functor MALexFun (structure Tokens : MA_TOKENS));
alpha = [A-Za-z];
digit = [0-9];
any   = [@a-zA-Z0-9];
whitespace = [\ \t];
%%

\n                => (pos := (Coord.nextline o (!pos)); continue ());
{whitespace}+     => (incPos (size yytext); continue ());

"cmd"             => (Tokens.CMD    (posTuple (size yytext)));
"ret"             => (Tokens.RET    (posTuple (size yytext)));
"bnd"             => (Tokens.BIND   (posTuple (size yytext)));
"dcl"             => (Tokens.DCL    (posTuple (size yytext)));
";"               => (Tokens.SCOLON (posTuple (size yytext)));
"<-"              => (Tokens.LARROW (posTuple (size yytext)));
":="              => (Tokens.GETS   (posTuple (size yytext)));
"in"              => (Tokens.IN     (posTuple (size yytext)));
"@"               => (Tokens.AT     (posTuple (size yytext)));

{alpha}{any}*     => (Tokens.IDENT  (posTupleWith (size yytext) yytext));
