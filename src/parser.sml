structure MALrVals =
  MALrValsFun(structure Token = LrParser.Token)

structure MALex =
  MALexFun(structure Tokens = MALrVals.Tokens)

structure MAParser =
  JoinWithArg
    (structure LrParser = LrParser
     structure ParserData = MALrVals.ParserData
     structure Lex = MALex)
