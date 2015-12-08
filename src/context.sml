functor SymbolOrdered (S : SYMBOL) : ORDERED =
struct
  open S.Eq
  open S
end

functor Context (S : SYMBOL) : DICT =
  SplayDict
    (structure Key = SymbolOrdered (S))


structure VCtx = Context (Variable)
structure SCtx = Context (Symbol)


