functor Context (S : SYMBOL) : DICT = SplayDict (structure Key = S)


structure VCtx = Context (Variable)
structure SCtx = Context (Symbol)


