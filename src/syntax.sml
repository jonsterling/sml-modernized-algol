structure Metavariable = Symbol ()
structure Variable = Symbol ()
structure Symbol = Symbol ()

structure Abt =
    Abt
     (structure Operator = Operator
      structure Metavariable = Metavariable
      structure Variable = Variable
      structure Symbol = Symbol)

structure Ast =
  Ast
    (structure Operator = Operator
     structure Metavariable = Metavariable)

structure ShowAbt = PlainShowAbt (Abt)
