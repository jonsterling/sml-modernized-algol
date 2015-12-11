structure Metavariable = Symbol ()
structure Variable = Symbol ()
structure Symbol = Symbol ()

structure Metacontext =
  Metacontext
    (structure Metavariable = Metavariable
     structure Valence = Operator.Arity.Valence.Eq)

structure Abt =
  AbtUtil
    (Abt
      (structure Operator = Operator
       structure Metavariable = Metavariable
       structure Metacontext = Metacontext
       structure Variable = Variable
       structure Symbol = Symbol))

structure ShowAbt = PlainShowAbt (Abt)
