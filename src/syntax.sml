structure Metavariable = Symbol ()
structure Variable = Symbol ()
structure Symbol = Symbol ()

structure Metacontext =
  Metacontext
    (structure Metavariable = Metavariable
     type valence = Operator.Arity.Valence.t)

structure Abt =
  AbtUtil
    (Abt
      (structure Operator = Operator
       structure Metavariable = Metavariable
       structure Metacontext = Metacontext
       structure Variable = Variable
       structure Symbol = Symbol))

structure ShowAbt = PlainShowAbt (Abt)
