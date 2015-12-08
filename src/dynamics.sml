structure Dynamics : DYNAMICS =
struct
  open Abt
  type exp = abt
  type cmd = abt

  type memory = exp SCtx.dict
  datatype state = || of cmd * memory
  infix 2 ||

  datatype 'a step =
      STEP of 'a
    | FINAL

  structure O = OperatorData
  structure S = SortData
  structure MC = Metacontext

  infix $ \

  fun step E =
    let
      val (E', S.EXP) = Abt.infer E
    in
      case E' of
           O.NUM n $ _ => FINAL
         | O.CMD $ _ => FINAL
         | _ => raise Match
    end

  fun exp E = Abt.check MC.empty (E, S.EXP)
  fun cmd M = Abt.check MC.empty (M, S.CMD)
  val emp = ([], [])

  fun $$ (theta, Es) = exp (theta $ Es)
  fun $~ (theta, Es) = cmd (theta $ Es)
  infix 3 $$ $~

  fun @@ (f, x) = f x
  infix 0 @@

  fun trans (M || mu) =
    let
      val (M', S.CMD) = Abt.infer M
    in
      case M' of
           O.RET $ [_ \ E] =>
             (case step E of
                   STEP E' => STEP @@ O.RET $~ [emp \ E'] || mu
                 | FINAL => FINAL)
         | O.BND $ [_ \ E, (_, [x]) \ M] =>
             STEP @@
             (case step E of
                   STEP E' => O.BND $~ [emp \ E', ([], [x]) \ M] || mu
                 | FINAL =>
                     (case #1 (Abt.infer E) of
                           O.CMD $ [_ \ M'] =>
                             (case trans (M' || mu) of
                                   STEP (M'' || mu') =>
                                     O.BND $~ [emp \ O.CMD $$ [emp \ M''], ([], [x]) \ M'] || mu'
                                 | FINAL =>
                                     let
                                       val (RET $ [_ \ E], _) = Abt.infer M'
                                     in
                                       subst (E, x) M || mu
                                     end)
                         | _ => raise Match))
         | O.GET a $ [] => STEP @@ O.RET $~ [emp \ SCtx.lookup mu a] || mu
         | O.SET a $ [_ \ E] =>
             STEP @@
             (case step E of
                   STEP E' =>
                     O.SET a $~ [emp \ E'] || mu
                 | FINAL =>
                     O.RET $~ [emp \ E] || SCtx.insert mu a E)
         | O.DCL $ [_ \ E, ([a], []) \ M] =>
             STEP @@
             (case step E of
                   STEP E' => O.DCL $~ [emp \ E', ([a], []) \ M] || mu
                 | FINAL =>
                     case trans (M || SCtx.insert mu a E) of
                          STEP (M' || mu') =>
                            let
                              val E' = SCtx.lookup mu' a
                            in
                              O.DCL $~ [emp \ E', ([a], []) \ M'] || SCtx.remove mu' a
                            end
                        | FINAL =>
                            let
                              val (O.RET $ [_ \ E'], _) = Abt.infer M
                            in
                              M || mu
                            end)
         | _ => raise Match
    end

end
