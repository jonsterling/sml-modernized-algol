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

  infix >>=
  infix <#>

  fun op>>= (FINAL, f) = FINAL
    | op>>= (STEP x, f) = f x

  fun op<#> (x, f) = x >>= (STEP o f)

  structure O = OperatorData
  structure S = SortData
  structure MC = Abt.Metavariable.Ctx

  infix 4 $ $$
  infix 3 \

  fun step E =
    let
      val (E', S.EXP) = Abt.infer E
    in
      case E' of
           O.NUM n $ _ => FINAL
         | O.CMD $ _ => FINAL
         | _ => raise Match
    end

  val emp = ([], [])

  fun @@ (f, x) = f x
  infix 0 @@

  fun trans (M || mu) =
    let
      val (M', S.CMD) = Abt.infer M
    in
      case M' of
           O.RET $ [_ \ E] => step E <#> (fn E' => O.RET $$ [emp \ E'] || mu)
         | O.BND $ [_ \ E, (_, [x]) \ M] => STEP @@ stepBind mu E x M
         | O.GET a $ [] => STEP @@ O.RET $$ [emp \ SCtx.lookup mu a] || mu
         | O.SET a $ [_ \ E] =>
             STEP
             (case step E of
                   STEP E' =>
                     O.SET a $$ [emp \ E'] || mu
                 | FINAL =>
                     O.RET $$ [emp \ E] || SCtx.insert mu a E)
         | O.DCL $ [_ \ E, ([a], []) \ M] =>
             STEP @@ stepDecl mu E a M
         | _ => raise Match
    end
  and stepBind mu E x M =
    case step E of
          STEP E' => O.BND $$ [emp \ E', ([], [x]) \ M] || mu
        | FINAL =>
            (case Abt.out E of
                  O.CMD $ [_ \ M'] =>
                    (case trans (M' || mu) of
                          STEP (M'' || mu') =>
                            O.BND $$ [emp \ O.CMD $$ [emp \ M''], ([], [x]) \ M'] || mu'
                        | FINAL =>
                            let
                              val RET $ [_ \ E] = Abt.out M'
                            in
                              subst (E, x) M || mu
                            end)
                | _ => raise Match)
  and stepDecl mu E a M =
    case step E of
         STEP E' => O.DCL $$ [emp \ E', ([a], []) \ M] || mu
       | FINAL =>
           case trans (M || SCtx.insert mu a E) of
                STEP (M' || mu') =>
                  let
                    val E' = SCtx.lookup mu' a
                  in
                    O.DCL $$ [emp \ E', ([a], []) \ M'] || SCtx.remove mu' a
                  end
              | FINAL =>
                  let
                    val O.RET $ [_ \ E'] = Abt.out M
                  in
                    M || mu (* is this right? looks wrong *)
                  end

end
