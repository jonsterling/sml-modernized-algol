structure Statics : STATICS =
struct
  open Abt

  type exp = abt
  type cmd = abt
  type typ = abt

  type vctx = typ VCtx.dict
  type sctx = typ SCtx.dict

  infix $ $$ \

  structure S = SortData and O = OperatorData

  fun mobile T =
    case #1 (Abt.infer T) of
         O.NAT $ [] => true
       | _ => false

  fun check (Gm, Sg) E =
    let
      val (E', S.EXP) = Abt.infer E
    in
      case E' of
           O.CMD $ [_ \ M] =>
             let
               val T = checkCmd (Gm, Sg) M
             in
               O.CMD_TY $$ [([], []) \ T]
             end
         | O.NUM n $ [] => O.NAT $$ []
         | `x => VCtx.lookup Gm x
         | _ => raise Match
    end

  and checkCmd (Gm, Sg) M =
    let
      val (M', S.CMD) = Abt.infer M
    in
      case M' of
           O.RET $ [_ \ E] => check (Gm, Sg) E
         | O.BND $ [_ \ E, ([], [x]) \ M] =>
             let
               val (O.CMD_TY $ [_ \ T], S.TYP) = Abt.infer (check (Gm, Sg) E)
               val Gm' = VCtx.insert Gm x T
               val T' = checkCmd (Gm', Sg) M
             in
               T'
             end
         | O.DCL $ [_ \ E, ([a], []) \ M] =>
             let
               val T = check (Gm, Sg) E
               val true = mobile T
               val Sg' = SCtx.insert Sg a T
               val T' = checkCmd (Gm, Sg') M
               val true = mobile T'
             in
               T'
             end
         | O.GET a $ [] => SCtx.lookup Sg a
         | O.SET a $ [_ \ E] =>
             let
               val T = SCtx.lookup Sg a
               val T' = check (Gm, Sg) E
               val true = Abt.eq (T, T')
             in
               T
             end
         | _ => raise Match
    end

end
