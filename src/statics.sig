signature STATICS =
sig
  type vctx
  type sctx

  type exp
  type cmd
  type typ

  val mobile : typ -> bool

  val check : vctx * sctx -> exp -> typ
  val checkCmd : vctx * sctx -> cmd -> typ

end
