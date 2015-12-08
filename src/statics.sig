signature STATICS =
sig
  type exp (* expression *)
  type cmd (* commands *)
  type typ (* types *)

  type vctx (* variable contexts *)
  type sctx (* symbol context *)

  val mobile : typ -> bool

  val check : vctx * sctx -> exp -> typ
  val checkCmd : vctx * sctx -> cmd -> typ
end
