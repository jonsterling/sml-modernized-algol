signature DYNAMICS =
sig
  type exp (* expressions *)
  type cmd (* commands *)

  type memory (* symbol environments *)
  datatype state = || of cmd * memory

  datatype 'a step =
      STEP of 'a
    | FINAL

  val step : exp -> exp step
  val trans : state -> state step

end
