structure SortData =
struct
  datatype t =
      TYP
    | EXP
    | CMD
end

structure Sort : SORT =
struct
  open SortData

  val eq = op=

  fun toString TYP = "typ"
    | toString EXP = "exp"
    | toString CMD = "cmd"
end
