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

  structure Eq = DiscreteEq (type t = t)

  structure Show =
  struct
    type t = t
    fun toString TYP = "typ"
      | toString EXP = "exp"
      | toString CMD = "cmd"
  end
end
