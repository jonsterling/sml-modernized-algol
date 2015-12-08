functor DiscreteEq (eqtype t) : EQ =
struct
  type t = t
  val eq = op=
end
