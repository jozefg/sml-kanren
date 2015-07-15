structure Operator =
struct
  datatype t
    = PAIR
    | ATOM of string

  val eq = op=
  fun arity oper =
    case oper of
        PAIR => #[0, 0]
      | ATOM _ => #[]

  fun toString oper =
    case oper of
        PAIR => "pair"
      | ATOM s => s
end

structure A = Abt(structure O = Operator
                  structure V = Variable())
structure Term = AbtUtil(A)
