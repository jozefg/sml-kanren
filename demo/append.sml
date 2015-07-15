structure Append =
struct
  open Kanren
  infix 3 ===
  infixr 1 conj disconj

  fun appendo xs ys zs =
      (xs === atom "nil" conj ys === zs)
          disconj
      fresh (fn x =>
      fresh (fn y =>
      fresh (fn z =>
        xs === pair (x, y) conj
        zs === pair (x, z) conj
        appendo y ys z)))

  val _::_ = run (fn _ => appendo (atom "nil") (atom "nil") (atom "nil"))
  val _::_ = run (fn x =>
                    appendo (pair (atom "foo", atom "nil"))
                            (pair (atom "bar", atom "nil"))
                            x)
  val [] = run (fn x =>
                fresh (fn y =>
                  appendo (atom "nil") (atom "nil") (pair (x, y))))
end
