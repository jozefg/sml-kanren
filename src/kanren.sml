structure Kanren :> KANREN =
struct
  structure Unify = AbtUnify(Term)
  open Operator
  open Term
  infix $ \
  infix 8 $$ // \\

  type var = Variable.t
  val varToString = Variable.toString

  type term = t
  val toString = toString

  fun atom s = ATOM s $$ #[]
  fun pair (l, r) = PAIR $$ #[l, r]
  val var = ``

  type sol = (var * term) list
  type program = sol -> sol Stream.t

  fun applySol s e = List.foldl (fn ((v, e'), e) => subst e' v e) e s

  fun === (t1, t2) s =
    let
      val s2 = Unify.unify (applySol s t1, applySol s t2)
    in
      Stream.return (s2 @ List.map (fn (v, e) => (v, applySol s2 e)) s)
    end handle Unify.Mismatch _ => Stream.empty

  fun conj (p1, p2) s = Stream.>>= (p1 s, p2)

  fun disconj (p1, p2) s = Stream.merge (p1 s) (p2 s)

  fun fresh f s =
    let
      val t = `` (Variable.named "x")
    in
      f t s
    end

  fun run p = Stream.toList (p [])
  fun runN i p = Stream.observe i (p [])
                 handle Stream.Empty => raise Subscript
end
