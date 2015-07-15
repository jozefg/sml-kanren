structure Kanren :> KANREN =
struct
  structure Unify = AbtUnify(Term)
  open Operator
  open Term
  infix $ \
  infix 8 $$ // \\

  type var = Var.t
  val varToString = Var.toString

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

  fun conj (p1, p2) s = Stream.>>- (Stream.delay (fn () => p1 s),
                                    fn s => Stream.delay (fn () => p2 s))

  fun disconj (p1, p2) s = Stream.merge (Stream.delay (fn () => p1 s))
                                        (Stream.delay (fn () => p2 s))

  fun fresh f s = f (`` (Var.new ())) s
  fun freshN i f s = f (List.tabulate (i, fn _ => `` (Var.new ()))) s

  fun run p =
    let
      val t = `` (Var.new ())
    in
      Stream.toList (Stream.map (fn s => applySol s t) (p t []))
    end

  fun runN i p =
    let
      val t = `` (Var.new ())
    in
      Stream.observe i (Stream.map (fn s => applySol s t) (p t []))
    end
    handle Stream.Empty => raise Subscript
end
