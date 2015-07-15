functor KanrenPrelude (K : KANREN)
        :> KANREN_PRELUDE
              where type term = K.term
              where type program = K.program =
struct
  open K
  infix 3 ===
  infixr 1 conj disconj

  type term = term
  type program = program

  fun list xs = foldr pair (atom "nil") xs

  fun membero x xs =
      freshN 2 (fn [h, t] =>
        xs === pair (h, t) conj
        (h === x disconj membero x t))

  fun nilo xs = xs === atom "nil"
  fun conso xs = freshN 2 (fn [x, y] => xs === pair (x, y))
  fun listo xs = nilo xs disconj conso xs

  val one = atom "1"
  val zero = atom "0"

  fun num n =
      let
        fun go 0 = []
          | go n = if n mod 2 = 0
                   then atom "0" :: go (n div 2)
                   else atom "1" :: go (n div 2)
      in
        list (go n)
      end

  fun numo x =
      x === atom "nil"
        disconj
      freshN 2 (fn [h, t] =>
        x === pair (h, t) conj
        (h === one disconj h === zero) conj
        numo t)

  fun inco n out =
      (n === atom "nil" conj out === list [one])
          disconj
      freshN 3 (fn [h, t, dummy] =>
        n === pair (h, t) conj
        ((h === zero conj out === pair (one, t))
             disconj
         (h === one conj inco t dummy conj out === pair (zero, dummy))))

  fun pluso m n p =
      (m === atom "nil" conj n === p)
          disconj
      (n === atom "nil" conj m === p)
          disconj
      freshN 7 (fn [mh, mt, nh, nt, ph, pt, dummy] =>
        m === pair (mh, mt) conj
        n === pair (nh, nt) conj
        p === pair (ph, pt) conj
        ((mh === zero conj ph === nh conj pluso mt nt pt)
             disconj
         (nh === zero conj ph === mh conj pluso mt nt pt)
             disconj
         (mh === one conj nh === one conj ph === zero conj pluso mt nt dummy conj
          inco dummy pt)))
end
