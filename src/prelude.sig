signature KANREN_PRELUDE =
sig
    type term
    type program

    (* Convert a list of Kanren terms into a
     * Scheme style list terminating in [atom "nil"]
     *)
    val list : term list -> term

    (* Given a term and a list as arguments, this relationship
     * holds the term is contained in the list.
     *)
    val membero : term -> term -> program

    (* Various predicates for listiness*)
    val nilo  : term -> program
    val conso : term -> program
    val listo : term -> program

    (* Convert a natural number into a ml-kanren term by representing it
     * as list of one's and zero's, little endian style
     *)
    val num   : int -> term

    (* A predicate to check whether a term is a number*)
    val numo  : term -> program

    (* The digits for one and zero. Note that these *aren't numbers*
     * they're what numbers are built from
     *)
    val zero : term
    val one  : term

    val inco  : term -> term -> program
    val pluso : term -> term -> term -> program
end
