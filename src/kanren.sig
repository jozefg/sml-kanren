signature KANREN =
sig
    type var
    type term

    val varToString : var -> string
    val toString    : term -> string

    val atom : string -> term
    val pair : term * term -> term
    val var  : var -> term

    type program

    val ===     : term * term -> program
    val conj    : program * program -> program
    val disconj : program * program -> program
    val fresh   : (term -> program) -> program
    val freshN  : int -> (term list -> program) -> program

    val run  : (term -> program) -> term list
    val runN : int -> (term -> program) -> term list
end
