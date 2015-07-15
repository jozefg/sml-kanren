signature KANREN =
sig
    type var
    type term

    val atom : string -> term
    val pair : term * term -> term
    val var  : var -> term

    type program

    val ===     : term * term -> program
    val conj    : program -> program -> program
    val disconj : program -> program -> program
    val fresh   : (term -> program) -> program

    type sol = (var * term) list

    val run  : program -> sol list
    val runN : int -> program -> sol list
end
