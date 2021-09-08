exception Atom_exception
    structure AST =
    struct
    datatype Prop = 
        Atom of string | NOT of Prop | AND of Prop * Prop | OR of Prop * Prop | COND of Prop * Prop | BIC of Prop * Prop | ITE of Prop * Prop

    datatype Argument = HENCE of Prop list * Prop