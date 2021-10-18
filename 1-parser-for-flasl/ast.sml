structure AST =
struct
    datatype Prop = ATOM of string     
                |   NOT of Prop        
                |   AND of Prop * Prop 
                |   OR of Prop * Prop  
                |   COND of Prop * Prop 
                |   BIC of Prop * Prop 
                |   ITE of Prop * Prop * Prop
    datatype Argument = HENCE of (Prop list) * Prop

    fun propToString (ATOM(s))          = ("\"" ^ s ^ "\"")
    |   propToString (NOT(p))           = ("NOT (" ^ propToString(p) ^")")
    |   propToString (AND(p1,p2))       = ("(" ^ propToString(p1) ^") AND (" ^ propToString(p2) ^ ")")
    |   propToString (OR(p1,p2))        = ("(" ^ propToString(p1) ^") OR (" ^ propToString(p2) ^ ")")
    |   propToString (COND(p1,p2))      = ("(" ^ propToString(p2) ^") IF (" ^ propToString(p1) ^ ")")
    |   propToString (BIC(p1,p2))       = ("(" ^ propToString(p1) ^") IFF (" ^ propToString(p2) ^ ")")
    |   propToString (ITE(p1,p2,p3))    = ("IF (" ^ propToString(p1) ^") THEN (" ^ propToString(p2) ^ ") ELSE (" ^ propToString(p3) ^ ")")
    
    fun listToString []         = ""
    |   listToString (x::xs)    =  (propToString(x) ^ ". \n" ^ (listToString(xs)))

    fun ast2flasl (HENCE([], p)) =  ("THEREFORE " ^ propToString(p)^".")
    |   ast2flasl (HENCE(l, p))  =  (listToString(l) ^ "THEREFORE " ^ propToString(p)^".")

    fun propToTree (ATOM(s))          = ("ATOM(\"" ^ s ^ "\")")
    |   propToTree (NOT(p))           = ("NOT (" ^ propToTree(p) ^")")
    |   propToTree (AND(p1,p2))       = ("AND (" ^ propToTree(p1) ^" , " ^ propToTree(p2) ^ ")")
    |   propToTree (OR(p1,p2))        = ("OR (" ^ propToTree(p1) ^" , " ^ propToTree(p2) ^ ")")
    |   propToTree (COND(p1,p2))      = ("COND (" ^ propToTree(p1) ^" , " ^ propToTree(p2) ^ ")")
    |   propToTree (BIC(p1,p2))       = ("BIC (" ^ propToTree(p1) ^" , " ^ propToTree(p2) ^ ")")
    |   propToTree (ITE(p1,p2,p3))    = ("ITE (" ^ propToTree(p1) ^" , " ^ propToTree(p2) ^ " , " ^ propToTree(p3) ^ ")")

    fun listToTree []           = ""
    |   listToTree (x::[])      = (propToTree(x))
    |   listToTree (x::xs)      =  (propToTree(x) ^ ", \n" ^ (listToTree(xs)))
    

    fun ast2tree (HENCE([], p)) =  ("val ast1 = HENCE([], " ^ propToTree(p) ^");")
    |   ast2tree (HENCE(l, p))  =  ("val ast1 = HENCE([" ^ listToTree(l) ^ "], " ^ propToTree(p) ^");")
 
end ;

exception ScanError;
exception ParseError;

(*AST.argToFile(AST.HENCE([AST.COND(AST.ATOM("a"),AST.ATOM("b"))], AST.ATOM("None")));*)