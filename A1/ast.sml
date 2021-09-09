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
end ;
exception ScanError;
exception ParseError;