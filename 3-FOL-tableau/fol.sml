structure FOL = 
struct 
    datatype term = VAR   of string
                |   FUN   of string * term list
                |   CONST of string (* for generated constants only *)
                
    datatype Pred = FF (* special constant for closing a tableau path *)
                |   ATOM  of string * term list
                |   NOT   of Pred
                |   AND   of Pred * Pred
                |   OR    of Pred * Pred
                |   COND  of Pred * Pred
                |   BIC   of Pred * Pred
                |   ITE   of Pred * Pred * Pred
                |   ALL   of term * Pred
                |   EX    of term * Pred
    
    datatype Argument = HENCE  of Pred list * Pred


    fun convertITE  (ATOM(s))           = ATOM(s)
    |   convertITE  FF                  = FF
    |   convertITE  (NOT(p))            = NOT(convertITE p)
    |   convertITE  (AND(p1, p2))       = AND(convertITE p1, convertITE p2)
    |   convertITE  (OR(p1, p2))        = OR(convertITE p1, convertITE p2)
    |   convertITE  (COND(p1, p2))      = COND(convertITE p1, convertITE p2)
    |   convertITE  (BIC(p1, p2))       = BIC(convertITE p1, convertITE p2)
    |   convertITE  (ITE(p1, p2, p3))   = let 
                                            val temp = convertITE p1 
                                          in 
                                            OR(AND(temp, convertITE p2), AND(NOT(temp), convertITE p3)) 
                                          end
    |   convertITE  (ALL(t, p))         = ALL(t, convertITE p)
    |   convertITE  (EX(t, p))          = EX(t, convertITE p)

    fun list2Conjuction (x::[])      =  x
    |   list2Conjuction (x::xs)      = AND(x, list2Conjuction(xs))

    fun arg2Tree    HENCE([], phi) = convertITE NOT(phi)
    |   arg2Tree    HENCE(l, phi)  = convertITE AND(list2Conjuction(l), NOT(phi))

    fun substituteTerm t s num replaceWithConst = case t of
                                    VAR(s1)     => if (s1=s) then (if replaceWithConst then CONST(Int.toString(num)) else VAR(Int.toString(num))) else t
                                |   FUN(s1, t1) => FUN(s1, substituteTermList t1 s num replaceWithConst)
                                |   _           => t

    fun substituteTermList [] s num replaceWithConst        = []
    |   substituteTermList (x::xs) s num replaceWithConst   = (substituteTerm x s num replaceWithConst)::(substituteTermList xs s num replaceWithConst)

    fun substituteVar prop s num replaceWithConst = case prop of
                                    AND(p1, p2)     => AND(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst)
                                |   NOT(p)          => NOT(substituteVar p s num replaceWithConst)
                                |   OR(p1, p2)      => OR(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst)
                                |   COND(p1, p2)    => COND(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst)
                                |   BIC(p1, p2)     => BIC(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst)
                                |   ITE(p1, p2, p3) => ITE(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst, substituteVar p3 s num replaceWithConst)
                                |   ALL(t, p)       => ALL(t, substituteVar p2 s num replaceWithConst)
                                |   EX(t, p)        => EX(t, substituteVar p s num replaceWithConst)
                                |   ATOM(s1, t)     => ATOM(s1, substituteTermList l s num replaceWithConst)

    fun isBoolean prop = case prop of 
                        AND(p1, p2)         =>  true
                    |   NOT(AND(p1, p2))    =>  true
                    |   OR(p1, p2)          =>  true
                    |   NOT(OR(p1, p2))     =>  true
                    |   COND(p1, p2)        =>  true
                    |   NOT(COND(p1, p2))   =>  true
                    |   BIC(p1, p2)         =>  true
                    |   NOT(BIC(p1, p2))    =>  true
                    |   NOT(NOT(p))         =>  true
                    |   ATOM(s, t)         =>  true
                    |   NOT(ATOM(s, t))     =>  true
                    |   _                   =>  false  

    fun isExistential prop = case prop of 
                        EX(t, p)            =>  true
                    |   NOT(ALL(t, p))      =>  true
                    |   _                   =>  false

    fun isUniversal prop = case prop of 
                        ALL(t, p)          =>  true
                    |   NOT(EX(t, p))      =>  true
                    |   _                  =>  false
    
    fun removeNth [] n = []
    |   removeNth l n = if n<0 then l else
                        ( 
                            if n=0 then (List.tl l) 
                            else (List.hd l)::(removeNth (List.tl l) n-1)
                        )
    fun tillNth [] n = []
    |   tillNth l n  = if n<=0 then [] else (List.hd l)::(tillNth (List.tl l) n-1)
    
    fun afterNth [] n = []
    |   afterNth l n = if n<0 then l else
                        (
                            if n=0 then (List.tl l)
                            else (afterNth (List.tl l) n-1)
                        )
    
    fun noBoolOccurs [] = true
    |   noBoolOccurs (x::xs) = if ((isExistential x) or (isUniversal x)) then (noBoolOccurs xs) else false

    fun noFutureBool l n = if n=0 then (noBoolOccurs l) else (noFutureBool (removeNth l n) n-1)

    fun noExistOccurs [] = true
    |   noExistOccurs (x::xs) = if (isUniversal x) then (noExistOccurs xs) else false

    fun noFutureExist l n = if n=0 then (noExistOccurs l) else (noFutureExist (removeNth l n) n-1)

    fun formTableau []  exVar unVar = []
    |   formTableau (branch::branchList) exVar unVar = let
                                                val indexToProcess  = (#2 branch)
                                                val propList        = (#1 branch)
                                            in
                                                if indexToProcess >= (List.length propList)
                                                then (propList::(formTableau branchList))
                                                else 
                                                    let
                                                        val prop = List.nth(propList, indexToProcess)
                                                        val n = indexToProcess
                                                    in
                                                        if (isBoolean prop) then
                                                            case prop of
                                                            AND(p1, p2)         =>  formTableau ((propList@[p1, p2], n+1)::branchList) exVar unVar
                                                        |   NOT(AND(p1, p2))    =>  formTableau ((propList@[NOT(p1)], n+1)::(propList@[NOT(p2)], n+1)::branchList) exVar unVar
                                                        |   OR(p1, p2)          =>  formTableau ((propList@[p1], n+1)::(propList@[p2], n+1)::branchList) exVar unVar
                                                        |   NOT(OR(p1, p2))     =>  formTableau ((propList@[NOT(p1), NOT(p2)], n+1)::branchList) exVar unVar
                                                        |   COND(p1, p2)        =>  formTableau ((propList@[NOT(p1)], n+1)::(propList@[p2], n+1)::branchList) exVar unVar
                                                        |   NOT(COND(p1, p2))   =>  formTableau ((propList@[p1, NOT(p2)], n+1)::branchList) exVar unVar
                                                        |   BIC(p1, p2)         =>  formTableau ((propList@[p1, p2], n+1)::(propList@[NOT(p1), NOT(p2)], n+1)::branchList) exVar unVar
                                                        |   NOT(BIC(p1, p2))    =>  formTableau ((propList@[p1, NOT(p2)], n+1)::(propList@[NOT(p1), p2], n+1)::branchList) exVar unVar
                                                        |   NOT(NOT(p))         =>  formTableau ((propList@[p], n+1)::branchList) exVar unVar
                                                        |   _                   =>  formTableau ((propList, n+1)::branchList) exVar unVar
                                                        else 
                                                        if (isExistential prop) then
                                                            (
                                                                if (noFutureBool propList n) then
                                                                    case prop of 
                                                                    EX(VAR(s), p)       => let val np = (substituteVar p s exVar true) in formTableau (((tillNth propList)@[prop, np]@(afterNth propList), n+1)::branchList) (exVar+1) unVar end
                                                                |   NOT(ALL(VAR(s), p)) => let val np = NOT(substituteVar p s exVar true) in formTableau (((tillNth propList)@[prop, np]@(afterNth propList), n+1)::branchList) (exVar+1) unVar end
                                                                |   _                   => formTableau ((propList, n+1)::branchList) exVar unVar
                                                                else
                                                                    (formTableau (((removeNth propList n)@[prop], n)::branchList)  exVar unVar) 
                                                            )
                                                        else
                                                        if (isUniversal prop) then
                                                            (
                                                                if (noFutureExist propList n) then
                                                                    case prop of 
                                                                    ALL(VAR(s), p)      => let val np = (substituteVar p s unVar false) in formTableau (((tillNth propList)@[np]@(afterNth propList)@[prop], n)::branchList) exVar (unVar+1) end
                                                                |   NOT(EX(VAR(s), p))  => let val np = NOT(substituteVar p s unVar false) in formTableau (((tillNth propList)@[np]@(afterNth propList)@[prop], n)::branchList) exVar (unVar+1) end
                                                                |   _                   => formTableau ((propList, n+1)::branchList) exVar unVar
                                                                else
                                                                    (formTableau (((removeNth propList n)@[prop], n)::branchList)  exVar unVar)
                                                            )
                                                        else
                                                            formTableau ((propList, n+1)::branchList) exVar unVar
                                                    end
                                            end

end

exception NotVAR (* Binding term in a quantified formula is not a variable *)
exception NotWFT (* term is not well-formed *)
exception NotWFP (* predicate is not well-formed *)
exception NotWFA (* argument is not well-formed *)
exception NotClosed (* a formula is not closed *)