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


    fun term2Str (VAR(s)) = "VAR("^s^")"
    |   term2Str (FUN(s, t)) = "FUN "^s^"("^(termlist2Str t)^")"
    |   term2Str (CONST(s)) = "CONST("^s^")"

    and termlist2Str [] = ""
    |   termlist2Str (x::[]) = term2Str x
    |   termlist2Str (x::xs) = (term2Str x)^", "^(termlist2Str xs)

    val temp = (term2Str (VAR("x")));

    fun replacement2Str a b = "replace "^(term2Str a)^" with "^(term2Str b)^"\n"

    val r = (replacement2Str (VAR("x")) (VAR("x")));

    fun contradiction2Str [] = ""
    |   contradiction2Str ((a, b)::xs) = (replacement2Str a b)^(contradiction2Str xs)

    fun pred2Str (FF) = "\n"
    |   pred2Str (ATOM(s, t))         = s^"("^(termlist2Str t)^")"
    |   pred2Str (NOT(p))             = "~("^(pred2Str p)^")"
    |   pred2Str (AND(p1, p2))        = "("^(pred2Str p1)^")"^" && ("^(pred2Str p2)^")"
    |   pred2Str (OR(p1, p2))         = "("^(pred2Str p1)^")"^" || ("^(pred2Str p2)^")"
    |   pred2Str (COND(p1, p2))       = "("^(pred2Str p1)^")"^" IMPLIES ("^(pred2Str p2)^")"
    |   pred2Str (BIC(p1, p2))        = "("^(pred2Str p1)^")"^" IFF ("^(pred2Str p2)^")"
    |   pred2Str (ITE(p1, p2, p3))    = "IF ("^(pred2Str p1)^")"^" THEN ("^(pred2Str p2)^") ELSE ("^(pred2Str p3)^")"
    |   pred2Str (ALL(t, p))          = "forall["^(term2Str t)^"]("^(pred2Str p)^")"
    |   pred2Str (EX(t, p))           = "exists["^(term2Str t)^"]("^(pred2Str p)^")"

    fun branch2Str (x::[]) = (pred2Str x)
    |   branch2Str (x::xs) = (pred2Str x)^"\n"^(branch2Str xs)
    |   branch2Str []   =   ""


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

    fun arg2Tree (HENCE([], phi)) = (convertITE (NOT(phi)))
    |   arg2Tree (HENCE(l, phi))  = (convertITE (AND(list2Conjuction(l), NOT(phi))))

    fun substituteTerm t s num replaceWithConst = case t of
                                    VAR(s1)     => if (s1=s) then (if replaceWithConst then CONST(num) else VAR(num)) else t
                                |   FUN(s1, t1) => FUN(s1, substituteTermList t1 s num replaceWithConst)
                                |   _           => t

    and substituteTermList [] s num replaceWithConst        = []
    |   substituteTermList (x::xs) s num replaceWithConst   = (substituteTerm x s num replaceWithConst)::(substituteTermList xs s num replaceWithConst)

    fun substituteVar prop s num replaceWithConst = case prop of
                                    AND(p1, p2)     => AND(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst)
                                |   NOT(p)          => NOT(substituteVar p s num replaceWithConst)
                                |   OR(p1, p2)      => OR(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst)
                                |   COND(p1, p2)    => COND(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst)
                                |   BIC(p1, p2)     => BIC(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst)
                                |   ITE(p1, p2, p3) => ITE(substituteVar p1 s num replaceWithConst, substituteVar p2 s num replaceWithConst, substituteVar p3 s num replaceWithConst)
                                |   ALL(t, p)       => ALL(t, substituteVar p s num replaceWithConst)
                                |   EX(t, p)        => EX(t, substituteVar p s num replaceWithConst)
                                |   ATOM(s1, t)     => ATOM(s1, substituteTermList t s num replaceWithConst)

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
                    |   ATOM(s, t)          =>  true
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
    |   removeNth l n = if (n<0) then l else
                        ( 
                            if (n=0) then (List.tl l) 
                            else ((List.hd l)::(removeNth (List.tl l) (n-1)))
                        )
    fun tillNth [] n = []
    |   tillNth l n  = if (n<=0) then [] else ((List.hd l)::(tillNth (List.tl l) (n-1)))
    
    fun afterNth [] n = []
    |   afterNth l n = if n<0 then l else
                        (
                            if (n=0) then (List.tl l)
                            else (afterNth (List.tl l) (n-1))
                        )
    
    fun noBoolOccurs [] = true
    |   noBoolOccurs (x::xs) = if ((isExistential x) orelse (isUniversal x)) then (noBoolOccurs xs) else false

    fun noFutureBool l n = noBoolOccurs (afterNth l n)

    fun noExistOccurs [] = true
    |   noExistOccurs (x::xs) = if (isUniversal x) then (noExistOccurs xs) else false

    fun noFutureExist l n = noExistOccurs (afterNth l n)

    fun replaceInList a b [] = []
    |   replaceInList a b (x::xs) =  case x of 
                                    FUN(m ,t)   => (FUN(m, replaceInList a b t))::(replaceInList a b xs)
                                |   _           => (if (a=x) then b else x)::(replaceInList a b xs)

    fun substituteDisagreementTermList [] l = l
    | substituteDisagreementTermList ((a, b)::xs) l = substituteDisagreementTermList xs (replaceInList a b l)

    fun checkNoVar [] v = true
    |   checkNoVar (x::xs) v = case x of
                                VAR(s)      => if (VAR(s) = v) then false else (checkNoVar xs v)
                            |   CONST(s)    => true
                            |   FUN(s, t)   => checkNoVar t v 

    fun disagreement (CONST(x)) (CONST(y)) = (x=y, [])
    |   disagreement (VAR(x)) (CONST(y)) = (true, [(VAR(x), CONST(y))])
    |   disagreement (CONST(x)) (VAR(y)) = (true, [(VAR(y), CONST(x))])
    |   disagreement (VAR(x)) (VAR(y)) = (true, [(VAR(y), VAR(x))])
    |   disagreement (CONST(x)) (FUN(s, t)) = (false, [])
    |   disagreement (FUN(s, t)) (CONST(x)) = (false, [])
    |   disagreement (VAR(x)) (FUN(s, t)) = if (checkNoVar t (VAR(x))) then (true, [(VAR(x), FUN(s, t))]) else (false, [])
    |   disagreement (FUN(s, t)) (VAR(x)) = if (checkNoVar t (VAR(x))) then (true, [(VAR(x), FUN(s, t))]) else (false, [])
    |   disagreement (FUN(s1, t1)) (FUN(s2, t2)) = if (s1=s2) then (checkTermList t1 t2) else (false, [])

    and unifyTermList (x1::y1) (x2::y2) = let 
                                            val dis = (disagreement x1 x2)
                                        in
                                            if (#1 dis) then (
                                                let 
                                                    val y1sub = (substituteDisagreementTermList (#2 dis) y1)
                                                    val y2sub = (substituteDisagreementTermList (#2 dis) y2)
                                                    val moreUnify = (unifyTermList y1sub y2sub)
                                                in
                                                    if (#1 moreUnify) then( (true, (#2 dis)@(#2 moreUnify))) else (false, [])
                                                end
                                            )
                                            else(
                                                (false, [])
                                            )
                                        end
    |   unifyTermList [] [] = (true, [])

    and checkTermList t1 t2 = if ((List.length t1) = (List.length t2)) then (unifyTermList t1 t2) else (false, [])
    
    fun complimentOfAtom s t [] = (false, [])
    |   complimentOfAtom s t (NOT(ATOM(s1, t1))::xs) =  if (s1=s) then 
                                                        let
                                                            val atomComp = (checkTermList t t1) 
                                                        in
                                                            if (#1 atomComp) then (atomComp) else (complimentOfAtom s t xs)
                                                        end
                                                        else (complimentOfAtom s t xs)
    |   complimentOfAtom s t (x::xs) = (complimentOfAtom s t xs)

    fun checkCompliments [] branch = (false, [])
    |   checkCompliments (ATOM(s, t)::xs) branch = let 
                                                        val unified = (complimentOfAtom s t branch)
                                                    in 
                                                        if (#1 unified) then (unified) else (checkCompliments xs branch)
                                                    end
    |   checkCompliments (x::xs) branch = (checkCompliments xs branch)

    fun branchIsClosed  branch = (checkCompliments branch branch)

    fun extractLiterals []                     = []
    |   extractLiterals (ATOM(s, t)::xs)       = (ATOM(s, t) :: (extractLiterals xs))
    |   extractLiterals (NOT(ATOM(s, t))::xs)  = (NOT(ATOM(s, t)) :: (extractLiterals xs))
    |   extractLiterals (x::xs)                = (extractLiterals xs) 

    fun formTableau []  exVar unVar = []
    |   formTableau (branch::branchList) exVar unVar = 
                                            let 
                                                val unification = branchIsClosed (extractLiterals (#1 branch))
                                            in
                                            if (#1 unification)
                                            then
                                            (  (#1 branch, #2 unification)::(formTableau branchList exVar unVar) )
                                            else
                                            (
                                            let
                                                val indexToProcess  = (#2 branch)
                                                val propList        = (#1 branch)
                                            in
                                                if indexToProcess >= (List.length propList)
                                                then ((propList, [])::(formTableau branchList exVar unVar))
                                                else 
                                                    let
                                                        val prop = List.nth(propList, indexToProcess)
                                                        val n = indexToProcess
                                                    in
                                                        print("Branch\n"^(branch2Str propList)^"\n");
                                                        print("No Unification " ^ (Int.toString (List.length propList)) ^" " ^ (Int.toString n) ^ "\n");
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
                                                        (
                                                        print("Not a boolean \n");
                                                        if (isExistential prop) then
                                                            (
                                                                if (noFutureBool propList n) then
                                                                    case prop of 
                                                                    EX(VAR(s), p)       => let val np = (substituteVar p s (Int.toString(exVar)) true) in formTableau (((tillNth propList n)@[prop, np]@(afterNth propList n), (n+1))::branchList) (exVar+1) unVar end
                                                                |   NOT(ALL(VAR(s), p)) => let val np = NOT(substituteVar p s (Int.toString(exVar)) true) in formTableau (((tillNth propList n)@[prop, np]@(afterNth propList n), (n+1))::branchList) (exVar+1) unVar end
                                                                |   _                   => formTableau ((propList, n+1)::branchList) exVar unVar
                                                                else
                                                                    (formTableau (((removeNth propList n)@[prop], n)::branchList)  exVar unVar) 
                                                            )
                                                        else
                                                        (
                                                        print("Not an Existential \n");
                                                            if (isUniversal prop) then
                                                                (
                                                                    if (noFutureExist propList n) then
                                                                        case prop of 
                                                                        ALL(VAR(s), p)      => let val np = (substituteVar p s (Int.toString(unVar)) false) in formTableau (((tillNth propList n)@[np]@(afterNth propList n)@[prop], n)::branchList) exVar (unVar+1) end
                                                                    |   NOT(EX(VAR(s), p))  => let val np = NOT(substituteVar p s (Int.toString(unVar)) false) in formTableau (((tillNth propList n)@[np]@(afterNth propList n)@[prop], n)::branchList) exVar (unVar+1) end
                                                                    |   _                   => formTableau ((propList, n+1)::branchList) exVar unVar
                                                                    else
                                                                        (
                                                                        print("No Future Exist\n"); 
                                                                        formTableau (((removeNth propList n)@[prop], n)::branchList)  exVar unVar
                                                                        )
                                                                )
                                                            else
                                                                (
                                                                print("Not a Universal \n"); 
                                                                formTableau ((propList, n+1)::branchList) exVar unVar
                                                                )
                                                        )
                                                        )
                                                    end
                                            end
                                            )
                                            end
    

    end

exception NotVAR (* Binding term in a quantified formula is not a variable *)
exception NotWFT (* term is not well-formed *)
exception NotWFP (* predicate is not well-formed *)
exception NotWFA (* argument is not well-formed *)
exception NotClosed (* a formula is not closed *)
open FOL