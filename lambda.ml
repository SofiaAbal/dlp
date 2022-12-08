
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyPair of ty * ty
  | TyString
  | TyUnit
  | TyList of ty
;;

type 'a context =
  (string * 'a) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term (* just one parameter, just like lambda.mli *)
  | TmPair of term * term
  | TmPairFstProj of term
  | TmPairSndProj of term
  | TmString of string
  | TmConcat of term * term
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
  | TmUnit
;;

type command = 
  Eval of term
  | Bind of string * term
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyPair (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ", " ^ string_of_ty ty2 ^ ")"
  | TyString ->
      "String"
  | TyUnit ->
      "Unit"
  | TyList ty ->
      string_of_ty ty ^ " List"
;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try getbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyArr (tyT11, tyT12) -> 
          if tyT11 = tyT12 then tyT12  (* or tyT11, it should be the same *)
          else raise (Type_error "result of body not compatible with domain")
      | _ -> raise (Type_error "arroy type expected")
    )

    (* T-Pair *)
  | TmPair (t1, t2) ->
      TyPair(typeof ctx t1, typeof ctx t2)

    (* T-Proj1 *)
  | TmPairFstProj t1 -> 
      typeof ctx t1

    (* T-Proj2 *)
  | TmPairSndProj t2 -> 
      typeof ctx t2

  (* T-String *)
  | TmString _ ->
      TyString

    (* T-Concat *)
  | TmConcat (t1, t2) ->
      if typeof ctx t1 <> TyString then raise (Type_error "first argument of concat is not a string")
      else if typeof ctx t2 <> TyString then raise (Type_error "second argument of concat is not a string")
      else TyString

      (* T-Unit *)
  | TmUnit ->
      TyUnit
   
      (* T-Nil *)
  | TmNil ty -> TyList ty

      (* T-Cons *)
  | TmCons (ty,h,t) -> 
    let tyTh = typeof ctx h in
      let tyTt = typeof ctx t in
        if (tyTh = ty) && (tyTt = TyList(ty)) then TyList(ty)
        else raise (Type_error "all list elements must share the same type")
  
        (* T-IsNil *)
  | TmIsNil (ty,t) ->
    if typeof ctx t = TyList(ty) then TyBool
    else raise (Type_error ("argument of isNil must be a list"))

        (* T-Head *)
  | TmHead (ty,t) ->
    if typeof ctx t = TyList(ty) then ty
    else raise (Type_error ("argument of head must be a list"))
        
    (* T-Tail *)
  | TmTail (ty,t) ->
      if typeof ctx t = TyList(ty) then TyList(ty)
      else raise (Type_error ("argument of tail must be a list"))
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t -> 
      "(fix" ^ string_of_term t ^ ")"
  | TmPair (t1, t2) -> 
      "(" ^ string_of_term t1 ^ ", " ^ string_of_term t2 ^ ")"
  | TmPairFstProj t1 ->
      string_of_term t1 ^ ".1"
  | TmPairSndProj t2 ->
      string_of_term t2 ^ ".2"
  | TmString s ->
        "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
        string_of_term t1 ^ " ^ " ^ string_of_term t2
  | TmUnit ->
      "()"
  | TmNil ty ->
    "nil[" ^ string_of_ty ty ^ "]"
  | TmCons (ty,h,t) ->
    "cons[" ^ string_of_ty ty ^ "] " ^ "(" ^ string_of_term h ^ ") (" ^ (string_of_term t) ^ ")"
  | TmIsNil (ty,t) ->
    "isnil[" ^ string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")"
  | TmHead (ty,t) ->
    "head[" ^ string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")"
  | TmTail (ty,t) ->
    "tail[" ^ string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")"
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmPair (t1, t2) -> 
      lunion (free_vars t1) (free_vars t2)
  | TmPairFstProj t1 ->
      free_vars t1
  | TmPairSndProj t2 ->
      free_vars t2
  | TmString _ ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmUnit ->
      []
  | TmNil ty -> 
        []
  | TmCons (ty,t1,t2) -> 
        lunion (free_vars t1) (free_vars t2)
  | TmIsNil (ty,t) ->
        free_vars t
  | TmHead (ty,t) ->
        free_vars t
  | TmTail (ty,t) ->
        free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmPair (t1, t2) ->
      TmPair (subst x s t1, subst x s t2)
  | TmPairFstProj t1 ->
      TmPairFstProj (subst x s t1)
  | TmPairSndProj t2 ->
      TmPairSndProj (subst x s t2)
  | TmString s ->
      TmString s
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmUnit ->
    TmUnit
  | TmNil ty -> 
      tm
  | TmCons (ty,t1,t2) -> 
      TmCons (ty, (subst x s t1), (subst x s t2))
  | TmIsNil (ty,t) ->
      TmIsNil (ty, (subst x s t))
  | TmHead (ty,t) ->
      TmHead (ty, (subst x s t))
  | TmTail (ty,t) ->
      TmTail (ty, (subst x s t))

;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | t when isnumericval t -> true
  | TmPair (a, b) when ((isval a) && (isval b)) -> true
  | TmString _ -> true
  | TmUnit -> true
  | TmNil _ -> true
  | TmCons(_,h,t) -> (&&) (isval h) (isval t)
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 vctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2) 
  
    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) -> 
      subst x tm t2

    (* E-Fix *)
  | TmFix t1 -> 
      let t1' = eval1 vctx t1 in
      TmFix t1'

  | TmVar s ->
      getbinding vctx s

    (* E-PairBeta1*)
  | TmPairFstProj (TmPair (t1, t2)) when isval t1 ->
    (try fst (t1, t2) with
    _ -> raise NoRuleApplies)

  (* E-PairBeta2*)
  | TmPairSndProj (TmPair (t1, t2)) when isval t2 ->
    (try snd (t1, t2) with
    _ -> raise NoRuleApplies)

    (* E-Proj1 *)
  | TmPairFstProj t1 ->
      TmPairFstProj (eval1 vctx t1)

    (* E-Proj2 *)
  | TmPairSndProj t2 ->
      TmPairSndProj (eval1 vctx t2)

  (* E-Pair2 *)
  | TmPair (v1, t2) when isval v1 ->
      TmPair (v1, eval1 vctx t2)

    (* E-Pair1 *)
  | TmPair (t1, t2) ->
      TmPair (eval1 vctx t1, t2)

    (* E-Concat *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

  | TmConcat (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmConcat (v1, t2')

  | TmConcat (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmConcat(t1', t2)
    
      (* E-Cons *)
  | TmCons(ty,h,t) when isval h -> 
    TmCons(ty,h,(eval1 vctx t)) 

  | TmCons(ty,h,t) -> 
    TmCons(ty,(eval1 vctx h),t)

    (* E-IsNil *)
  | TmIsNil(ty,TmNil(_)) -> 
    TmTrue  

  | TmIsNil(ty,TmCons(_,_,_)) -> 
    TmFalse

  | TmIsNil(ty,t) -> 
    TmIsNil(ty,eval1 vctx t)

    (* E-Head *)
  | TmHead(ty,TmCons(_,h,_)) -> 
    h

  | TmHead(ty,t) -> 
    TmHead(ty,eval1 vctx t)

    (* E-Tail *)
  | TmTail(ty,TmCons(_,_,t)) -> 
    t

  | TmTail(ty,t) -> 
    TmTail(ty,eval1 vctx t)
    | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm = 
  List.fold_left (fun t x -> subst x (getbinding ctx x) t) tm (free_vars tm)
;;

let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> apply_ctx vctx tm
;;

let execute (vctx, tctx) = function
  Eval tm ->
    let tyTm = typeof tctx tm in
    let tm' = eval vctx tm in
    print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (vctx, tctx)
  | Bind (s, tm) ->
    let tyTm = typeof tctx tm in
    let tm' = eval vctx tm in
    print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (addbinding vctx s tm', addbinding tctx s tyTm)
;;