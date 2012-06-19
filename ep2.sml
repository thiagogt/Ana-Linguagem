exception Indefinido;

datatype Expression =
	  VInt of int
	| VBool of bool
	| Id of string
	| AExp of Expression * string * Expression
	| BExp of Expression * string * Expression
	| If of string * Expression * Expression * Expression
	| Fun of string * Expression list * Expression * Expression list;

fun substituiAExp(Id id, AExp(Id id1, operacao, any), valor) = if id = id1 then AExp(valor, operacao, any)
                                                                            else AExp(Id id, operacao, any)
  | substituiAExp(Id id, AExp(any, operacao, Id id1), valor) = if id = id1 then AExp(any, operacao, valor)
                                                                            else AExp(any, operacao, Id id)
  | substituiAExp(id, AExp(AExp e1, operacao, any), v) = AExp(substituiAExp(id, AExp e1, v), operacao, any)
  | substituiAExp(id, AExp(any, operacao, AExp e1), v) = AExp(any, operacao, substituiAExp(id, AExp e1, v))
  | substituiAExp(v1, exp, v2) = exp;
  
fun substituiBExp(Id id, BExp(Id id1, operacao, any), valor) = if id = id1 then BExp(valor, operacao, any)
                                                                            else BExp(Id id, operacao, any)
  | substituiBExp(Id id, AExp(any, operacao, Id id1), valor) = if id = id1 then BExp(any, operacao, valor)
                                                                            else BExp(any, operacao, Id id)
  | substituiBExp(id, BExp(BExp e1, operacao, any), v) = BExp(substituiBExp(id, BExp e1, v), operacao, any)
  | substituiBExp(id, BExp(any, operacao, BExp e1), v) = BExp(any, operacao, substituiBExp(id, BExp e1, v))
  | substituiBExp(v1, exp, v2) = exp;
  
fun substitui(id, AExp aexp, valor) = substituiAExp(id, AExp aexp, valor)
  | substitui(id, BExp bexp, valor) = substituiBExp(id, BExp bexp, valor)
  | substitui(id, any, valor) = raise Indefinido;
                                                                        
(* substituicao dos ids em expressoes *)
substituiAExp(Id "x", AExp(Id "x", "+", VInt 1), VInt 1) = AExp(VInt 1, "+", VInt 1);
substituiAExp(Id "x", AExp(VInt 1, "+", Id "x"), VInt 3) = AExp(VInt 1, "+", VInt 3);
substituiAExp(Id "x", AExp(VInt 1, "+", AExp(VInt 1, "+", Id "x")), VInt 3) = AExp(VInt 1, "+", AExp(VInt 1, "+", VInt 3));
substituiAExp(Id "x", AExp(AExp(VInt 1, "+", Id "x"), "+", VInt 1), VInt 3) = AExp(AExp(VInt 1, "+", VInt 3), "+", VInt 1);

substitui(Id "x", AExp(AExp(VInt 1, "+", Id "x"), "+", VInt 1), VInt 3) = AExp(AExp(VInt 1, "+", VInt 3), "+", VInt 1);

substituiBExp(Id "x", BExp(Id "x", "And", VBool true), VBool false) = BExp(VBool false, "And", VBool true);

(*(print ("AExp(VInt "^Int.toString(v1)^" \"+\" VInt "^Int.toString(v2)^")\n"); VInt(v1 + v2))*)

(*******************************************************)
(******             funcoes auxiliares            ******)
fun printList ([], _) = print("]\n\n\n")
    | printList ((x::xs), true) = (print ("["^Int.toString(x)^", "); printList(xs, false))
    | printList ((x::nil), _) = (print (Int.toString(x)); printList(nil, false))
    | printList ((x::xs), false) = (print (Int.toString(x)^", "); printList(xs, false));


(*******************************************************)

fun apriori(VInt v) = VInt(v)
  | apriori(VBool b) = VBool(b)
  
  | apriori(BExp(VBool b1, "And", VBool b2)) = 
    (print ("BEXP(VBool "^Bool.toString(b1)^" \"And\" VBool "^Bool.toString(b2)^")\n"); VBool(b1 andalso b2))
  
  | apriori(BExp(VBool b1, "Or", VBool b2)) = 
    (print ("BEXP(VBool "^Bool.toString(b1)^" \"Or\" VBool "^Bool.toString(b2)^")\n"); VBool(b1 orelse b2))
  
  | apriori(BExp(VBool b1, "==", VBool b2)) = 
    (print ("BEXP(VBool "^Bool.toString(b1)^" \"==\" VBool "^Bool.toString(b2)^")\n"); VBool(b1 = b2))
  
  | apriori(BExp(VInt v1, "==", VInt v2)) = 
    (print ("BEXP(VInt "^Int.toString(v1)^" \"==\" VInt "^Int.toString(v2)^")\n"); VBool(v1 = v2))
      
  | apriori(BExp(e1, operacao, e2)) = apriori(BExp(apriori(e1), operacao, apriori(e2)))
  
  | apriori(AExp(VInt v1, "+", VInt v2)) = 
    (print ("AExp(VInt "^Int.toString(v1)^" \"+\" VInt "^Int.toString(v2)^")\n"); VInt(v1 + v2))
  
  | apriori(AExp(VInt v1, "-", VInt v2)) =
    (print ("AExp(VInt "^Int.toString(v1)^" \"-\" VInt "^Int.toString(v2)^")\n"); VInt(v1 - v2))
  
  | apriori(AExp(e1, operacao, e2)) = apriori(AExp(apriori(e1), operacao, apriori(e2)))
  
  | apriori(If("if", bexp, exp1, exp2 )) = if apriori(bexp) = VBool true then apriori(exp1)
                                                                         else apriori(exp2)
  | apriori(Fun("fun", [], exp, [])) = apriori(exp)
  
  | apriori(Fun("fun", id::id_list, exp, valor::valor_list)) = 
    apriori(Fun("fun", id_list, substitui(id, exp, valor), valor_list))
  
  | apriori(any) = raise Indefinido;
  
  
(*******************************************************)
(******             bateria de testes             ******)
  
(* operacoes aritmeticas *)
apriori(AExp(AExp(VInt 1, "+", VInt 3), "+", VInt 6)) = VInt 10;
apriori(AExp(AExp(VInt 1, "+", VInt 3), "-", VInt 6)) = VInt ~2;
apriori(AExp(VInt 1, "+", VInt 3)) = VInt(4);
apriori(AExp(VInt 3, "-", VInt 3)) = VInt(0);

(* operacoes booleanas *)
apriori(BExp(VBool true, "And", VBool true)) = VBool(true);
apriori(BExp(VBool false, "And", VBool true)) = VBool(false);
apriori(BExp(BExp(VBool false, "And", VBool true), "And", VBool false)) = VBool(false);
apriori(BExp(VBool true, "Or", VBool true)) = VBool(true);
apriori(BExp(VBool true, "Or", VBool false)) = VBool(true);
apriori(BExp(VBool false, "Or", VBool false)) = VBool(false);
apriori(BExp(VBool false, "Or", VBool false)) = VBool(false);
apriori(BExp(VBool false, "==", VBool false)) = VBool(true);
apriori(BExp(VBool true, "==", VBool false)) = VBool(false);

apriori(BExp(VInt 1, "==", VInt 2)) = VBool(false);
apriori(BExp(VInt 2, "==", VInt 2)) = VBool(true);

(*  if then else *)
apriori(If("if", VBool true, VInt 100, VInt 200)) = VInt 100;
apriori(If("if", VBool false, VInt 100, VInt 200)) = VInt 200;
apriori(If("if", BExp(VBool true, "Or", VBool false), VInt 100, VInt 200)) = VInt 100;
apriori(If("if", BExp(VBool true, "Or", VBool false), AExp(VInt 50, "+", VInt 50), VInt 200)) = VInt 100;

(* funcoes *)
apriori(Fun("fun", [], AExp (VInt 1, "+", VInt 2), [])) = VInt 3;
apriori(Fun("fun", [Id "x", Id "y"], AExp (Id "x", "+", Id "y"), [VInt 2, VInt 3])) = VInt 5;
apriori(Fun(("fun", [Id "x", Id "y"], AExp (AExp (Id "x", "+", Id "y"), "+", VInt 10), [VInt 2, VInt 3])));
(* apriori(Fun("fun", [Id "x", Id "y"], BExp (Id "x", "And", Id "y"), [VBool false, VBool true])); *)

(* substituiAExp(Id "x", AExp (AExp (Id "x", "+", Id "y"), "+", VInt 10), VInt 10); *)
(* apriori(AExp (AExp (VInt 2, "+", VInt 1), "+", VInt 10)); *)
















