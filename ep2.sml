exception Indefinido;

datatype Expression =
	  VInt of int
	| VBool of bool
	| Id of string
	| AExp of Expression * string * Expression
	| BExp of Expression * string * Expression
	| If of string * Expression * Expression * Expression
	| Fun of string * Expression list * Expression * Expression list;

	
fun apriori(VInt v) = VInt(v)
  | apriori(VBool b) = VBool(b)
  | apriori(BExp(VBool b1, "And", VBool b2)) = VBool(b1 andalso b2)
  | apriori(BExp(VBool b1, "Or", VBool b2)) = VBool(b1 orelse b2)
  | apriori(BExp(VBool b1, "==", VBool b2)) = VBool(b1 = b2)
  | apriori(BExp(VInt v1, "==", VInt v2)) = VBool(v1 = v2)
  | apriori(BExp(e1, operacao, e2)) = apriori(BExp(apriori(e1), operacao, apriori(e2)))
  | apriori(AExp(VInt v1, "+", VInt v2)) = VInt(v1 + v2)
  | apriori(AExp(VInt v1, "-", VInt v2)) = VInt(v1 - v2)
  | apriori(AExp(e1, operacao, e2)) = apriori(AExp(apriori(e1), operacao, apriori(e2)))
  | apriori(If("if", bexp, exp1, exp2 )) = if apriori(bexp) = VBool true then apriori(exp1)
                                                                         else apriori(exp2)
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

Fun(("fun"), [Id "x", Id "y"], AExp (Id "x", "+", Id "y"), [VInt 2, VInt 3]);









