datatype Expression =
	  VInt of int
	| VBool of bool
	| Id of string
	| AExp of Expression * string * Expression
	| BExp of Expression * string * Expression
	| If of string * Expression * Expression * Expression;

	
fun apriori(VInt v) = VInt(v)
  | apriori(VBool b) = VBool(b)
  | apriori(AExp(VInt v1, "+", VInt v2)) = VInt(v1 + v2)
  | apriori(AExp(VInt v1, "-", VInt v2)) = VInt(v1 - v2);
  
  
  
apriori(AExp(VInt 1, "+", VInt 3)) = VInt(4);
apriori(AExp(VInt 3, "-", VInt 3)) = VInt(0);