(* fun resolveExpressao(exp1, operador, exp2) = resolveExpressaoAritimetica(exp1, operador, exp2); *)

(* datatype expressaoAritimetica = x of int | y of expressaoAritimetica * string * expressaoAritimetica; *)

(* (case (1,2,3) of tuple => true) *)
(*val x = 1;
(case x 
    of 1 => "nao string"
     | 2 => "string"
);*)


(*fun avaliaExpressaoAritimetica() = 
    (case e of int => e);
    *)

datatype expressaoAritm = ValorInteiro of int
  | ExpAritm of expressaoAritm * string * expressaoAritm;
  
  
datatype expressaoBool = ValorBooleano of bool
  | ExpBool of expressaoBool * string * expressaoBool;

  
fun avaliaExpAritm(ValorInteiro(v)) = v
  | avaliaExpAritm(ExpAritm(x, "+", y)) = avaliaExpAritm(x) + avaliaExpAritm(y)
  | avaliaExpAritm(ExpAritm(x, "-", y)) = avaliaExpAritm(x) - avaliaExpAritm(y);
  
  
fun avaliaExpBool(ValorBooleano(b)) = b
  | avaliaExpBool(ExpBool(b, "And", y)) = avaliaExpBool(b) andalso avaliaExpBool(y)
  | avaliaExpBool(ExpBool(b, "Or", y)) = avaliaExpBool(b) orelse avaliaExpBool(y);


avaliaExpBool(ValorBooleano(true)) = true;
avaliaExpBool(ValorBooleano(false)) = false;
avaliaExpBool(ExpBool(ValorBooleano(false), "And", ValorBooleano(true))) = false;
avaliaExpBool(ExpBool(ValorBooleano(true), "And", ValorBooleano(true))) = true;
avaliaExpBool(ExpBool(ValorBooleano(false), "And", ValorBooleano(false))) = false;

avaliaExpBool(ExpBool(ValorBooleano(false), "Or", ValorBooleano(false))) = false;
avaliaExpBool(ExpBool(ValorBooleano(true), "Or", ValorBooleano(false))) = true;
avaliaExpBool(ExpBool(ValorBooleano(true), "Or", ValorBooleano(true))) = true;

avaliaExpAritm(ExpAritm(ValorInteiro(1), "+", ValorInteiro(2))) = 3;

avaliaExpAritm(ExpAritm(ExpAritm(ValorInteiro(1), "+", ValorInteiro(2)), "-", ValorInteiro(10))) = ~7;




