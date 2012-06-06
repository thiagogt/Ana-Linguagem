datatype valor = ValorBooleano of bool
  | ValorInteiro of int;

datatype expressao = Valor of valor
  | ExpBinaria of expressao * string * expressao
  | IfThenElse of string * expressao * expressao * expressao;
  
exception Indefinido;
  
fun SOMA(Valor(ValorInteiro(x)), Valor(ValorInteiro(y))) = Valor(ValorInteiro(x+y))
  | SOMA(_, _) = raise Indefinido;

fun SUBTRAI(Valor(ValorInteiro(x)), Valor(ValorInteiro(y))) = Valor(ValorInteiro(x-y))
  | SUBTRAI(_, _) = raise Indefinido;
  
fun AND(Valor(ValorBooleano(x)), Valor(ValorBooleano(y))) = Valor(ValorBooleano(x andalso y))
  | AND(_, _) = raise Indefinido;
  
fun OR(Valor(ValorBooleano(x)), Valor(ValorBooleano(y))) = Valor(ValorBooleano(x orelse y))
  | OR(_, _) = raise Indefinido;

fun IGUAL(Valor(ValorBooleano(x)), Valor(ValorBooleano(y))) = Valor(ValorBooleano(x = y))
  | IGUAL(Valor(ValorInteiro(x)), Valor(ValorInteiro(y))) = Valor(ValorBooleano(x = y))
  | IGUAL(_, _) = raise Indefinido;
  
fun apriori(Valor(ValorInteiro(v))) = Valor(ValorInteiro(v))
  | apriori(ExpBinaria(x, "+", y)) = SOMA(apriori(x), apriori(y))
  | apriori(ExpBinaria(x, "-", y)) = SUBTRAI(apriori(x), apriori(y))
  | apriori(Valor(ValorBooleano(b))) = Valor(ValorBooleano(b))
  | apriori(ExpBinaria(x, "And", y)) = AND(apriori(x), apriori(y))
  | apriori(ExpBinaria(x, "Or", y)) = OR(apriori(x), apriori(y))
  | apriori(ExpBinaria(x, "==", y)) = IGUAL(apriori(x), apriori(y))
  | apriori(IfThenElse("if", expBool, Valor(x), Valor(y))) = 
        if (apriori(expBool) = Valor(ValorBooleano(true))) then apriori(Valor(x))
                                                             else apriori(Valor(y))
  | apriori(IfThenElse("if", expBool, ExpBinaria(x1, s1, y1), ExpBinaria(x2, s2, y2))) = 
        if (apriori(expBool) = Valor(ValorBooleano(true))) then apriori(ExpBinaria(x1, s1, y1))
                                                             else apriori(ExpBinaria(x2, s2, y2))
  | apriori(IfThenElse("if", expBool, Valor(x), ExpBinaria(x2, s2, y2))) = 
        if (apriori(expBool) = Valor(ValorBooleano(true))) then apriori(Valor(x))
                                                             else apriori(ExpBinaria(x2, s2, y2))
  | apriori(IfThenElse("if", expBool, ExpBinaria(x2, s2, y2), Valor(x))) = 
        if (apriori(expBool) = Valor(ValorBooleano(true))) then apriori(ExpBinaria(x2, s2, y2))
                                                             else apriori(Valor(x))
  | apriori(_) = raise Indefinido;
      
  
(* Testes para a funcao apriori todos eles devem retornar true *)
(* 2 + 1 = 3 *)
apriori(ExpBinaria(Valor(ValorInteiro(1)), "+", Valor(ValorInteiro(2)))) = Valor(ValorInteiro(3)); 

(* 1 - 2 = -1 *)
apriori(ExpBinaria(Valor(ValorInteiro(1)), "-", Valor(ValorInteiro(2)))) = Valor(ValorInteiro(~1)); 

(* (1 - 2) + 3 = 2 *)
apriori(
    ExpBinaria(
        ExpBinaria(Valor(ValorInteiro(1)), "-", Valor(ValorInteiro(2))), 
        "+", Valor(ValorInteiro(3))
    )
) = Valor(ValorInteiro(2)); 

(* true && true = true *)
apriori(ExpBinaria(Valor(ValorBooleano(true)), "And", Valor(ValorBooleano(true)))) = Valor(ValorBooleano(true));

(* false && true = false *)
apriori(ExpBinaria(Valor(ValorBooleano(false)), "And", Valor(ValorBooleano(true)))) = Valor(ValorBooleano(false));

(* false || true = true *)
apriori(ExpBinaria(Valor(ValorBooleano(false)), "Or", Valor(ValorBooleano(true)))) = Valor(ValorBooleano(true));

(* false || false = false *)
apriori(ExpBinaria(Valor(ValorBooleano(false)), "Or", Valor(ValorBooleano(false)))) = Valor(ValorBooleano(false));

(* false == false = true *)
apriori(ExpBinaria(Valor(ValorBooleano(false)), "==", Valor(ValorBooleano(false)))) = Valor(ValorBooleano(true));

(* 2 == 2 = true *)
apriori(ExpBinaria(Valor(ValorInteiro(2)), "==", Valor(ValorInteiro(2)))) = Valor(ValorBooleano(true));

(* 2 == 3 = false *)
apriori(ExpBinaria(Valor(ValorInteiro(2)), "==", Valor(ValorInteiro(3)))) = Valor(ValorBooleano(false));

(* if (false) 1 else 2  retorna 2 *)
apriori(IfThenElse("if", Valor(ValorBooleano(false)), Valor(ValorInteiro(1)), Valor(ValorInteiro(2)))) = Valor(ValorInteiro(2));

(* if (true) 1 else 2  retorna 1 *)
apriori(IfThenElse("if", Valor(ValorBooleano(true)), Valor(ValorInteiro(1)), Valor(ValorInteiro(2)))) = Valor(ValorInteiro(1));

(* if (false || false) 1+10 else 1+2  retorna 3 *)
apriori(
    IfThenElse("if", ExpBinaria(Valor(ValorBooleano(false)), "Or", Valor(ValorBooleano(false))), 
        ExpBinaria(Valor(ValorInteiro(1)), "+", Valor(ValorInteiro(10))), 
        ExpBinaria(Valor(ValorInteiro(1)), "+", Valor(ValorInteiro(2)))
    )
)= Valor(ValorInteiro(3));

(* if (true || false) 1 else 2  retorna 1 *)        
apriori(
    IfThenElse("if", ExpBinaria(Valor(ValorBooleano(true)), "Or", Valor(ValorBooleano(false))),
        Valor(ValorInteiro(1)),
        Valor(ValorInteiro(2))
    )
) = Valor(ValorInteiro(1));

(* if (true || false) 2 else 1+10  retorna 2 *)
apriori(
    IfThenElse("if", ExpBinaria(Valor(ValorBooleano(true)), "Or", Valor(ValorBooleano(false))),
        Valor(ValorInteiro(2)),
        ExpBinaria(Valor(ValorInteiro(1)), "+", Valor(ValorInteiro(10)))
    )
) = Valor(ValorInteiro(2));

(* if (true && true) 1+10 else 1  retorna 11 *)
apriori(
    IfThenElse("if", ExpBinaria(Valor(ValorBooleano(true)), "Or", Valor(ValorBooleano(false))),
        ExpBinaria(Valor(ValorInteiro(1)), "+", Valor(ValorInteiro(10))),
        Valor(ValorInteiro(2))
    )
) = Valor(ValorInteiro(11));

