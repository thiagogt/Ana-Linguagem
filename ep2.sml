datatype valor = ValorBooleano of bool
  | ValorInteiro of int;

datatype expressao = Valor of valor
  | ExpBinaria of expressao * string * expressao;

exception Indefinido;
(*| valor funcao? *)
  
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
  
fun avaliaExp(Valor(ValorInteiro(v))) = Valor(ValorInteiro(v))
  | avaliaExp(ExpBinaria(x, "+", y)) = SOMA(avaliaExp(x), avaliaExp(y))
  | avaliaExp(ExpBinaria(x, "-", y)) = SUBTRAI(avaliaExp(x), avaliaExp(y))
  | avaliaExp(Valor(ValorBooleano(b))) = Valor(ValorBooleano(b))
  | avaliaExp(ExpBinaria(x, "And", y)) = AND(avaliaExp(x), avaliaExp(y))
  | avaliaExp(ExpBinaria(x, "Or", y)) = OR(avaliaExp(x), avaliaExp(y))
  | avaliaExp(ExpBinaria(x, "==", y)) = IGUAL(avaliaExp(x), avaliaExp(y))
  | avaliaExp(_) = raise Indefinido;
    
avaliaExp(ExpBinaria(Valor(ValorInteiro(1)), "+", Valor(ValorInteiro(2)))) = Valor(ValorInteiro(3));
avaliaExp(ExpBinaria(Valor(ValorInteiro(1)), "-", Valor(ValorInteiro(2)))) = Valor(ValorInteiro(~1));
avaliaExp(ExpBinaria(ExpBinaria(Valor(ValorInteiro(1)), "-", Valor(ValorInteiro(2))), "+", Valor(ValorInteiro(3)))) = Valor(ValorInteiro(2));

avaliaExp(ExpBinaria(Valor(ValorBooleano(true)), "And", Valor(ValorBooleano(true)))) = Valor(ValorBooleano(true));
avaliaExp(ExpBinaria(Valor(ValorBooleano(false)), "And", Valor(ValorBooleano(true)))) = Valor(ValorBooleano(false));
avaliaExp(ExpBinaria(Valor(ValorBooleano(false)), "Or", Valor(ValorBooleano(true)))) = Valor(ValorBooleano(true));
avaliaExp(ExpBinaria(Valor(ValorBooleano(false)), "Or", Valor(ValorBooleano(false)))) = Valor(ValorBooleano(false));
avaliaExp(ExpBinaria(Valor(ValorBooleano(false)), "==", Valor(ValorBooleano(false)))) = Valor(ValorBooleano(true));
avaliaExp(ExpBinaria(Valor(ValorInteiro(2)), "==", Valor(ValorInteiro(2)))) = Valor(ValorBooleano(true));
avaliaExp(ExpBinaria(Valor(ValorInteiro(2)), "==", Valor(ValorInteiro(3)))) = Valor(ValorBooleano(false));
