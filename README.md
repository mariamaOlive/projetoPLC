Track List:
** quando uma função não é reconhecida a "linha 98" (comentada no main.hs) retorna um erro
** head[], qual deve ser o resultado?

Pendências:
* Remover o escopo local do for

#Mini interpretador JS

Ver aqruivo 'consideracoes.txt' para algumas explicações

## Menos (Operador unário de -)
feito

## Funções
feito

###def:
function mult(p1, p2) {
    return p1 * p2;
}

###chamada:

var x = mult(10, 20)
ou
mult(10, 20)

## Funções Recursivas
feito

###def:

function fib(n) {
    if(n <= 2) {
        return 1;
    } else {
        return this.recursive(n - 1) + this.recursive(n - 2);
    }
}

## Variaveis Locais - FUNCTION
feito

var carName = " Volvo";

## Variaveis Globais
feito

var carName = "Volvo";

funtion foo() {
   carName = "Uno";
}

## Variaveis Automaticamente Globais
PENDENTE

## Comentários
feito por definição

//
/*   */

## for + break
feito

for (i = 0; i < 10; i++) {
    if (i === 3) { break; }
    text += "The number is " + i;
}


## if the
feito

if (hour < 18) {
    greeting = "Good day";
}

## if then else
feito

if (hour < 18) {
    greeting = "Good day";
} else {
    greeting = "Good evening";
}

## i++; ++i ; i--; --i
feito

PS: Testar mais:
implementado segundo http://www.guj.com.br/java/171982-qual-a-diferenca-i-e-i


## Listas (similares a haskell)
feito

## Funções head, tail e concat e len(feita na própria linguagem ñ em haskell)
Head: Em teste
Tail, concat e len: PENDENTE

## Fazer 10 exemplos de programas, ao menos 1 usando quicksort recursivo
PENDENTE
http://rosettacode.org/wiki/Sorting_algorithms/Quicksort


##Perguntas para o monitor
-Perguntar sobre listas
--Duvida: inicializar ou nao a variavel de escorpo local de mesmo nome que globa (function e for)
