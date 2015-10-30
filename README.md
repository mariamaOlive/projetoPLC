OBS:
quando o breake é usando, por algum motivo o escopo local não funciona dentro do for

#Mini interpretador JS

Ver aqruivo 'consideracoes.txt' para algumas explicações

## Menos (Operador unário de -) [FEITO]

## Funções [CHECAR NA POSSIBILIDADE DE SEPARAR A MEMÓRIA]

###def:
function mult(p1, p2) {
    return p1 * p2;
}

###chamada:

var x = mult(10, 20)
ou
mult(10, 20)

## Funções Recursivas

###def:

function fib(n) {
    if(n <= 2) {
        return 1;
    } else {
        return this.recursive(n - 1) + this.recursive(n - 2);
    }
}

## Variaveis Locais - FUNCTION E FOR

var carName = " Volvo";

## Variaveis Globais

var carName = "Volvo";

funtion foo() {
   carName = "Uno";
}

## Comentários  -- Falar com o monitor, pois aparentemente já está implementado

//

/*   */

## for [FEITO] + break [FEITO]

for (i = 0; i < 10; i++) {
    if (i === 3) { break; }
    text += "The number is " + i;
}


## if the [FEITO]
if (hour < 18) {
    greeting = "Good day";
}

## if then else [FEITO]


if (hour < 18) {
    greeting = "Good day";
} else {
    greeting = "Good evening";
}

## i++; ++i ; i--; --i

-- checar comportamento do parser com só if e if the else

## Listas (similares a haskell)
N/A

## Funções head, tail e concat
N/A

##Perguntas para o monitor
-Como devemos imprimir a função quando se imprime tudo que está no environment
-Perguntar sobre listas
--Duvida sobre parametros de função com tamanhos diferentes quando declarado e chamado
--Duvida: inicializar ou nao a variavel de escorpo local de mesmo nome que globa (function e for)
