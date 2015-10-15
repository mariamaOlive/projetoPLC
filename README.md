#Mini interpretador JS

Ver aqruivo 'consideracoes.txt' para algumas explicações

## Funções

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

## Variaveis Locais

var carName = " Volvo";

## Variaveis Globais

var carName = "Volvo";

funtion foo() {
   carName = "Uno";
}

## Comentários

//

/*   */

## for + break

for (i = 0; i < 10; i++) {
    if (i === 3) { break; }
    text += "The number is " + i;
}

## if then else

if (hour < 18) {
    greeting = "Good day";
} else {
    greeting = "Good evening";
}

-- checar comportamento do parser com só if e if the else

## Listas (similares a haskell)
N/A

## Funções head, tail e concat
N/A
