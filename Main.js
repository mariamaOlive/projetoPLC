/*function foo(x) {
	return 10;
}

function fat(n) {
	
	if(n==0) {
		return 1;
	} else {
		return n*fat(n-1);
	}
}

function fib(n) {
	if(n <= 2) {
		return 1;
	} else {
		return fib(n-1) + fib(n-2);
	}
}

var a = fib(16);
*/
concat [1, 2, 3] [1, 2, 3];



/*
var x = 5, y = 11, b;
var b=x + y;
b=100;


for(var i=1; i<10; i=i+1){

	if(i==6){
		break;
		i=7;
	}
	var z = 400;
	b=i+b;
	break;
}
*/
/*
var a = 30;
var b=20;

for(var i = 0; i < 10; i=i+1) {
	a = 3+i;
	if(i == 3) {
		break;
	}
}*/

/*old [(x,5),(y,11),(b,16)]
new [(x,5),(y,11),(b,3),(a,1),(c,3),(d,-2)]

1. [(x,5),(y,11),(d,-2)]
1.5 [(x,5),(y,11),(d,-2),(b,16)]
2. [(x,5),(y,11),(b,16)]

old [(x,5),(y,11),(b,16)]
1. [(x,5),(y,11),(b,0), (d,3)]
2. [(x,5),(y,11),(b,0)]

old [(x,5),(y,11),(b,16)]
[(x,5),(y,11),(b,3), (a,3), (d,3)]
1. [(x,5),(y,11),(b,3), (d,3)]
2.[(x,5),(y,11),(b,3)]
*/
