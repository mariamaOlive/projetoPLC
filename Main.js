
var cpf= [0, 5, 4, 7, 6, 1, 4, 3, 4];


function gerador(lst){
	
	var lst1=lst;
	var lst2=lst;
	var s=0;

	for(var i=10; i>1; i--){
		s=s+(i*head(lst1));
		lst1=tail(lst1);
	}

	var r1=(s*10)%11;
	if (r1==10){
		r1=0;
	}

	s=0;
	for(var i=11; i>2; i--){
		s=s+(i*head(lst2));
		lst2=tail(lst2);
	}

	s=s+(r1*2);

	var r2= (s*10)%11;
	if(r2==10){
		r2=0;
	}

	return (10*r1)+r2;
	//return lst;
}


dig=gerador(cpf);


/*function smaller(pivot, lst) {

	var acc;

	if(lst == []) {
		return [];
	}

	if(head(lst) <= pivot) {
		acc = concat([head(lst)],smaller(pivot, tail(lst)));
	}else{
		acc= concat([], smaller(pivot, tail(lst),acc));
	}

	return acc;
} 

function bigger(pivot, lst){
	var acc;

	if(lst == []) {
		return [];
	}

	if(head(lst) > pivot) {
		acc = concat([head(lst)],bigger(pivot, tail(lst)));
	}else{
		acc= concat([], bigger(pivot, tail(lst),acc));
	}

	return acc;
}

function qs(lst) {
	var pivot=head(lst);

	if(lst==[]){
		return [];
	}

	lst=tail(lst);
	var s=smaller(pivot,lst);
	var b=bigger(pivot,lst);

	var r1=qs(s);
	var r2=qs(b);
	var r=concat(r1,[pivot]);
	r=concat(r,r2);
	return r; 

}

retorno=qs([100, 5, 7, 50, 34, 60, 16]);

var cpf= [0, 5, 4, 6, 6, 0, 4, 9, 4];


function gerador(lst){
	
	var lst1=lst;
	var lst2=lst;
	var s=0;

	for(i=10; i>1; i=i-1){
		s=s+(i*head(lst1));
		lst=tail(lst1);
	}

	var r1=(s*10)%11;
	if (r1==10){
		r1=0;
	}

	s=0;
	for(i=11; i>3; i=i-1){
		s=s+(i*head(lst2));
		lst=tail(lst2);
	}

	s=s+(r1*2);

	var r2= (s*10)%11;

	return ;
	//return lst;
}


dig=gerador(cpf);

/*var array1 = [100, 5, 7, 50, 34, 60, 16];



//v=foo(array1);



function foo(a) {
	function foo2() {
		var casa = 1;
		return casa;
	}
	var c = foo2();
	return c;
}

foo(10);
*/

/* FUNCAO LEN
function len(lst) {
	if(lst == []) {
		return 0;
	} else {
		return 1 + len(tail(lst));
	}
}

len(concat(["a", "b"], ["c"]));
*/

/*
var l1 = [1, 2, 3];
var l2 = [4, 5, 6];

v=concat([], []);
*/

/*


for(var i = 0; i < 10; i++) {
	var c = 20;
	
	for(j=0; j<10; j=j+1){

		if(j== 2) {
		break;
		}
	}

	if (i==4){
		break;
	}
}

var l1 = [1, 2, 3];
var l2 = [4, 5, 6];

head([1, 2, 3]);
var d = 30;
for(var i = 0; i < 10; i++) {
	var c = 20;
	if(i == 2) {
		break;
	}
}
*/
/*
a=4+3;

var leel=1;


function foo(x,b) {
	var c,e;
	q=8;
	var a=100;
	if(q==8){
		var v;
		
		leel=b;

		if(a==100){
		var a1=0;
		}else{
			var a1=0;
		}
	}

	for(var i = 0; i < 10; i++) {
	var c = 20;
	var caramba = 20;
		if(i == 2) {
			casa= 5;
			if(casa==5){
				casa2=0;
				var casinha=7;
			}
		}

		flat=0;
		j=0;
		for(; j<10;j++){
			var ap=flat+1;
		}
	}

	return a+1;
	var d=2;
}


function foo2(a){
		haha=3;
		return a;
}

foo2(3);
var chec= foo(3 ,3);
q=10;


function foo(x,b) {
	var c,e;
	q=8;
	a=100;
	if(q==8){
		var v;
		casa= 5;
		leel=b;

		if(a==100){
		var a1=0;
		}else{
			var a1=0;
		}
	}

	
	function foo2(a){
		haha=3;
		return a;
	}

	
	
	return foo2(3);
	var d=2;
}




foo2(3);
foo(3 ,3);
q=10;
*
//head(['a', 'b', 'c']);
head([1, 2, 3]);
//head(['a', 'b', 'c']);
/*

if(a==100){
		var a1=0;
	}else{
		var a1=0;
	}

	for(var i=0; i<10; i=i+1){
		var q2=1;
	}


//ver http://www.guj.com.br/java/171982-qual-a-diferenca-i-e-i
var i = 8;
var j;
var k;

//j = ++i; // j == 9, i == 9
//k = i++; // k == 8, i == 9
*/

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
//concat [1, 2, 3] [1, 2, 3];



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
