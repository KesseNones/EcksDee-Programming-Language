README FOR ECKSDEE PROGRAMMING LANGUAGE:

Syntax:
	--EcksDee is a stack based language with multiple stack operations.
	--DESCRIPTION OF OPERATIONS HERE
	
	--Functions and variables also exist in this language.
	
	--To create a function, use the "func" keyword, followed by white space,
	the function name, the function body, and end it with a semicolon.
	For example: func foo 2 2 + ; is a function that pushes 2 on the stack twice,
	then performs the addition operator on them.

	--Variables have several operations. To perform a variable operation the "var" keyword 
	is needed, followed by the operation type, followed by the variable name, 
	ending with a semicolon. 
	--The valid variable operations are: mak, get, mut, and del.
	Operation mak makes a variable, get gets the value of a variable, 
	mut changes the value of a variable, and del deletes a variable, 
	allowing semilocal scope to variables.
	--Example:
	2 var mak foo ;
	var get foo ;
	1 +
	var mut foo ; drop
	var get foo ;
	+
	var del foo ;

	This example has 2 pushed on the stack, variable foo is then set as 2.
	From there, the value of foo is fetched using the get operation.
	The + operator adds the pushed 1, yielding 3. The mut operation is then used to
	mutate the variable foo to hold 3 instead of 2. The value of foo is then fetched using get.
	From there the second call to the + operator adds 2 and 3 together, getting 5.

	--VARIABLE NOTE: 
	All variables are global in scope natively. However, using the del keyword for var allows
	some local scoping for a function and the functions it calls.
