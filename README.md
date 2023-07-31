# The EcksDee Programming Language
	-Created by Jesse A. Jones (KesseNones)
	-Inspired by Forth

## Introduction:
	-The EcksDee programming language started as an interpretor 
	for a really simple Forth dialect in Programming Language Design. 
	From there, I decided to make my own programming language once I learned the basics. 
	From there I added variables, and changed function syntax lightly, 
	making it officially its own programming language. Ever since more features have been added 
	to make it even more its own language. The main features 
	of the language are detailed below.

## Mechanics:
### Stack Based Approach:
	-Since EcksDee is based on Forth, it's stack based. This means that the language is generally composed 
	of things pushed to the stack, and things that operate on those things pushed 
	onto the stack known as operators. For instance, to add two numbers, 
	one pushes a number, another number, and then uses the add operator, like so: a b + 
	This means that a is pushed onto the stack and then b is pushed onto the stack, 
	yielding a stack, displayed top being the rightmost item, like this: a b.
	The operator + then pops b and a and adds them together and pushes the result, 
	yielding a stack with just result c.
	-Following this explanation is a section on the different types of data that can be pushed to the stack.
### Data Types:
	-This language contains multiple data types that are pushed onto the stack. 
	The data types currently are:
	
	-Integer: An integer value that depends on system architecture for size. 
	On most modern machines it'll range from -2^63 to 2^63 - 1 as a signed 64 bit integer. 
	Pushing values onto the stack that are of type Integer is easy: 
	just push a whole number, i.e. 42, 666, 2319, etc. 
	This pushes values of type Integer onto the stack.  

	-BigInteger: This is like the previous data type except it has no upper or lower bound limits. 
	It can keep growing in either direction for as much space as is able to be allocated to it. 
	Pushing values onto the stack of type BigInteger is very similar 
	to type Integer as one would expect except that one has to add a little b at the end. 
	The b being for BigInteger as one would expect. 
	For example the data 42b would push the value 42 as a BigInteger onto the stack.
	
	-Float: This is a type capable of expressing floating point values with 32 bits of precision. 
	As the name indicates this is used for floating point values of not super high precision. 
	To push a value of type Float onto the stack one just pushes a number ending 
	in a decimal, such as 4.2, 3.14, 6.28, 1.42, 2.0, etc. If the number is whole 
	and one wants to push it as a Float then just push the whole number and .0 at the end, 
	such as 42.0 which does indeed push the number as a Float to the stack.
	
	-Double: A data type very similar to the Float type except 
	it's built off a 64 bit precision floating point number 
	which allows greater decimal precision and allows it to be significantly larger.
	To push a value of type Double onto the stack one does it almost 
	like the Float data type except they also add a d at the end, the d standing for Double. 
	For example: 42.0d, 3.14159265358979323d, 2.71828d, etc. 
	
	-Char: A single UTF-8 encoded character that makes up a piece of a string. 
	This data type isn't that useful but it exists. 
	To push a Char type to the stack, one just pushes a single character surrounded 
	by apostraphies, like so: 'a', 'b', 'c', '9', '⭐', etc. 
	
	-String: Like the Char data type but a collection of them. 
	The String data type is a collection of Chars. This data type is most commonly used 
	in IO but it can have other uses too. To push a String type onto the stack, 
	one must push at least two double quotes, ideally 
	with other stuff in them, for example: "", "foo", "bar", "This is a sentence", "Wow!", 
	"Put whatever text you want in here!", etc. 
	
	-Boolean: Contains the value of either True or False. Used in if statements, loops, 
	and logical operations. To push a boolean to the stack, 
	one pushes the word true, True, False, or false to the stack. 
	The capitalization of the first letter doesn't matter, both code to the same value. 
	Booleans are most often produced as a result of logical expressions discussed later.
	
	-List: This data type is incredibly versatile. It is a free collection of data. 
	Where a string can only be a collection of Chars the List is not restricted by type. 
	As a result, the list can store multiple data types, including itself. 
	Construcing a proper list is more complicated but starting a list is easy. 
	All one has to do is to push the empty list to the
	stack like so: []. From there values can be added to this empty list 
	using the push or fpush operator which will be discussed later.
### Operators:
	-Now that all the types of data have been covered that can be pushed onto the stack, 
	now it's time to cover all the basic operators that can be used on the data.
	When displaying the result of an example program or usage of the operator, 
	the stack will be displayed in a left to right format 
	where the top of the stack is the right-most item. 
	In the actual language when a program finishes running, the stack is printed vertically 
	in a top-down order with the top item of the stack being at the bottom of the print. 

	-At present, the current operators are: 

	- +: Adds the top two elements of a stack together. 
	For example, given a stack of numbers: b a, it pops b and a 
	and adds them together and pushes number c, leaving stack: c. In the case of numbers a and b, 
	their types have to match and the valid types for + are: Integer, BigInteger, Float, Double, and Boolean.
	For the first four types mentioned, + works as an adder as expected. For type Boolean, 
	the + works like a logical OR, which means the result of adding two Booleans is true 
	as long as at least one is True.
	Example Program: 
		2 2 + 2b 3b + 3.0 7.0 + 21.0d 21.2d + False True +
	Finishes with stack: Integer 4, BigInteger 5, Float 10.0, Double 42.2, Boolean True

	- -: Subtracts a number at the top of the stack from a number at the second to top of the stack. 
	The valid types for the subtraction operator - are: Integer, BigInteger, Float, and Double. 
	As with the + operator, the two operand types need to match.
	Example Program:
		5 3 - 44b 2b - 3.28 0.14 - 2.71828d 0.7d - 
	Which yields stack: Integer 2, BigInteger 42, Float 3.1399999, Double 2.01828

	- *: Multiplies the top two elements of the stack together. As with + and - , 
	the types must match and the valid types for * are: Integer, BigInteger, Float, Double, and Boolean.
	For the first four types * acts as multiplication. For Booleans it acts as a logical AND.
	Example Program: 
		2 2 * 2b 3b * 3.0 7.0 * 21.0d 21.2d * False True *
	Ending Stack: Integer 4, BigInteger 6, Float 21.0, Double 445.2, Boolean False

	- /: Divides second to top element of stack by top element. As stated before, 
	the types have to match for both operands. The valid types for the division operator / are:
	Integer, BigInteger, Float, and Double.
	Example Program:
		2 2 / 3b 2b / 3.0 2.0 / 355.0d 113.0d / 
	Final Stack: Integer 1, BigInteger 1, Float 1.5, Double 3.1415929203539825

	- swap: Swaps the top two items on top of the stack. If the stack is empty 
	or only one item is on the stack, nothing happens. In terms of types, any two items 
	of any data type can be swapped on the stack. Generally, given stack: x, y, 
	the resulting stack from using swap is: y x.
	Example Program:
		"foo" "bar" swap
	Final Stack: String "bar", String "foo"

	- drop: Easily the most common operator. Given stack x y z, drop removes the top item from the stack. 
	In this case, applying drop to the stack gives x y, removing z.
	Example Program:
		"foo" 42 1.414 666 drop
	Final Stack: String "foo", Integer 42, Float 1.414

	- rot: Rotates the top three items of the stack to the right. 
	If only two operands are on stack it acts like swap operator, 
	and if only one or no items exist, nothing happens. The operator rot can work with any types involved.
	Generally, given stack: x, y, z, using rot makes it z, x, y because it gets rotated to the right.
	Example Program:
		"foo" "bar" "baz" rot
	Final Stack: String "baz", String "foo", String "bar"

	- dup: Pushes a duplicate of the top element of the stack or does nothing if stack is empty. 
	Like swap, drop, and rot, dup works with any type on the stack. In general, given a stack: x, 
	using dup results in the stack: x x' where x' is the same as x.
	Example Program:
		1 2 3 dup
	Final Stack: Integer 1, Integer 2, Integer 3, Integer 3

	- ==: Determines if the top two elements of a stack are equal to each other. 
	Pops top two items and pushes a Boolean based on equality. 
	For example, given stack: x y, x and y are popped and checked if x == y. 
	If so, True is pushed and False otherwise. 
	For a valid comparison to be made, the types of x and y must match, otherwise an error is thrown.
	Example Program:
		1 1 == false true == 2.0 2.1 == 1.0d 1.42d == "foo" "bar" == "foo" "foo" ==
	Final Stack: Boolean True, Boolean False, Boolean False, Boolean False, Boolean False, Boolean True

	- /=: Exactly the same as == except it pushes the opposite Boolean to the stack.
	The types of the operands also need to match for this to work.
	Example Program:
		1 1 /= false true /= 2.0 2.1 /= 1.0d 1.42d /= "foo" "bar" /= "foo" "foo" /=
	Final Stack: Boolean False, Boolean True, Boolean True, Boolean True, Boolean True, Boolean False

	- >: Determines if the second to top element is greater than the top element of the stack. 
	Pushes Boolean based on the truth value of the expression. If the types don't match, an error occurs.
	Generally, given stack: x y, it pops x and y to see if x > y.
	Example Program:
		6 9 > 9.9 9.5 > 148494484948484b -485b > 3.141592653589793d 2.71828d > "foo" "bar" > "bar" "baz" >
	Final Stack: Boolean False, Boolean True, Boolean True, Boolean True, Boolean True, Boolean False

	- <: Acts like the greater than (>) operator but checks to see 
	if the second to top is LESS than the top, rather than greater.
	Types of the two comparison operands need to match.
	Example Program:
		6 9 < 9.9 9.5 < 148494484948484b -485b < 3.141592653589793d 2.71828d < "foo" "bar" < "bar" "baz" <
	Final Stack: Boolean True, Boolean False, Boolean False, Boolean False, Boolean False, Boolean True

	- >=: Examines if second to top element is greater than or equal to the top element of the stack.
	Types need to match. Given stack: x y, operation x >= y is performed. 
	Example Program:
		"foo" "foo" >= 99 100 >= 3.14 2.718 >= 69.420d 420.69d >= 'c' 'a' >= true true >= 
	Final Stack: Boolean True, Boolean False, Boolean True, Boolean False, Foolean True, Boolean True

	- <=: Acts like the greater than equal to operator (>=) but has generally opposite results 
	by checking if the second to top of the stack is LESS than or equal to the top.
	Types of operands need to match.
	Example Program:
		"foo" "foo" <= 99 100 <= 3.14 2.718 <= 69.420d 420.69d <= 'c' 'a' <= true true <= 
	Final Stack: Boolean True, Boolean True, Boolean False, Boolean True, Boolean False, Boolean True 

	- %: Given stack: x y, x and y are popped and x % y is performed with the result pushed on the stack. 
	In other words, the second to top item of the stack is modded by the top item of the stack.
	The types of x and y have to match and the valid types are BigInteger and Integer. 
	Using non-matching types or types that aren't the two valid ones, an error is trown.
	Example Program:
		23 2 % 16 2 % 2023b 4b % 2020b 4b % 1969b 100b % 22b 10b % 40999 400 %
	Ending Stack: Integer 1, Integer 0, BigInteger 3, BigInteger 0, BigInteger 69, BigInteger 2, 
	Integer 199

	- and: Given stack: x y, where x and y are of type Boolean, the operator and pops x and y 
	and performs a logical AND operation on them, pushing the Boolean result to the stack. True is only
	pushed if x and y are both True.
	Example Program:
		true true and true false and false true and false false and
	Final Stack: Boolean True, Boolean False, Boolean False, Boolean False

	- or: Given stack: x y, where x and y are of type Boolean, the operator or pops x and y 
	and performs a logical OR operation on them, pushing the Boolean result to the stack.
	False is only pushed when x and y are both False.
	Example Program:
		true true or true false or false true or false false or
	Final Stack: Boolean True, Boolean True, Boolean True, Boolean False

	- xor: Given stack: x y, where x and y are of type Boolean, the operator xor pops x and y 
	and performs a logical XOR (Exclusive OR) operation on them, 
	pushing the Boolean result to the stack. True is pushed when exactly x or y is True 
	and exactly y or x is False.
	Example Program:
		true true xor true false xor false true xor false false xor
	Final Stack: Boolean False, Boolean True, Boolean True, Boolean False

	- not: Given stack: x, where x is of type Boolean, the operator not pops x and peforms a logical NOT
	operation on x, pushing the opposite boolean to the stack.
	Example Program:
		false not true not 
	Final Stack: Boolean True, Boolean False

	- pow: Given stack: x y, where x and y have matching types of valid types: Float and Double, x and y
	are popped and x is raised to the power of y, yielding an exponent result that's pushed to the stack.
	Example Program:
		2.0 10.0 pow 2.71828d 3.14159265358979323d pow 2.0 0.5 pow 9.0 0.5 pow
	Final Stack: Float 1024.0, Double 23.14064373189989, Float 1.4142135, Float 3.0

	- ++: Given stack: x y, where x and y are of type String or List, x and y are popped and concatenated
	together, with the result being pushed onto the stack. 
	Example Program:
		"foo " "bar" ++ [] 1 p 2 p 3 p [] 4 p 5 p 6 p ++
	Final Stack: String "foo bar", List [Integer 1, Integer 2, Integer 3, Integer 4, Integer 5, Integer 6]
	How the p aka push operator works is discussed below.

	- push: Given stack: x y, where x is of type String or List, and y is of type Char for a String 
	and any type for a List, x and y are popped and x is pushed into x using the push aka p operator. 
	This operator is how lists are built in EcksDee and can be how extra characters are added 
	to strings. The operator push and p are synonymous. Having a shortened version 
	of push just makes it easier to manually build a list in code.
	Example Program:
		[] "foo" push "bar" p "baz" p [] 2 push 4 push 6 push 8 push "Pushed Char: " 'c' push
		[] "lists" p "can" p 2 p "have" p 'c' p "multiple" p "types" p
	Final Stack: 
		List [String "foo", String "bar", String "baz"], 
		List [Integer 2, Integer 4, Integer 6, Integer 8], 
		String "Pushed Char: c"
		List [String "lists", String "can", Integer 2, String "have", 
			Char 'c', String "multiple", String "types"]

	- pop: Given stack: x, where x is of type String or List. Item x is popped from the stack, 
	the last item is removed from the list or string, 
	with the altered String/List pushed back on with the Char/Value pushed also.
	If the String/List is empty, nothing happens except the empty String/List is pushed back to the stack.
	Operators pop and po are equivalent.
	Example Program:
		"Hello, Worldz" pop [] 1 p 2 p 3 p 77 p pop
	Final Stack: String "Hello, World", Char 'z', List [Integer 1, Integer 2, Integer 3], Integer 77

	- fpush: Given stack: x, y where x is of type String/List and x is of type Char/Any. 
	Items x and y are popped and y is pushed to the front of x 
	with the result being pushed back to the stack. 
	Basically the same as push but acts at the front of a String/List instead of at the back.
	Operators fpush and fp are also synonymous just like with push and p. 
	Example Program: 
		[] "foo" fpush "bar" fpush "baz" fpush " is a front pushed Char" 'c' fp
	Final Stack: List [String "baz", String "bar", String "foo"], String "c is a front pushed Char"

	- fpop: Given stack: x, fpop or fpo pops an item from the front of a String or List instead 
	of the back like pop/po does. The altered String/List is then pushed back on the stack 
	and the front popped Char/Value is pushed onto the stack afterwards.
	Example Program:
		"9There was a Char that doesn't belong at the start of this sentence." fpop
		[] 666 p 1 p 2 p 3 p fpo
	Final Stack: 
		String "There was a Char that doesn't belong at the start of this sentence.",
		Char '9',
		List [Integer 1, Integer 2, Integer 3],
		Integer 666

	- index: Given stack: x y, where x is of type String/List and y is of type Integer. 
	Item y is popped and used as an index into x. If the index is valid, 
	then the Char/Value contained in x at index y is duplicated and pushed onto the stack. 
	The String/List remains unchanged. 
	Example Program:
		[] 1 p 2 p 3 p 1 index "foo bar baz" 0 index
	Final Stack: List [Integer 1, Integer 2, Integer 3], Integer 2, String "foo bar baz", Char 'f'

	- length: Given stack: x, where x is of type String/List. 
	The operator length or len, determines the length of the String/List 
	and pushes the resulting length as an Integer to the stack.
	Example Program:
		"foo bar baz" length [] 1 p 2 p 3 p "wow!" p len
	Final Stack: 
		String "foo bar baz", 
		Integer 11, 
		List [Integer 1,Integer 2,Integer 3,String "wow!"], 
		Integer 4

	- isEmpty: Given stack: x, where x is of type String/List. 
	The operator isEmpty determines if x is an empty String/List 
	and pushes a Boolean to the stack based on that check.
	Example Program:
		"" isEmpty [] isEmpty [] 1 p isEmpty "foo" isEmpty
	Final Stack: 
		String "", 
		Boolean True, 
		List [], 
		Boolean True, 
		List [Integer 1], 
		Boolean False, 
		String "foo", 
		Boolean False
 
	- clear: Given stack: x, where x is of type String/List. 
	Pops x off the stack and pushes an empty String/List, effectively clearing x.
	Example program: 
		"This string isn't empty!" clear [] 1 p 2 p clear
	Final Stack:
		String ""
		List []

	- contains: Given stack x y, where x is a String/List and y is a Char/Value. 
	Checks to see if y is contained in x and pushes Boolean to stack as a result, leaving original values.
	Example Program:
		"foo" 'f' contains
		"bar" 'z' contains
		[] 1 p 2 p 3 p 2 contains
	Final Stack:
		String "foo"
		Char 'f'
		Boolean True
		String "bar"
		Char 'z'
		Boolean False
		List [Integer 1,Integer 2,Integer 3]
		Integer 2
		Boolean True

	- changeItemAt: Given stack: x y z, where x is a List, y is type Value, and z is type Integer.
	Pops x y and z and alters item at index z in x to item y and then pushes the altered list to the stack.
	Example Program:
		[] 'f' p 'o' p 'o' p
		
		/' Demonstrates the change more easily. 
		This is a comment by the way. This will be discussed more later on. '/
		dup 
		
		'b' 0 changeItemAt
	Final Stack:
		List [Char 'f',Char 'o',Char 'o']
		List [Char 'b',Char 'o',Char 'o']

	- isWhiteSpace: Given stack: x, where x is type Char. Determines if x is a whitespace character 
	and pushes a Boolean based on the result.
	Example Program:
		' ' isWhitespace
		'g' isWhitespace
	Final Stack:
		Char ' '
		Boolean True
		Char 'g'
		Boolean False

	- cast: Given stack: x y, where x is a Value that isn't a List and y is a string. 
	The cast operator pops x and y and tries to cast x to the type spelled 
	out in the string y. If the type is valid in the String y, 
	the cast occurs and the casted value is pushed to the stack.
	The following example program is far from exhaustive in terms of castable types, 
	but it's a good example of some common uses for the cast operator.
	Example Program:
		"Your number is: "
		2.71828 81.0 pow
		"BigInteger" cast 
		"String" cast
		++

		355.0d 113.0d / 
		dup
		"Integer" cast
	Final Stack:
		String "Your number is: 150601872777831737268034534874546176"
		Double 3.1415929203539825
		Integer 3

	- printLine: Given stack: x, where x is a String. 
	The operator printLine performs an IO action which writes the String x to standard output, 
	ending it with a newline character. The stack remains unchanged but the string is printed.
	This is the out portion of IO.
	Example Program:
		"Hello, World!"
		printLine
		drop /' Dropping the string leaves an empty stack. 
		As a result, all you see when running this program is the hello world string getting printed.  '/
	Final Stack:
		 (Empty)
	Stdout:
		Hello, World!

	- readLine: Reads a String ending in a newline of input and pushes the String to the stack.
	Example Program:
		readLine
	Stdin:
		This is some input!
	Final Stack:
		String "This is some input!"
	
	-Example using full IO to illustrate that it's indeed neat:
		"Enter your name: "
		printLine drop
		readLine
		"Hello, "
		swap ++
		"!" ++
		printLine
		drop
	Running:
		Enter your name:
		Joe
		Hello, Joe!
	Final Stack:
		(Empty)

### While Loops:
	-While loops are a form of operator known as a fancy operator 
	because more is required to use it than just having some stuff 
	on a stack and then using one word to operate on the stack.

	-The general syntax of a while loop in EcksDee is: 
	while [CODE] ;
	The semicolon at the end is absolutely necessary due to how EcksDee parses while loops.
	The [CODE] block can be any code, ranging from nothing to a gigantic program.
	
	-Also note that the semi-colon has to be free floating, it can't be attached to other chunks of code.
	This is because EcksDee tokenizes largely on whitespace, so as long as you have at least one whitespace
	character on each side of the semi-colon you're good.

	-A while loop runs the specified code inside it if the top of the stack is a Boolean True.
	If the top of the stack is a Boolean False it stops running.
	
	-While loops error if the stack is empty when it comes time to have them run or run again. 
	They also error out if the top of the stack isn't a Boolean type when it comes time to run them
	or run them again. To make a loop run well, the drop operator is necessary.
	
	-Example Program:
		/' Basically just counts from 1 to 10, pushing each number onto the stack. '/
		1 
		dup
		10 <
		while 
			drop /' Removes true Boolean if loop decides to run. '/
			dup
			1 +
			dup 
			10 <
		;
		drop /' Removes remaining false Boolean when loop finishes running. '/
	-Resulting Stack:
		Integer 1
		Integer 2
		Integer 3
		Integer 4
		Integer 5
		Integer 6
		Integer 7
		Integer 8
		Integer 9
		Integer 10

	-The most common error one will face dealing with while loops is the error: 
	"While Loop error:
	Top of stack needs to be type Boolean for loop to see if it needs to run again!"
	This error means that the top of the stack isn't a Boolean type that the loop needs to determine 
	if it needs to run at all or again. To fix this, make sure 
	to push a Boolean type to the top of the stack or have it as a result of a logical expression.

	-The other error likely is the error: 
	"While Loop error: 
	No boolean value for while loop to check because stack is empty."
	This means that the stack is empty when the loop starts trying to run or tries to run again.
	To fix this, have a Boolean type on the stack before the loop starts and before it ends.

### Conditionals:
	X
### Functions:
	X
### Variables:
	X
### Comments and Whitespace Information:
	X

## How to Run:
	X

## Conclusion:
	X
