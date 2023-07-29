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
	For example, given stack: x, y, x and y are popped and checked if x == y. 
	If so, True is pushed and False otherwise.
	Example Program:
		1 1 == false true == 2.0 2.1 == 1.0 1 == "foo" "bar" == "foo" "foo" ==
	Final Stack: Boolean True, Boolean False, Boolean False, Boolean False, Boolean False, Boolean True

	- /=: Exactly the same as == except it pushes the opposite Boolean to the stack.
	Example Program:
		1 1 /= false true /= 2.0 2.1 /= 1.0 1 /= "foo" "bar" /= "foo" "foo" /=
	Final Stack: Boolean False, Boolean True, Boolean True, Boolean True, Boolean True, Boolean False

	- >: X
### While Loops:
	X
### Conditionals:
	X
### Functions:
	X
### Variables:
	X
### Comments:
	X

## Conclusion:
	X
