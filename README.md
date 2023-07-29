# The EcksDee Programming Language
	Created by Jesse A. Jones (KesseNones)
	Inspired by Forth

## Introduction:
		The EcksDee programming language started as an interpretor 
	for a really simple Forth dialect in Programming Language Design. 
	From there, I decided to make my own programming language 
	once I learned the basics. From there I added variables, 
	and changed function syntax lightly, 
	making it officially its own programming language. 
	Ever since more features have been added 
	to make it even more its own language. The main features 
	of the language are detailed below.

## Mechanics:
### Data Types:
		This language contains multiple data types that are pushed onto the stack. 
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
		To push a value of type Float onto the stack one just pushes a number ending in a decimal, such as
		4.2, 3.14, 6.28, 1.42, 2.0, etc. If the number is whole and one wants 
		to push it as a Float then just push the whole number and .0 at the end, 
		such as 42.0 which does indeed push the number as a Float to the stack.
		
		-Double: A data type very similar to the Float type except 
		it's built off a 64 bit precision floating point number 
		which allows greater decimal precision and allows it to be significantly larger.
		To push a value of type Double onto the stack one does it almost like the Float data type except they also add a d at the end, the d standing for Double. 
		For example: 42.0d, 3.14159265358979323d, 2.71828d, etc. 
		
		-Char: A single UTF-8 encoded character that makes up a piece of a string. 
		This data type isn't that useful but it exists. 
		To push a Char type to the stack, one just pushes a single character surrounded by apostraphies,
		like so: 'a', 'b', 'c', '9', '⭐', etc. 
		
		-String: Like the Char data type but a collection of them. 
		The String data type is a collection of Chars. This data type is most commonly used 
		in IO but it can have other uses too. To push a String type onto the stack, one must push at least two double quotes, ideally with other stuff in them, for example: "", "foo", "bar", 
		"This is a sentence", "Wow!", "Put whatever text you want in here!", etc. 
		
		-Boolean: Contains the value of either True or False. Used in if statements, loops, 
		and logical operations. To push a boolean to the stack, one pushes the word true, True, False, 
		or false to the stack. The capitalization of the first letter doesn't matter, both code to the same
		value. Booleans are most often produced as a result of logical expressions discussed later.
		
		-List: This data type is incredibly versatile. It is a free collection of data. 
		Where a string can only be a collection of Chars the List is not restricted by type. As a result,
		the list can store multiple data types, including itself. Construcing a proper list 
		is more complicated but starting a list is easy. All one has to do is to push the empty list to the
		stack like so: []. From there values can be added to this empty list 
		using the push or fpush operator which will be discussed later.
### Operators:
		X
### While Loops:
		X
### Conditionals:
		X
### Functions:
		X
### Variables:
		X

## Conclusion:
		X
