# The EcksDee Programming Language
Created by Jesse A. Jones (KesseNones) <br>
Inspired by a Forth Dialect

## Introduction
The EcksDee programming language started as an interpretor 
for a really simple Forth dialect in the Programming Language Design class I took at university. 
From there, I decided to make my own programming language once I learned the basics. 
I added variables, and changed function syntax lightly, 
making it officially its own programming language. Ever since, more features have been added 
to make it even more distinct. The main features 
of the language are detailed below in the Mechanics section.

THIS DOCUMENTATION IS STILL A WORK IN PROGRESS SO IGNORE ANY BAD GRAMMAR OR SPELLING IF YOU SEE IT. 
IT WILL LIKELY GET FIXED LATER.

## Mechanics
### Stack Based Approach
Since EcksDee is based on Forth, it's stack based. 
This means that the language is generally composed of things pushed to the stack 
and things that operate on those things pushed onto the stack known as operators. 

For instance, to add two numbers, one pushes a number, another number, 
and then uses the add operator, like so:
``` a b + ``` 
This means that ```a``` is pushed onto the stack and then ```b``` is pushed onto the stack, 
yielding a stack, displayed top being the rightmost item, like this: ```a b```.
The operator ```+``` then pops ```b``` and ```a``` and adds them together and pushes the result, 
yielding a stack with just result ```c```.

Following this explanation is a section on the different types 
of data that can be pushed to the stack.

### Data Types
This language contains multiple data types that are pushed onto the stack. 
The data types currently are:
	
#### Integer
An integer value that depends on system architecture for size. 
On most modern machines it'll range from (-2^63) to (2^63 - 1) as a signed 64 bit integer. 
Pushing values onto the stack that are of type Integer is easy: 
just push a whole number, i.e. ```42```, ```666```, ```2319```, etc. 
This pushes values of type Integer onto the stack.  

#### BigInteger
This is like the previous data type except it has no upper or lower bound limits. 
It can keep growing in either direction for as much space as is able to be allocated to it. 
Pushing values onto the stack of type BigInteger is very similar 
to type Integer as you would expect however you have 
to add a little ```b``` at the end to indicate that the type is BigInteger 
and not just Integer. For example, the data ```42b``` 
would push the value ```42``` as a BigInteger onto the stack.

#### Float 
This is a type capable of expressing floating point values with 32 bits of precision. 
As the name indicates this is used for floating point values of not super high precision. 
To push a value of type Float onto the stack one just pushes a number ending 
in a decimal, such as ```4.2```, ```3.14```, ```6.28```, ```1.42```, ```2.0```, etc. If the number is whole and you want to push it as a Float then just push the whole number and .0 at the end, 
such as ```42.0``` which does indeed push the number as a Float to the stack.

#### Double
A data type very similar to the Float type except 
it's built off a 64 bit precision floating point number 
which allows greater decimal precision and allows it to be significantly larger.
To push a value of type Double onto the stack you do it almost 
like the Float data type except you also add a ```d``` at the end, the ```d``` standing for Double. 
For example: ```42.0d```, ```3.14159265358979323d```, ```2.71828d```, etc. 

#### Char
A single UTF-8 encoded character that makes up a piece of a string. 
To push a Char type to the stack, you just push a single character surrounded 
by apostraphies for examples: ```'a'```, ```'b'```, ```'c'```, ```'9'```, ```'⭐'```, etc. 

#### String
Like the Char data type but a collection of them. 
The String data type is a collection of Chars. This data type is most commonly used 
in IO but it can have other uses too. To push a String type onto the stack, 
you must push at least two double quotes, ideally 
with other stuff in them, for example: ```""```, ```"foo"```, ```"bar"```, ```"This is a sentence", "Wow!"```, 
```"Put whatever text you want in here!"```, etc. 

#### Boolean
Contains the value of either True or False. Used in if statements, loops, 
and logical operations. To push a boolean to the stack, 
you push the word ```true```, ```True```, ```False```, or ```false``` to the stack. 
The capitalization of the first letter doesn't matter, both code to the same value. 
Booleans are most often produced as a result of logical expressions discussed later.

#### List
This data type is incredibly versatile. It is a free collection of data. 
Where a string can only be a collection of Chars the List is not restricted by type. 
As a result, the list can store multiple data types, including itself. 
Construcing a proper list is more complicated but starting a list is easy. 
All you have to do is to push the empty list to the
stack like so: ```[]```. From there, values can be added to this empty list 
using the ```push``` or ```fpush``` operator which will be discussed later.

#### Object
This is another versatile data type different from lists but also very useful.
Objects store key value pairs where the keys are type String and the values are
type Value. This means like Lists Objects can store themselves. 
An object is constructed very similarly to a list where an empty object is pushed
on the stack like so: ```{}```. The operator ```addField``` can then be used to 
add fields to the object. The Object operators will be discussed later. 

### Operators
Now that all the types of data have been covered that can be pushed onto the stack, 
it's time to cover all the basic operators that can be used on the data.

The general stack will be displayed in a left-to-right format where the top of the stack is the rightmost item. These generalized stacks will only contain the bare minimum items for a given operation, such as
a general stack being ```x y``` for an operation that needs two operands. However, more than just ```x``` and ```y``` can be on the stack. 
For example, the stack could be ```a b c d e f x y``` and the operation would only care about ```x``` and ```y``` since it only needs two operands at the top of the stack.  
The ending stack that results from an example program will be displayed top down with the top item of the stack listed last.

The final stack given in the examples is the stack you'd see get printed in the terminal after running EcksDee on a program file. 
This form of the stack is read bottom up, where the top of the stack is at the bottom. In other words, the stack grows down.

Both forms of stacks will be used in describing various code examples in operators and beyond.

At present, the current operators are: 

#### Operator: ```+``` 

##### Performance: ```O(1)``` (constant time)

Adds the top two elements of a stack together. 
For example, given a stack of numbers: ```b a```, it pops ```b``` and ```a``` 
and adds them together and pushes number ```c```, leaving stack: ```c```. In the case of numbers ```a``` and ```b```, 
their types have to match and the valid types for ```+``` are: ```Integer```, ```BigInteger```, ```Float```, ```Double```, and ```Boolean```.
For the first four types mentioned, ```+``` works as an adder as expected. For type ```Boolean```, 
the ```+``` works like a logical OR, which means the result of adding two Booleans is ```true``` 
as long as at least one is True.

Example Program: <br>
	```2 2 + 2b 3b + 3.0 7.0 + 21.0d 21.2d + False True +``` <br>
Final Stack: 
```
Integer 4
BigInteger 5 
Float 10.0 
Double 42.2 
Boolean True
```

#### Operator: ```-``` 

##### Performance: ```O(1)``` (constant time)

Given a stack of values: ```a b``` where ```a``` and ```b``` are matching types and of types: 
```Integer```, ```BigInteger```, ```Float```, or ```Double```. Subtracts ```b``` from ```a``` 
and pushes the result ```c``` to the stack, leaving stack ```c```.

Example Program: <br>
	```5 3 - 44b 2b - 3.28 0.14 - 2.71828d 0.7d -``` <br> 
Final Stack: 
```
Integer 2 
BigInteger 42 
Float 3.1399999 
Double 2.01828
```

#### Operator: ```*``` 

##### Performance: ```O(1)``` (constant time)

Multiplies the top two elements of the stack together. As with ```+``` and ```-``` , 
the types must match and the valid types for ```*``` are: ```Integer```, ```BigInteger```, ```Float```, ```Double```, and ```Boolean```.
For the first four types ```*``` acts as multiplication. For ```Boolean``` it acts as a logical AND.

Example Program:<br> 
	```2 2 * 2b 3b * 3.0 7.0 * 21.0d 21.2d * False True *``` <br>
Final Stack: 
```
Integer 4 
BigInteger 6 
Float 21.0 
Double 445.2 
Boolean False
```

#### Operator: ```/``` 

##### Performance: ```O(1)``` (constant time)

Given stack: ```a b``` where ```a``` and ```b``` are both of type ```t``` in which ```t``` can be
types: ```Integer```, ```BigInteger```, ```Float```, and ```Double```. The operator ```/``` then
pops ```a``` and ```b``` and divides ```a``` by ```b```, getting resulting value ```c``` still 
of type ```t``` which is then pushed to the stack, leaving stack: ```c```. 


Example Program: <br>
	```2 2 / 3b 2b / 3.0 2.0 / 355.0d 113.0d /```  <br>
Final Stack: 
```
Integer 1
BigInteger 1 
Float 1.5 
Double 3.1415929203539825
```

#### Operator: ```swap``` 

##### Performance: ```O(1)``` (constant time)

Swaps the top two items on top of the stack. If the stack is empty 
or only one item is on the stack, nothing happens. In terms of types, any two items 
of any data type can be swapped on the stack. Generally, given stack: ```x y```, 
the resulting stack from using swap is: ```y x```.

Example Program:
```
"foo" 
"bar" 
swap
``` 

Final Stack: 
```
String {chrs = "bar", len = 3}
String {chrs = "foo", len = 3}
```

#### Operator: ```drop``` 

##### Performance: ```O(1)``` (constant time)

Easily the most common operator. Given stack ```x y z```, drop removes the top item from the stack. 
In this case, applying drop to the stack gives ```x y```, removing ```z```.
If the stack is empty when ```drop``` is used, nothing happens.

Example Program: <br>
	```"foo" 42 1.414 666 drop``` <br>
Final Stack: 
```
String {chrs = "foo", len = 3}
Integer 42
Float 1.414
```

#### Operator: ```dropStack``` 

##### Performance: ```O(1)``` (constant time)

Clears the entire stack. Can be used if the stack is really full 
and the user doesn't want to type ```drop``` over and over again.

**Be aware!** The stack is a global data structure in EcksDee, 
therefore if you use ```dropStack``` it will clear the stack in all contexts, 
including any data desired to be saved.

Example Program: <br>
	```"foo" 42 1.414 666 dropStack``` <br>
Final Stack: 
```
```

#### Operator: ```rot``` 

##### Performance: ```O(1)``` (constant time)

Rotates the top three items of the stack to the right. 
If only two operands are on stack it acts like the ```swap``` operator, and if only one or no items exist, nothing happens. 
The operator ```rot``` can work with any types involved.
Generally, given stack: ```x y z```, using rot makes it ```z x y``` because it gets rotated to the right. 

Example Program: <br>
	```"foo" "bar" "baz" rot```<br>
Final Stack: 
```
String {chrs = "baz", len = 3}
String {chrs = "foo", len = 3}
String {chrs = "bar", len = 3}
```

#### Operator: ```dup``` 

##### Performance: ```O(1)``` (constant time)

Pushes a duplicate of the top element of the stack or does nothing if stack is empty. 
Like ```swap```, ```drop```, and ```rot```, ```dup``` works with any type on the stack. In general, given a stack: ```x```, 
using ```dup``` results in the stack: ```x x```.

Example Program: <br>
	```1 2 3 dup```<br>
Final Stack: 
```
Integer 1 
Integer 2 
Integer 3 
Integer 3
```

#### Operator: ```==``` 

##### Performance: ```Worst Case: O(n) Best Case: O(1)``` <br> (Constant time for all types except lists which is linear if lengths of two lists being compared equal each other.)

Determines if the top two elements of a stack are equal to each other. 
Pops top two items and pushes a ```Boolean``` based on equality. 
For example, given stack: ```x y```, where both items are NOT type ```Object```, ```x``` and ```y``` are popped and checked if ```x``` is equal to ```y```. 
If so, ```True``` is pushed and ```False``` otherwise. 
For a valid comparison to be made, the types of ```x``` and ```y``` must match, otherwise an error is thrown.

Example Program: <br>
	```1 1 == false true == 2.0 2.1 == 1.0d 1.42d == "foo" "bar" == "foo" "foo" ==```<br>
Final Stack: 
```
Boolean True 
Boolean False 
Boolean False 
Boolean False 
Boolean False 
Boolean True
```

#### Operator: ```/=``` 

##### Performance: ```Worst Case: O(n) Best Case: O(1)``` <br> (Constant time for all types except lists which is linear.)

Exactly the same as ```==``` except it pushes the opposite Boolean to the stack.
The types of the operands also need to match for this to work.

Example Program: <br>
	```1 1 /= false true /= 2.0 2.1 /= 1.0d 1.42d /= "foo" "bar" /= "foo" "foo" /=```<br>
Final Stack: 
```
Boolean False 
Boolean True 
Boolean True 
Boolean True 
Boolean True 
Boolean False
```

#### Operator: ```>``` 

##### Performance: ```Worst Case: O(n) Best Case: O(1)``` <br> (Constant time for all types except lists which is linear.)

Determines if the second to top element is greater than the top element of the stack. 
Pushes ```Boolean``` based on the truth value of the expression. If the types don't match, an error occurs.
Generally, given stack: ```x y```, where both items are NOT type ```Object```, it pops ```x``` and ```y``` to see if ```x``` is greater than ```y```
and pushes a ```Boolean``` based on the result. 

Example Program: <br>
	```6 9 > 9.9 9.5 > 148494484948484b -485b > 3.141592653589793d 2.71828d > "foo" "bar" > "bar" "baz" >```<br>
Final Stack: 
```
Boolean False 
Boolean True 
Boolean True 
Boolean True 
Boolean True 
Boolean False
```

#### Operator: ```<``` 

##### Performance: ```Worst Case: O(n) Best Case: O(1)``` <br> (Constant time for all types except lists which is linear.)

Acts like the greater than (```>```) operator but checks to see 
if the second to top is LESS than the top, rather than greater.
Types of the two comparison operands need to match. <br>
Example Program: <br>
	```6 9 < 9.9 9.5 < 148494484948484b -485b < 3.141592653589793d 2.71828d < "foo" "bar" < "bar" "baz" <```<br>
Final Stack: 
```
Boolean True 
Boolean False 
Boolean False 
Boolean False 
Boolean False 
Boolean True
```

#### Operator: ```>=``` 

##### Performance: ```Worst Case: O(n) Best Case: O(1)``` <br> (Constant time for all types except lists which is linear.)

Examines if second to top element is greater than or equal to the top element of the stack.
Types need to match. Given stack: ```x y```, where both items are NOT type ```Object```, it is determined if ```x``` is greater than or equal to ```y```. 

Example Program: <br>
	```"foo" "foo" >= 99 100 >= 3.14 2.718 >= 69.420d 420.69d >= 'c' 'a' >= true true >=``` <br>
Final Stack: 
```
Boolean True 
Boolean False 
Boolean True 
Boolean False 
Foolean True 
Boolean True
```

#### Operator: ```<=``` 

##### Performance: ```Worst Case: O(n) Best Case: O(1)``` <br> (Constant time for all types except lists which is linear.)

Acts like the greater than equal to operator (```>=```) but has generally opposite results 
by checking if the second to top of the stack is LESS than or equal to the top.
Types of operands need to match or an error occurs.

Example Program: <br>
	```"foo" "foo" <= 99 100 <= 3.14 2.718 <= 69.420d 420.69d <= 'c' 'a' <= true true <=``` <br>
Final Stack: 
```
Boolean True 
Boolean True 
Boolean False 
Boolean True 
Boolean False 
Boolean True 
```

#### Operator: ```%``` 

##### Performance: ```O(1)``` (constant time)

Given stack: ```x y```, ```x``` and ```y``` are popped and ```x``` modulo (division but finding the remainder instead of the quotient) ```y``` 
is performed with the result pushed on the stack. 
In other words, the second to top item of the stack is modded by the top item of the stack.
The items ```x``` and ```y``` need to both be of data type ```t``` where ```t``` can be ```BigInteger``` or ```Integer```. 

Example Program: <br>
	```23 2 % 16 2 % 2023b 4b % 2020b 4b % 1969b 100b % 22b 10b % 40999 400 %```<br>
Ending Stack: 
```
Integer 1 
Integer 0 
BigInteger 3 
BigInteger 0 
BigInteger 69 
BigInteger 2 
Integer 199
```

#### Operator: ```and``` 

##### Performance: ```O(1)``` (constant time)

Given stack: ```x y```, where ```x``` and ```y``` are of type Boolean, the operator ```and``` pops ```x``` and ```y``` 
and performs a logical AND operation on them, pushing the Boolean result to the stack. ```True``` is only
pushed if ```x``` and ```y``` are both ```True```.

Example Program: <br>
	```true true and true false and false true and false false and```<br>
Final Stack: 
```
Boolean True 
Boolean False 
Boolean False 
Boolean False
```

#### Operator: ```or``` 

##### Performance: ```O(1)``` (constant time)

Given stack: ```x y```, where ```x``` and ```y``` are of type Boolean, the operator ```or``` pops ```x``` and ```y``` 
and performs a logical OR operation on them, pushing the Boolean result to the stack.
```False``` is only pushed when ```x``` and ```y``` are both ```False```. 

Example Program: <br>
	```true true or true false or false true or false false or```<br>
Final Stack: 
```
Boolean True 
Boolean True 
Boolean True 
Boolean False
```

#### Operator: ```xor``` 

##### Performance: ```O(1)``` (constant time)

Given stack: ```x y```, where ```x``` and ```y``` are of type Boolean, the operator ```xor``` pops ```x``` and ```y``` and performs a logical XOR (eXclusive OR) operation on them, 
pushing the Boolean result to the stack. ```True``` is only pushed when ```x``` is ```true``` and ```y``` is ```false``` or ```x``` is ```false``` and ```y``` is ```true```.

Example Program: <br>
	```true true xor true false xor false true xor false false xor```<br>
Final Stack: 
```
Boolean False 
Boolean True 
Boolean True 
Boolean False
```

#### Operator: ```not``` 

##### Performance: ```O(1)``` (constant time)

Given stack: ```x```, where ```x``` is of type Boolean, the operator ```not``` pops ```x``` and peforms a logical NOT operation on ```x```, pushing the opposite Boolean to the stack. 

Example Program: <br>
	```false not true not``` <br>
Final Stack: 
```
Boolean True 
Boolean False
```

#### Operator: ```pow``` 

##### Performance: ```O(1)``` (constant time)

Given stack: ```x y```, where ```x``` and ```y``` are of type ```t``` which can be: 
```Float``` or ```Double```, ```x``` and ```y``` are popped and ```x``` is raised to the power of ```y```, yielding an exponent result that's pushed to the stack. 

Example Program: <br>
	```2.0 10.0 pow 2.71828d 3.14159265358979323d pow 2.0 0.5 pow 9.0 0.5 pow```<br>
Final Stack: 
```
Float 1024.0 
Double 23.14064373189989 
Float 1.4142135
Float 3.0
```

#### Operator: ```++``` 

##### Performance: ```O(n + m)``` <br> 
(Linear time where n is the number of things in the first String/List and m is the number of items in the second String/List.)

Given stack: ```x y```, where ```x``` and ```y``` are both the same type ```String``` or ```List```, ```x``` and ```y``` are popped and concatenated together, with the result being pushed onto the stack.

Example Program: <br>
	```"foo " "bar" ++ [] 1 p 2 p 3 p [] 4 p 5 p 6 p ++```<br>
Final Stack: 
```
String {chrs = "foo bar", len = 7}
[Integer 1, Integer 2, Integer 3, Integer 4, Integer 5, Integer 6]
``` 

How the ```p``` aka ```push``` operator works is discussed below.

#### Operator: ```push``` 

##### Performance: 
```
Lists: O(log(n))
Strings O(n)
``` 

(Logarithmic time for Lists and linear time for Strings.)

Given stack: ```x y```, where ```x``` is of type ```String``` or ```List```, and ```y``` is of type ```Char``` for a ```String``` 
and ANY type for a ```List```, ```x``` and ```y``` are popped and ```y``` is pushed into ```x``` using the ```push``` aka ```p``` operator. 
This operator is how lists are built in EcksDee and can be how extra characters are added 
to strings. The operator ```push``` and ```p``` are synonymous. Having a shortened version 
of ```push``` just makes it easier to manually build a list in code.

Example Program:
```
[] "foo" push "bar" p "baz" p 
[] 2 push 4 push 6 push 8 push 
"Pushed Char: " 'c' push
[] "lists" p "can" p 2 p "have" p 'c' p "multiple" p "types" p
``` 

Final Stack: 
```
[String {chrs = "foo", len = 3}, String {chrs = "bar", len = 3}, String {chrs = "baz", len = 3}]
[Integer 2, Integer 4, Integer 6, Integer 8]
String {chrs = "Pushed Char: c", len = 14}
[String {chrs = "lists", len = 5}, String {chrs = "can", len = 3}, Integer 2, 
String {chrs = "have", len = 4}, Char 'c', String {chrs = "multiple", len = 8}, 
String {chrs = "types", len = 5}]
```

#### Operator: ```pop``` 

##### Performance: 
```
Lists: O(log(n))
Strings O(n)
```

(Logarithmic time for Lists, linear time for Strings.) 

Given stack: ```x```, where ```x``` is of type ```String``` or ```List```. Item ```x``` is popped from the stack, the last item is removed from the list or string, 
with the altered ```String```/```List``` pushed back on with the ```Char```/```Value``` pushed also.
If the ```String```/```List``` is empty, nothing happens except the empty ```String```/```List``` is pushed back to the stack. Operators ```pop``` and ```po``` are equivalent.
If the ```String```/```List``` is empty when ```pop``` is called, then nothing happens.

Example Program: <br>
	```"Hello, Worldz" pop [] 1 p 2 p 3 p 77 p pop```<br>
Final Stack: 
```
String {chrs = "Hello, World", len = 12}
Char 'z'
[Integer 1, Integer 2, Integer 3]
Integer 77
```

#### Operator: ```fpush``` 

##### Performance: 
```
Lists: O(n * log(n))
Strings O(1)
``` 

(Linear logarithmic time for lists. Constant time for Strings!)

Given stack: ```x y``` where ```x``` is of type ```String```/```List``` and ```x``` is of type ```Char```/Any. 
Items ```x``` and ```y``` are popped and ```y``` is pushed to the front of ```x``` 
with the result being pushed back to the stack. 
Basically the same as ```push``` but acts at the front of a ```String```/```List``` instead of at the back. Operators ```fpush``` and ```fp``` are also synonymous just like with ```push``` and ```p```. 

Example Program: <br>
	```[] "foo" fpush "bar" fpush "baz" fpush " is a front pushed Char" 'c' fp```<br>
Final Stack: 
```
[String {chrs = "baz", len = 3}, 
String {chrs = "bar", len = 3}, String {chrs = "foo", len = 3}]
String {chrs = "c is a front pushed Char", len = 24}
```

#### Operator: ```fpop``` 

##### Performance: 
```
Lists: O(n * log(n))
Strings O(1)
``` 

(Linear logarithmic time for lists. Constant time for Strings!)

Given stack: ```x```, ```fpop``` or ```fpo``` pops an item from the front of a ```String``` or ```List``` instead of the back like ```pop```/```po``` does. The altered ```String```/```List``` is then pushed back on the stack and the front popped ```Char```/Value is pushed onto the stack afterwards.

Example Program: <br>
```
"9There was a Char that doesn't belong at the start of this sentence." 
fpop
[] 666 p 1 p 2 p 3 p 
fpo
```
<br>

Final Stack: 
```
String {chrs = "There was a Char that doesn't belong at the start of this sentence.", len = 67}
Char '9'
[Integer 1, Integer 2, Integer 3]
Integer 666
```

#### Operator: ```index``` 

##### Performance: 
```
Lists: O(log(n))
Strings O(n)
``` 

(Logarithmic time for lists. Linear time for Strings.)

Given stack: ```x y```, where ```x``` is of type ```String```/```List``` and ```y``` is of type ```Integer```. Item ```y``` is popped and used as an index into ```x```. If the index is valid, 
then the ```Char```/Value contained in ```x``` at index ```y``` is duplicated and pushed onto the stack. 
The item ```x``` remains unchanged.

Example Program:<br>
	```[] 1 p 2 p 3 p 1 index "foo bar baz" 0 index``` <br>
Final Stack: 
```
[Integer 1, Integer 2, Integer 3]
Integer 2
String {chrs = "foo bar baz", len = 11}
Char 'f'
```

#### Operator: ```length``` 

##### Performance: 
```
O(1)
``` 

(Constant time)

Given stack: ```x```, where ```x``` is of type ```String```/```List```. 
The operator ```length``` or ```len``` fetches the length of the ```String```/```List``` 
and pushes the resulting length as an ```Integer``` to the stack.

Example Program:<br>
	```"foo bar baz" length [] 1 p 2 p 3 p "wow!" p 666 p len``` <br>
Final Stack: 
```
String {chrs = "foo bar baz", len = 11}
Integer 11
[Integer 1, Integer 2, Integer 3, String {chrs = "wow!", len = 4}, Integer 666]
Integer 5
``` 

#### Operator: ```isEmpty``` 

##### Performance: 
```
O(1)
``` 

(Constant time)

Given stack: ```x```, where ```x``` is of type ```String```/```List```/```Object```. 
The operator ```isEmpty``` determines if ```x``` is empty 
and pushes a ```Boolean``` to the stack based on that check.

Example Program: <br>
	```"" isEmpty [] isEmpty [] 1 p isEmpty "foo" isEmpty {} isEmpty {} "foo" 42 addField isEmpty``` <br>
Final Stack: 
```
String {chrs = "", len = 0}
Boolean True
[]
Boolean True
[Integer 1]
Boolean False
String {chrs = "foo", len = 3}
Boolean False
{}
Boolean True
{foo : Integer 42}
Boolean False
```

#### Operator: ```clear``` 

##### Performance: 
```
O(1)
``` 

(Constant time)

Given stack: ```x```, where ```x``` is of type ```String```/```List```/```Object```. 
Pops ```x``` off the stack and pushes an empty ```String```/```List```/```Object```, effectively clearing ```x```. 

Example program:  <br>
	```"This string isn't empty!" dup clear [] 1 p 2 p dup clear {} "bar" "foo" addField dup clear``` <br>
Final Stack:
```
String {chrs = "This string isn't empty!", len = 24}
String {chrs = "", len = 0}
[Integer 1, Integer 2]
[]
{bar : String {chrs = "foo", len = 3}}
{}
```

#### Operator: ```contains``` 

##### Performance: 
```
Strings and Lists: O(n)
Objects: O(log(n))
``` 

(Linear time for Strings and Lists. Logarithmic time for Objects.)

Given stack ```x y```, where ```x``` is a ```String```/```List```/```Object``` and ```y``` is a ```Char```/Value/```String```. 
Checks to see if ```y``` is contained in ```x``` and pushes a ```Boolean``` to stack as a result, leaving original values.

Example Program:
```
"foo"
'f' contains
"bar"
'z' contains

[] 1 p 2 p 3 p
2 contains

{} "foo" 666 addField
"foo" contains

{} "foo" 666 addField
"bar" contains
```

Final Stack:
```
String {chrs = "foo", len = 3}
Char 'f'
Boolean True
String {chrs = "bar", len = 3}
Char 'z'
Boolean False
[Integer 1, Integer 2, Integer 3]
Integer 2
Boolean True
{foo : Integer 666}
String {chrs = "foo", len = 3}
Boolean True
{foo : Integer 666}
String {chrs = "bar", len = 3}
Boolean False
```

#### Operator: ```changeItemAt``` 

##### Performance: 
```
O(log(n))
``` 

(Logarithmic time)

Given stack: ```x y z```, where ```x``` is a ```List```, ```y``` is type Value, 
and ```z``` is type ```Integer```.
Pops ```x```, ```y```, and ```z```, and alters item at index ```z``` in ```x``` to item ```y``` and then pushes the altered list ```L``` to the stack, leaving stack: ```L```

Example Program:
```
[] 'f' p 'o' p 'o' p

/' Demonstrates the change more easily. 
This is a comment by the way. 
This will be discussed more later on. '/

dup 

'b' 0 changeItemAt
```

Final Stack:
```
[Char 'f', Char 'o', Char 'o']
[Char 'b', Char 'o', Char 'o']
```

#### Operator: ```isWhiteSpace``` 

##### Performance: 
```
O(1)
``` 

(Constant time)

Given stack: ```x```, where ```x``` is type ```Char```. Determines if ```x``` is a whitespace character 
and pushes a ```Boolean``` based on the result.

Example Program:
```
' ' isWhitespace
'g' isWhitespace
```

Final Stack:
```
Char ' '
Boolean True
Char 'g'
Boolean False
```

#### Operator: ```cast``` 

##### Performance: 
```
List to String: O(n)
Object to String: O(n)
BigInteger to String: O(n)
All other casts: O(1)
``` 

(Linear time to stringify Lists, Objects, and BigIntegers. Constant time for all other casts.)

Given stack: ```x y```, where ```x``` is a Value and ```y``` is a ```String```, The cast operator pops ```x``` and ```y``` and tries to cast ```x``` to the type spelled 
out in the string ```y```. If the type is valid in the ```String``` ```y```, 
the cast occurs and the casted value ```z``` is pushed to the stack, resulting in stack: ```z```.

The following example program is far from exhaustive in terms of castable types, 
but it's a good example of some common uses for the cast operator.

Example Program:
```
"Your number is: "
2.71828 81.0 pow
"BigInteger" cast 
"String" cast
++

355.0d 113.0d / 
dup
"Integer" cast
```

Final Stack:
```
String {chrs = "Your number is: 150601872777831737268034534874546176", len = 52}
Double 3.1415929203539825
Integer 3
```

#### Operator: ```queryType```

##### Performance:
```
O(1)
```
(Constant time)

Given stack: `x` where `x` is of type `Value` the `queryType` operator pushes a `String` `s` onto the stack based on what type `x` is.
This is useful for restricting the types of arguments in functions especially when paired with the `attempt onError` fancy operator
discussed later on. This has other use cases too. Anything where you'd need to know the types of stuff you're dealing with.

Example:
```
69b queryType
42 queryType
3.14 queryType
1.414d queryType
"foo" queryType
'a' queryType
False queryType
[] queryType
{} queryType
```

Final Stack:
```
BigInteger 69
String {chrs = "BigInteger", len = 10}
Integer 42
String {chrs = "Integer", len = 7}
Float 3.14
String {chrs = "Float", len = 5}
Double 1.414
String {chrs = "Double", len = 6}
String {chrs = "foo", len = 3}
String {chrs = "String", len = 6}
Char 'a'
String {chrs = "Char", len = 4}
Boolean False
String {chrs = "Boolean", len = 7}
[]
String {chrs = "List", len = 4}
{}
String {chrs = "Object", len = 6}
```

#### Operator: ```print``` 

##### Performance: 
```
O(n)
``` 

(Linear time based on size of string being printed.)

Given stack: ```x```, where ```x``` is a String. 
The operator print performs an IO action which writes the ```String``` ```x``` to standard output. 
The stack remains unchanged. This operator prints the string as-is, without anything else happening.

Example Program:
```
"Hello, World!\n"
print
/' Dropping the string leaves an empty stack. 
As a result, all you see when running this program 
is the hello world string getting printed.  '/
drop
```

Final Stack:
```
```

Stdout:
```
Hello, World!
```

#### Operator: ```printLine``` 

##### Performance: 
```
O(n)
``` 

(Linear time based on size of string being printed.)

Works basically the same as ```print``` except for the fact that it automatically adds a newline (```\n```) to the end of the string being printed.

Example Program:
```
/' \n not needed at the end of this string because it's automatically put there by printLine '/
"Hello, World II: Electric Boogaloo!"
printLine
/' Dropping the string leaves an empty stack. 
As a result, all you see when running this program 
is the hello world string getting printed.  '/
drop
```

Final Stack:
```
```

Stdout:
```
Hello, World II: Electric Boogaloo!
```

#### Operator: ```printChar```

##### Performance:
```
O(1)
```

(Constant time since it's one character being printed.)

Given stack ```x``` where ```x``` is of type ```Char```, prints ```x``` to stdout, 
leaving ```x``` on the stack. 

Example Program:
```
'a'
printChar
```

Stdout:
```
a
```

Final Stack:
```
Char 'a'
```

#### Operator: ```printError```
##### Performance:
```
O(n)
```
(Linear time based on size of error string.)

Given stack `x` where `x` is of type `String`,
`x` is written to stdout and the program is
terminated due to an error being thrown.
This operator essentially allows you to throw custom
errors. Note that this operator appends a newline
like the `printLine` operator does.

Example Program: <br>
```
"Custom Error!!!"
printError
```

Stdout:
```
ecksdee: GeneralException "Custom Error!!!"
```

Final Stack:<br>
N/A Since program terminated early so what 
the stack has does not matter

#### Operator: ```debugPrintStack```
##### Performance:
```
o(n)
```
(At least linear time badness but likely worse if there's nested data.)

Debug operator that prints current stack to standard out as well as its length.
Beyond debugging this doesn't have a ton of use cases but it's still useful.

Example:
```
42 3.14 "cheese" 
debugPrintStack
dropStack
```

Stdout:
```
STACK START
String {chrs = "cheese", len = 6}
Float 3.14
Integer 42
STACK END
STACK LENGTH: 3
```

Final Stack:
```

```

As can be seen, the stack is empty but the stack contents are printed 
to stdout showing what the stack was before the whole stack was dropped.

#### Operator: ```read``` 

##### Performance: 
```
O(n)
``` 

(Linear time based on size of string being read from stdin.)

Reads a ```String``` from stdin and pushes the ```String``` to the stack. 
The example input given for stdin is an example, any input could've gone there.

Example Program: <br>
	```read```

Stdin:
```
This
is
multiline
input!!!

```

Final Stack:
```
String {chrs = "This\nis\nmultiline\ninput!!!\n", len = 27}
```

Example using full IO to illustrate multi-line capability:
```
"Enter a statement to be printed: "
printLine
drop

read

"You wrote: \n"
swap ++
printLine
drop
```

TERMINAL:
```
Enter a statement to be printed:
This is a multiline statement.
This will go until an extra newline is entered.
Also will go until EOF is reached.

You wrote:
This is a multiline statement.
This will go until an extra newline is entered.
Also will go until EOF is reached.

```

Final Stack:
```
```

#### Operator: ```readLine``` 

##### Performance: 
```
O(n)
``` 

(Linear time based on size of string being read from stdin.)

Reads a ```String``` ending in a newline of input and pushes the ```String``` to the stack. 
The example input given for Stdin is an example, any input ending with a newline could've gone there.

Generally ```readLine``` is more convenient for basic input 
and ```read``` is better for parsing large input strings. 

Example Program: <br>
	```readLine```

Stdin:
```
This is some input!
```

Final Stack:
```
String {chrs = "This is some input!", len = 19}
```

Example using full IO to illustrate that it's indeed neat:
```
"Enter your name: "
printLine drop
readLine
"Hello, "
swap ++
"!" ++
printLine
drop
```

TERMINAL:
```
Enter your name:
Joe
Hello, Joe!
```

Final Stack:
```
```

#### Operator: ```readChar```

##### Performance:
```
O(1)
```

(Constant time since it's one character being read.)

Reads a ```Char``` from stdin and pushes the ```Char``` to the stack.

Example Program:
```
readChar
```

Stdin:
```
a
```

Final Stack:
```
Char 'a'
```

This operator only consumes one ```Char``` at a time. 

Example with Same Program as Before:

Stdin:
```
abc
```

Final Stack:
```
Char 'a'
```

Example Program to Show how the Consumtion Works:
```
readChar
readLine
```

Stdin:
```
abc
```

Final Stack:
```
Char 'a'
String {chrs = "bc", len = 2}
```

#### Operator: ```addField``` 

##### Performance: 
```
O(log(n))
``` 

(Logarithmic time)

Given a stack ```x y z``` where ```x``` is type ```Object```, ```y``` is type ```String```, and ```z``` is any Value type,
pops the three values from the stack and adds the field named by ```String``` ```y``` with Value ```z``` to ```Object```
```x```. This altered object ```o``` is pushed on the stack leaving stack ```o```. An error is thrown if the field already exists.

Example Program:
```
{}
"foo"
42
addField
{}
"foo" 666 addField
"bar" [] addField
"baz" "This is a string!!!" addField
"qux" {} "foo" 2829682985925825728957927572800002b addField addField
```

Final Stack:
```
{foo : Integer 42}
{bar : [], baz : String {chrs = "This is a string!!!", len = 19}, 
foo : Integer 666, 
qux : {foo : BigInteger 2829682985925825728957927572800002}}
```

#### Operator: ```getField``` 

##### Performance: 
```
O(log(n))
``` 

(Logarithmic time)

Given a stack ```x y``` where ```x``` is type ```Object``` and ```y``` is type ```String```,
pops the two values from the stack, looks up the field named by ```String``` ```y``` and pushes the value ```z``` contained by field ```y```
to the stack, yielding stack: ```x z```. An error happens if the field doesn't exist in the object.

Example Program:
```
{}
"foo"
42
addField
{}
"foo" 666 addField
"bar" [] addField
"baz" "This is a string!!!" addField
"qux" {} "foo" 2829682985925825728957927572800002b addField addField

/' Fetches value of foo contained in object '/

"foo"
getField
```

Final Stack:
```
{foo : Integer 42}
{bar : [], baz : String {chrs = "This is a string!!!", len = 19}, 
foo : Integer 666, 
qux : {foo : BigInteger 2829682985925825728957927572800002}}
Integer 666
```

#### Operator: ```mutateField``` 

##### Performance: 
```
O(log(n))
``` 

(Logarithmic time)

Given a stack ```x y z``` where ```x``` is type ```Object```, ```y``` is type ```String```, and ```z``` is any Value type,
pops the three values from the stack and mutates the field named by ```String``` ```y``` with Value ```z``` to ```Object```
```x```. This altered object with the altered field ```o``` is pushed on the stack leaving stack ```o```. 
An error is thrown if the field doesn't exist in the object or the type of the field's old value and the type of the new value don't match.

Example Program:
```
{}
"foo"
42
addField
{}
"foo" 666 addField
"bar" [] addField
"baz" "This is a string!!!" addField
"qux" {} "foo" 2829682985925825728957927572800002b addField addField

/' Mutates value of foo and qux contained in object '/

dup

"foo"
420
mutateField

/' Grabs the object value of qux 
and mutates its field foo to the value BigInteger 42. 
This updated object is then mutate for the new value of qux in the object. '/

"qux" getField 
"foo"
42b
mutateField
"qux"
swap
mutateField
```

Final Stack:
```
{foo : Integer 42}
{bar : [], baz : String {chrs = "This is a string!!!", len = 19}, 
foo : Integer 666, 
qux : {foo : BigInteger 2829682985925825728957927572800002}}
{bar : [], baz : String {chrs = "This is a string!!!", len = 19}, 
foo : Integer 420, 
qux : {foo : BigInteger 42}}
```

#### Operator: ```removeField``` 

##### Performance: 
```
O(log(n))
``` 

(Logarithmic time)

Given a stack ```x y``` where ```x``` is type ```Object``` and ```y``` is type ```String```,
pops the two values from the stack, looks up the field named by ```String``` ```y``` and removes field ```y``` from ```x```
yielding an altered object without the field in it ```o``` that's pushed onto the stack, yielding stack ```o```. 
An error is thrown if the field doesn't exist in the object ```x```.

Example Program:
```
{}
"foo"
42
addField
{}
"foo" 666 addField
"bar" [] addField
"baz" "This is a string!!!" addField
"qux" {} "foo" 2829682985925825728957927572800002b addField addField

dup

/' Removes object from current object. '/

"qux"
removeField
```

Final Stack:
```
{foo : Integer 42}
{bar : [], baz : String {chrs = "This is a string!!!", len = 19}, 
foo : Integer 666, 
qux : {foo : BigInteger 2829682985925825728957927572800002}}
{bar : [], baz : String {chrs = "This is a string!!!", len = 19}, 
foo : Integer 666}
```

### While Loops
While loops are a form of operator known as a fancy operator 
because more is required to use it than just having some stuff 
on a stack and then using one word to operate on the stack.

The general syntax of a while loop in EcksDee is: <br> 
```while CODE ;```<br>
The semicolon at the end is absolutely necessary due to how EcksDee parses while loops.
The ```CODE``` block can be any code, ranging from nothing to a gigantic program. 

Also note that the semi-colon (;) has to be free floating, it can't be attached to other chunks of code.
This is because EcksDee tokenizes largely on whitespace, so as long as you have at least one whitespace
character on each side of the semi-colon you're good.

A while loop runs the specified code inside it if the top of the stack is a ```Boolean True```.
If the top of the stack is a ```Boolean False``` it stops running.

While loops error if the stack is empty when it comes time to have them run or run again. 
They also error out if the top of the stack isn't a ```Boolean``` type when it comes time to run them
or run them again. To make a loop run properly, the drop operator is necessary. 
	
Example Program:
```
/' Basically just counts from 1 to 10, 
pushing each number onto the stack. '/
1 
dup
10 <
while 
	/' Removes true Boolean 
	if loop decides to run. '/
	drop 
	
	dup
	1 +
	dup 
	10 <
;
/' Removes remaining false Boolean 
when loop finishes running. '/
drop 
```

Final Stack:
```
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
```

The most common error one will face dealing with while loops is the error: <br> 
```
ecksdee: GeneralException "While Loop Error. Top of stack needs to be type Boolean for loop to try and run! Attempted type: Integer"
``` 

This error means that the top of the stack isn't a ```Boolean``` type that the loop needs to determine 
if it needs to run at all or again. To fix this, make sure 
to push a ```Boolean``` type to the top of the stack or have it as a result of a logical expression.

In this example, the error shows the attempted type as an `Integer` which means the stack had an `Integer` on its top which isn't a `Boolean`. 
This statement of what the type of thing on top of the stack was is mostly there to help users debug their code.

The other error likely is the error:
```
ecksdee: GeneralException "While Loop Error. No Boolean value for while loop to check because stack is empty!"
```

This means that the stack is empty when the loop starts trying to run or tries to run again.
To fix this, have a ```Boolean``` type on the stack before the loop starts and before it ends.

### If Statements
This fancy operator is used in branching code and conditional statements. 
These are useful for determining program flow and a series of other things.

The general syntax for an if statement comes in two forms:
```
if CODE_IF_TRUE ;
if CODE_IF_TRUE else CODE_IF_FALSE ;
```

In both cases, the if statement runs the ```CODE_IF_TRUE``` when the top 
of the stack is a ```Boolean True```.
Optionally you can also include code to run if the top of the stack is ```Boolean False```, which is what
the ```else``` chunk is about that contains ```CODE_IF_FALSE```. 

Like the while loop, if statements require the top of the stack to be a ```Boolean``` type to properly
branch. If the top of the stack isn't of that type, an error is thrown. 

Also like with the while loop, the semi-colon (;) is necessary to end an if statement. 
If it's a lone if statement, then put the semi-colon after the ```CODE_IF_TRUE```.
If there is an else chunk, put the semi-colon after the ```CODE_IF_FALSE```.

One error that can occur with if statements is the error:
```
ecksdee: GeneralException "If statement error. No Boolean for if to check because stack is empty!"
```

Which means that the stack is empty and therefore can't be used in branching.
To fix this, have some kind of ```Boolean``` at the top of the stack before the if statement runs.

The other kind of error that can occur with if statements is error: 
```
ecksdee: GeneralException "If statement error. If statement requires top of the stack to be type Boolean to branch! Attempted type: Float"
```
This error means that the stack isn't empty but the top isn't of type ```Boolean```.
To fix this, have a Boolean type at the top of the stack for branching to occur.
This error also mentions the type that is on top of the stack, to help the user debug their code.

Example Program:
```
true 
if
	"I only push if I'm true!"
;

42 69 >
if
	42 69 *
else
	"Forty two isn't bigger than sixty nine!"
;
```

Final Stack:
```
Boolean True
String {chrs = "I only push if I'm true!", len = 24}
Boolean False
String {chrs = "Forty two isn't bigger than sixty nine!", len = 39}
```

### Functions
Functions are useful for repeating code without having to copy-paste it. 
They make code more streamlined and overall more readable. 
Consequently, EcksDee has functions because they're very useful.

The function syntax is different from how while loops or if statements are structured. 
The basic syntax is: <br> 
```func OPP_KEYWORD FUNCTION_NAME POTENTIALLY_CODE ;``` <br>
The ```OPP_KEYWORD``` is an operation keyword. <br>
For functions two such keywords exist: ```def``` and ```call```.

#### def 
Defines a function which runs ```POTENTIALLY_CODE``` when called.
The general syntax looks like this: <br>
```func def FUNCTION_NAME CODE ;```

#### call 
Calls a function. If ```FUNCTION_NAME``` isn't defined, an error is thrown. 
If it is, the code given in the function definition (```CODE```) is run.
EcksDee runs linearly top to bottom so if you call a function 
before you define it in the program flow, an error will be thrown 
as if the function isn't defined even though it is.
The general syntax for calling a function looks like this: <br>
```func call FUNCTION_NAME ;``` 

Functions have no return functionality, they just run whatever code is given 
to them when defined which could even be nothing at all. 
To return something it's best practice to have some kind 
of return value variable that you get the value of at the end of the function. 
How you approach it is obviously up to you though.

#### Example Program
```
/' Calculates root 2, duplicates the value 
and then sees what root 2 times e times pi equals. '/
func def foo 
	2.0d 0.5d pow
	dup
	2.71828d
	*
	3.14159d
	*
;

"First Run"
func call foo ;
"Second Run"
func call foo ;
```

Final Stack:
```
String {chrs = "First Run", len = 9}
Double 1.4142135623730951
Double 12.076989632131767
String {chrs = "Second Run", len = 10}
Double 1.4142135623730951
Double 12.076989632131767
```

#### Nested Function Example Program
```
/' Squares the number at the top of the stack 
while preserving the original number 
Throws error if stack is empty due to * operator. '/
func def square
	dup dup *
;

/' Computes the first 10 squares starting at 0. '/
func def firstTenSquares 
	0
	dup 10 < 
	while
		drop

		func call square ;
		swap
		1 +

		dup 10 <
	;
	drop
	drop
;

func call firstTenSquares ;
```

Final Stack:
```
Integer 0
Integer 1
Integer 4
Integer 9
Integer 16
Integer 25
Integer 36
Integer 49
Integer 64
Integer 81
```

### Variables
Variables are incredibly useful in programming. 
They act as a way of generalizing programs and can make things much easier.

In the case of EcksDee not everything has to be on the stack or a function, 
variables exist to make programming easier as is the case in most languages.
However, EcksDee being stack based results in an interesting take on variables.

Generally, variable syntax has some similarities with function syntax.
The super generalized variable syntax looks like this: <br>
```var CMD_KEYWORD VAR_NAME ;```

The ```CMD_KEYWORD``` has four valid potential values: ```mak```, ```get```, ```mut```, and ```del```.

#### mak 
Performance:
```
O(log(n))
```
Logarithmic time where `n` is the number of variables in existence.

Short for "make", makes a variable. The ```mak``` keyword causes 
the top value of the stack to be read and associated with the desired ```VAR_NAME``` given. 
If the stack is empty, an error is thrown because there's no value to assign to ```VAR_NAME```.

Once the value is read from the top of the stack, its type is analyzed and read in as well.
If the variable with ```VAR_NAME``` was previously made, an error is thrown 
because a variable can't be made more than once without extra measures.

The general syntax for this keyword is: <br>
```var mak VAR_NAME ;``` <br> 
Which saves top value ```x``` from the stack as ```VAR_NAME```.
The original stack is unchanged.

#### get 
Performance:
```
O(log(n))
```
Logarithmic time where `n` is the number of variables in existence.


The second most common ```CMD_KEYWORD```, this keyword gets the value held 
in a variable and pushes it to the stack. 
If the variable with ```VAR_NAME``` hasn't been made using ```mak``` yet, 
an error will be thrown because ```VAR_NAME``` isn't defined. <br>
The general syntax for the get keyword is: <br>
```var get VAR_NAME ;``` <br>
Which pushes the value ```x``` held by ```VAR_NAME``` onto the stack.

#### mut 
Performance:
```
O(log(n))
```
Logarithmic time where `n` is the number of variables in existence.

Short for "mutate", commands ```VAR_NAME``` to hold a new value, effectively mutating it.
If ```VAR_NAME``` hasn't been made with the ```mak``` keyword yet, 
then an error is thrown because ```VAR_NAME``` isn't a variable yet and therefore can't be mutated.
If you try to change ```VAR_NAME``` to a value of a different type, 
an error is thrown because of static typing. 
Once a variable is made it stays the type of the value it holds.
Given stack: ```y``` and ```VAR_NAME``` with value ```x``` of type ```t```. If ```x``` and ```y``` are both type ```t```, then the command:
```var mut VAR_NAME ;``` will change the value of ```VAR_NAME``` from ```x``` to ```y```. 
The stack remains unchanged but the variable has been sucessfully mutated.

#### del 
Performance:
```
O(log(n))
```
Logarithmic time where `n` is the number of variables in existence.

Short for "delete", deletes a variable from existence. 
Since variables are global in scope by default, 
the ```del``` keyword allows some manual scoping to occur, allowing the existence of more local variables.
This keyword is also necessary in functions with variables 
that get called more than once since variables can only be made once by default. 
This allows variables to be made multiple times.
This keyword isn't necessary in functions that are only called once 
or for global variables because it all gets garbage collected by the interpreter in the end.
The general syntax for the ```del``` keyword is: <br>
```var del VAR_NAME ;``` <br>
Which removes ```VAR_NAME``` from existence as a variable, allowing ```VAR_NAME``` to be remade potentially.

#### Example Program Using All Four Keywords
```
40
var mak foo ;
var get foo ;
1 +
var mut foo ;
var get foo ;
1 + 
var mut foo ;

/' Not necessary for a program like this 
but just showing it's a valid action here. '/
var del foo ;
```

Final Stack:
```
Integer 40
Integer 41
Integer 42
```

#### Example Program With Functions and Variables
```
/' This program is like the last one demo'd 
in the functions section but there's variables now. '/

/' Squares the number at the top of the stack 
while preserving the original number 
Throws error if stack is empty due to * operator. '/
func def square
	/' Saves input square number at top 
	of stack to num variable. '/
	var mak num ;
	/' Makes two copies of num and multiples them, 
	effectively squaring num. '/
	var get num ;
	var get num ;
	*

	/' Error would be thrown without this line 
	once function square is called again. '/
	var del num ;
;

/' Computes the first 10 squares starting at 0. '/
func def firstTenSquares 
	0
	var mak count ;
	10 < 
	while
		drop

		var get count ;
		func call square ;
		swap
		1 +
		var mut count ;

		10 <
	;
	drop

	/' Variable count doesn't have to be deleted here 
	since firstTenSquares is only called once 
	but it's best practice to do this to keep the scope 
	of count to the firstTenSquares function 
	and the square function. '/
	var del count ;
;

func call firstTenSquares ;
```

Final Stack:
```
Integer 0
Integer 1
Integer 4
Integer 9
Integer 16
Integer 25
Integer 36
Integer 49
Integer 64
Integer 81
```

Overall variables aren't strictly necessary but they can be very useful 
in making some code more readable and generalizable.

#### LOCAL VARIABLES
Local variables now exist in the language which are a more convenient form of the variable system.
Local variables follow a scoping system where variables of a particular scope get dropped once that
scope ends. Scopes change when entering a function, while loop, or if statements as the program runs.


What this means is that you can create variables inside functions and not have to worry 
about deleting them if the function is called multiple times like with the standard variable
system. 

The syntax of local variables is very similar to standard variables: <br>
```loc CMD_KEYWORD LOC_VAR_NAME ;```

This syntax is very similar to global variables except with the `loc` keyword used instead of `var`.
The keyword `loc` is short for `loc`al variable as one would expect.
The `CMD_KEYWORD` follows the same potential commands as `var` except that there's no `del`, 
since variables automatically follow scope.

Here's a quick summary of all commands:
All commands work on a base-level similarly to how it works for `var`.
##### mak
Performance:
```
O(log(n))
```
Logarithmic time where `n` is the number of variables in given stack frame existence.

Creates the variable with the same type as the item at the top of the stack.

##### get
Performance:
```
O(m * log(n))
```
Linear-logarithmic time where `n` is the number of variables in 
a given stack frame and `m` is the number of stack frames in existence.
This would only happen if a local variable was to be found far away from the current program scope 
where a lot of traversal would be needed.

Traverses through all existing stack frames to find the desired variable name.
Throws error if none is found and pushes value to stack if found.

##### mut
Performance:
```
O(m * log(n))
```
Linear logarithmic time where `n` is the number of variables in 
a given stack frame and `m` is the number of stack frames in existence.

If variable has been found to exist and its value has the same type as the 
value at the top of the stack, the variable is changed to that value at the top of the stack.

##### *NO* del
There is no del for `loc` since variables are cleared out when a given scope is left.

##### Basic Example with a Sum Function:
```
/' Sums the top two arguments on the stack. 
Basically does that + does but preserves the top two elements. '/
func def sum
	/' ENTERED FUNCTION SCOPE '/

	swap
	loc mak x ;
	swap
	loc mak y ;

	loc get x ;
	loc get y ;
	+

	/' 
	No need to delete x or y 
	because they're local variables ! 
	'/

	/' LEFT FUNCTION SCOPE '/

;

2 2
func call sum ;

2 3 
func call sum ;

57 12
func call sum ;
```

Final Stack
```
Integer 2
Integer 2
Integer 4
Integer 2
Integer 3
Integer 5
Integer 57
Integer 12
Integer 69
```

##### Local Variable Example with Recursion
```
func def recurse
	loc mak curr ;
	drop
	
	loc get curr ;
	0 ==
	if 
		drop
		
		"DONE RUNNING!!!"
		printLine
		
		drop
	else
		drop
		
		"STILL RUNNING!!! "
		loc get curr ;
		"String" cast
		++ 
		printLine
		drop

		loc get curr ;
		1 -

		func call recurse ;
	;

	/' 
		As can be seen, this function recursively counts down and the local 
		variable curr doesn't need to be deleted at the end of the function.
		This allows for behavior much closer to recursion!

	'/
;

30
func call recurse ;
10
func call recurse ;
```
Final Stack
```
```
STDOUT
```
STILL RUNNING!!! 30
STILL RUNNING!!! 29
STILL RUNNING!!! 28
STILL RUNNING!!! 27
STILL RUNNING!!! 26
STILL RUNNING!!! 25
STILL RUNNING!!! 24
STILL RUNNING!!! 23
STILL RUNNING!!! 22
STILL RUNNING!!! 21
STILL RUNNING!!! 20
STILL RUNNING!!! 19
STILL RUNNING!!! 18
STILL RUNNING!!! 17
STILL RUNNING!!! 16
STILL RUNNING!!! 15
STILL RUNNING!!! 14
STILL RUNNING!!! 13
STILL RUNNING!!! 12
STILL RUNNING!!! 11
STILL RUNNING!!! 10
STILL RUNNING!!! 9
STILL RUNNING!!! 8
STILL RUNNING!!! 7
STILL RUNNING!!! 6
STILL RUNNING!!! 5
STILL RUNNING!!! 4
STILL RUNNING!!! 3
STILL RUNNING!!! 2
STILL RUNNING!!! 1
DONE RUNNING!!!
STILL RUNNING!!! 10
STILL RUNNING!!! 9
STILL RUNNING!!! 8
STILL RUNNING!!! 7
STILL RUNNING!!! 6
STILL RUNNING!!! 5
STILL RUNNING!!! 4
STILL RUNNING!!! 3
STILL RUNNING!!! 2
STILL RUNNING!!! 1
DONE RUNNING!!!
```

##### Erronious Example with Local Variables
```
true 
if
	drop
	420 
	loc mak foo ;
	loc get foo ;
	loc get foo ;
	* *
	/' Leaving if statement scope! '/
;

/' Will throw an error because foo is not defined in scope here, since we left the if statement scope! '/
loc get foo ;
```

STDERR
```
ecksdee: GeneralException "Local Variable (loc) Get Error. Local Variable foo not defined in any scope!"
```

##### Same Example but Fixed
```
true 
if
	drop
	420 
	loc mak foo ;
	loc get foo ;
	loc get foo ;
	* *
	/' Leaving if statement scope! '/
;
/' This works since attempt to access foo outside its scope has been removed. '/
```
Final Stack
```
Integer 74088000
```

##### Final Local Variable Example
```
/' Good demonstration on the convenience of local variables '/

/' Squares the number at the top of the stack 
while preserving the original number 
Throws error if stack is empty due to * operator. '/
func def square
	/' Saves input square number at top 
	of stack to num variable. '/
	loc mak num ;

	/' Makes two copies of num and multiples them, 
	effectively squaring num. '/
	loc get num ;
	loc get num ;
	*
;

/' Computes the first 10 squares starting at 0. '/
func def firstTenSquares 
	0
	loc mak count ;
	10 < 
	while
		drop

		loc get count ;
		func call square ;
		swap
		1 +
		loc mut count ;

		10 <
	;
	drop
;

/' Can be called multiple times without needing manual variable deletion :) '/
func call firstTenSquares ;
func call firstTenSquares ;
```

Final Stack
```
Integer 0
Integer 1
Integer 4
Integer 9
Integer 16
Integer 25
Integer 36
Integer 49
Integer 64
Integer 81
Integer 0
Integer 1
Integer 4
Integer 9
Integer 16
Integer 25
Integer 36
Integer 49
Integer 64
Integer 81
```

#### `var` VS `loc` When to use which?
Both work as variable systems and both have tradeoffs. 
The `var` system has better performance since it only has to check one map instead of potentially `m` maps
but it requires manual scoping where `loc` automatically manages the variables based on scope which makes
things like functions much more convenient since you don't need a huge list of `var del`s at the end
of a function anymore. However, manual scoping can be useful sometimes like for temporary variables
or other potential use cases. 

TL;DR: Both work with `loc` often being more convenient unless you *really need* manual scoping.

### The `tempStackChange` Block
Tired of putting a billion `drop`s everywhere when just setting up variables? Is it annoying to have to keep writting all these `drop`s?
Then, `tempStackChange` is for you! This block runs code inside it and disregards any changes made to the stack when the block is done.

The syntax is very simple:
```
tempStackChange CODE_TO_RUN ;
```

`CODE_TO_RUN` is whatever code you want to run inside this block. When `CODE_TO_RUN` finishes running, the stack is reverted to what it was before
`tempStackChange` began, which means you can do a bunch of stuff and preserve the stack as what it was.

The use cases for this fancy operator are quite large! 

One nice feature is the fact that this prevents functions from making stack side effects.
For example:
```
func def square
	tempStackChange
		loc mak n ;
		"random crap"
		"gibberish"
		"garbage"
		4763734
		loc get n ;
		loc get n ;
		*
		var mak retVal ;
		debugPrintStack
	;
	debugPrintStack

	var get retVal ;
	var del retVal ;
;

6
func call square ;
```

Stdout:
```
----------------------------------------------
DEBUG START
STACK START
Integer 6
String {chrs = "random crap", len = 11}
String {chrs = "gibberish", len = 9}
String {chrs = "garbage", len = 7}
Integer 4763734
Integer 36
STACK END
STACK LENGTH: 6
DEBUG END
----------------------------------------------
----------------------------------------------
DEBUG START
STACK START
Integer 6
STACK END
STACK LENGTH: 1
DEBUG END
----------------------------------------------
```

Final Stack:
```
Integer 6
Integer 36
```

As can be seen using `debugPrintStack` a bunch of random stuff was on the stack inside the block and none of it survived outside the block.
This means that a function can run its course and the user need not worry about stack side effects outside of the function due to one too many `drop`s.

#### IMPORTANT NOTE
You will however notice from that example that `var` was needed to save the return value for that function instead of `loc`. This is because 
`tempStackChange` creates its own scope with its own frame so local variables made inside the `tempStackChange` block are gone once the block 
finishes. This does make things a little messy for creating a bunch of variables since either you need to use `var` purely, or
transfer from `var` to `loc` following the block, or have some `loc` variables declared with default values before the block and mutate them inside the block.
Consequently, on the front of making a bunch of variables, the `tempStackChange` block doesn't really save on typing.

#### `tempStackChange` - Conclusion
Even though more typing is needed both for the block and for any variable creation done with this block, it's definitely worth using to avoid stack side effects 
and having to think as hardly about what's on the stack and what needs to be dropped from said stack every step of the way.
Overall, it doesn't save on typing but it does save on headaches :).

### The `box` data type and the heap
To allow for the existence of data structures in EcksDee, the abstraction utilizing boxes exists.
Using `box` is much like using variables or functions, with the box keyword followed by a command keyword.
The reason `box` is used instead of pointers is because that's basically how it works: the user has the heap store some blob of data in a box, 
but not in the same contiguous manner as you would see in C. For example, a `box` storing an `Integer` couldn't store multiple integers 
but just a single one in the "box", unlike an `int*` in C. All operations for `box` are thus built around this abstraction.

Specifically, the usage for `box` is this:
```
box COMMAND_WORD ;
```
Where `COMMAND_WORD` is one of five different command words that tell `box` what to do.
These command words are: `make`, `open`, `altr`, `free`, `null`.

#### Command `make`
Given a stack `x` where `x` is any data type `t` the `make` command consumes `x` and puts it in a box to be
stored on the heap. A `Box` containing a box number `b` is then pushed back to the stack, resulting in stack: `b`

Example:
```
5040
box make ;
```
Final Stack:
```
Box 0
```
`Box 0` on the stack represents a box tag which contains the number the desired box is stored at. 
This is effectively like a reference or pointer in other languages. To access what's in this box, 
the `open` command can be used.

#### Command `open`
Given stack `b` where `b` is of type `Box`, "opens" `b` and pushes the contents `c` onto the stack,
resulting in stack: `b c`.

Example:
```
5040
box make ;
box open ;
```
Final Stack:
```
Box 0
Integer 5040
```
As can be seen, `Box 0` was opened and its contents pushed, in this case `Integer 5040`.

#### Command `altr`
Given stack `b v` where `b` is of type `Box` and `v` is any data type which matches the data type stored in
`b`, updates the contents of `Box` `b` to store the new value `v`. (The reason this is `altr` and not `alter`
is because this way all five `box` commands are four characters long.)

Example:
```
5040
box make ;
box open ;
swap
42
box altr ;
box open ;
``` 
Final Stack:
```
Integer 5040
Box 0
Integer 42
```
In this case, `Integer 5040` was altered to be `Integer 42`. If the types weren't the same, an error would've been thrown.

#### Command `free`
Given stack `b` where `b` is of type `Box`, consumes `b` and frees it for reuse by newer `box`es
leaving an empty stack. Akin to C or other languages with manual memory management, `free` frees a box on the heap 
and allows it to be recycled for newly allocated `box`es. This is a good command to use to help save data when
frequently allocating since it frees up space on the heap.

Example:
```
5040
box make ;
box free ;
```

Final Stack:
```

```
If the user where to try to `open` `Box 0` after freeing, an error would be thrown since it's freed.

#### Command `null`
Given empty stack, pushes a NULL `Box` to the stack, leaving stack `b`. 
This command is mostly useful for placeholders of `box`es where boxes
are intended to go eventually but don't exist yet, such as BST node children.

Example:
```
{} 
"value" 5040 addField
"next" box null ; addField
```
Final Stack:
```
{next : Box NULL, value : Integer 5040}
```
This is how a default node in a linked list may be initialized.

#### `Box` - Conclusion
This is a really neat addition to the language since it makes data structures 
and pointers possible at least to an extent which opens up a lot of possibilities.

### The `attempt` `onError` Block (aka try-catch)
An extremely powerful fancy operator that allows for error handling akin to try-catch in other languages.
This code allows you to try to run some code and have code to run as backup if there's an error.

The syntax looks like this:
```
attempt CODE_TO_ATTEMPT onError CODE_TO_RUN_IF_ERROR ;
```

`CODE_TO_ATTEMPT` is any amount of code to try to run without an error.
If an error is thrown anywhere in `CODE_TO_ATTEMPT` then `CODE_TO_RUN_IF_ERROR`
is run instead with the stack reset to before `CODE_TO_ATTEMPT` was run, with the exception
of pushing the error string of the error that would've been thrown to the stack.

Example Program Involving Casting:
```
attempt
	"foo123"
	"Integer"
	cast
onError
	0	
;
```
Final Stack:
```
String {chrs = "Operator (cast) error. Failed to convert String \"foo123\" to type Integer.", len = 73}
Integer 0
```

As can be seen, the error string of the error that would've been thrown 
was simply pushed onto the stack and the code in `onError` 
was run which pushed a default value of `0` onto the stack.
If the user wanted, they could've dropped the error string or thrown it. 
That's the beauty of this: the user can do whatever they want, 
rather than being forced into a thrown error by the system.

A really cool way to use this fancy operator is in controling function input.
With being able to handle errors, it's possible to ensure 
that a function only has particular inputs.

Example:
```
func def square
	attempt
		loc mak arg1 ;
	onError
		drop
		"Error! Function square expects one argument!"
		printError
	;

	dup *
;

func call square ;
```

Stderr:
```
ecksdee: GeneralException "Error! Function square expects one argument!"
```

This custom error was thrown because the function `square` 
was called with nothing on the stack when it expected one thing.
What triggered the error was trying to make a local variable `arg1` with an empty stack
which caused an error that was caught. The original error string was dropped and replaced
with the new custom error string which was then thrown via `printError`

To fix this example, have something on the stack.

Fixed Example:
```
func def square
	attempt
		loc mak arg1 ;
	onError
		drop
		"Error! Function square expects one argument!"
		printError
	;

	dup *
;

45
func call square ;
```
Final Stack:
```
Integer 2025
```

Not only can the count of arguments be checked via this method, 
but so can the type. 

Example:
```
func def square
	/' Checks if one argument exists. '/
	attempt
		loc mak arg1 ;
	onError
		drop
		"Error! Function square expects one argument!"
		printError
	;

	/' Checks to make sure type is an Integer. '/
	attempt
		dup dup
		"Integer"
		cast
		==
		drop
	onError
		drop

		"Error! Function needs argument of type Integer!"
		printError

	;

	dup *
;

"Cheese"
func call square ;
```

Stderr:
```
ecksdee: GeneralException "Error! Function needs argument of type Integer!"
```

An error was thrown because the argument pushed to the stack was `"Cheese"` which is of type `String`.
In this case, an error was thrown when trying to cast `"Cheese"` as an `Integer`. 
The equality check is to ensure that the original matches the casted version, which ensures that
the argument was of type `Integer` because a comparisson error would be thrown due to non-matching
types otherwise.

To fix the error thrown, have an argument on the stack of type `Integer`.

Fixed Example:
```
func def square
	/' Checks if one argument exists. '/
	attempt
		loc mak arg1 ;
	onError
		drop
		"Error! Function square expects one argument!"
		printError
	;

	/' Checks to make sure type is an Integer. '/
	attempt
		dup dup
		"Integer"
		cast
		==
		drop
	onError
		drop

		"Error! Function needs argument of type Integer!"
		printError

	;

	dup *
;

42
func call square ;
```

Final Stack:
```
Integer 1764
```

Finally, as mentioned way back in the `queryType` operator section, the `queryType` operator
is immensely useful when paired with `attempt onError`. This allows broader but more 
understandable type restrictions.

Example using `queryType`:
```
func def square
	/' Checks if one argument exists. '/
	attempt
		loc mak arg1 ;
	onError
		drop
		"Error! Function square expects one argument!"
		printError
	;

	/' Checks to make sure type is numeric. Numeric being an Integer,  '/
	attempt
		queryType
		
		loc mak argType ;
		drop

		/' 
		Determines if the type of argument 
		is any one of the numeric types. 
		'/

		loc get argType ;
		"Integer" ==

		loc get argType ;
		"BigInteger" ==

		loc get argType ;
		"Float" == 

		loc get argType ;
		"Double" == 

		or or or
		not

		/' Errors out if type isn't one of the four numeric types. '/
		if 
			"Error! Function needs argument of type Integer, BigInteger, Float, or Double!"
			printError
		;
		drop

	onError
		/' Just using printError here is generally the default action 
		to take initially since any exceptions thrown still bubble up to user-level. 
		HOWEVER, you can also use drop to get rid of the given error string 
		and throw something different or do something else entirely. Freedom!
		'/
		printError

	;

	dup *
;

"Cheese"
func call square ;
```

Stderr:
```
ecksdee: GeneralException "Error! Function needs argument of type Integer, BigInteger, Float, or Double!"
```

This function has a more lenient restriction since the input can be any of the numeric types 
where before it was just an integer. Though you could restrict it to just an integer if you wanted!
That's the beauty of using `attempt onError` and `queryType` in tandem!

Fixed Example:
```
func def square
	/' Checks if one argument exists. '/
	attempt
		loc mak arg1 ;
	onError
		drop
		"Error! Function square expects one argument!"
		printError
	;

	/' Checks to make sure type is numeric. Numeric being an Integer,  '/
	attempt
		queryType
		
		loc mak argType ;
		drop

		/' 
		Determines if the type of argument 
		is any one of the numeric types. 
		'/

		loc get argType ;
		"Integer" ==

		loc get argType ;
		"BigInteger" ==

		loc get argType ;
		"Float" == 

		loc get argType ;
		"Double" == 

		or or or
		not

		/' Errors out if type isn't one of the four numeric types. '/
		if 
			"Error! Function needs argument of type Integer, BigInteger, Float, or Double!"
			printError
		;
		drop

	onError
		printError

	;

	dup *
;

42
func call square ;

69b
func call square ;

3.14
func call square ;

2.718d
func call square ;

/' Calls to this function with *any* other type would error out! '/
```

Final Stack:
```
Integer 1764
BigInteger 4761
Float 9.859601
Double 7.387524
```

These examples are by no means exhaustive in terms of the potential of this fancy operator!
Any code that's put in the `attempt` block can be recovered from with the code in 
the `onError` block in the event of an error. <br> The potential use cases for this are extremely vast!

### Comments and Whitespace Information
The way EcksDee largely tokenizes its code is by whitespace. 
Meaning that almost every token, with the exception of pushing ```Char```s and ```String```s, 
should all be separated between each other by at least one whitespace character.

Comments in programming languages are super useful because they describe what a chunk of code is doing 
or can contain other misc information for the user or programmer(s) involved. <br>
The general syntax for comments is: <br>
```/' COMMENT_TEXT '/```

Where ```COMMENT_TEXT``` is any text you want in this comment.
**BE SURE TO HAVE /' AND '/ BE THEIR OWN SEPARATE TOKENS**.
If you have one of the comment start/end tokens attached to another word, 
the interpreter will error out as if it were a gibberish word or a comment wasn't ended properly.

#### Examples
Correct comment: <br> 
```/' This is a valid comment! '/``` <br>
Incorrect comments:
``` 
/'This is a bad comment!'/ 
/'So is this! '/ 
/' Or this!'/
drop/' Also this is bad too '/
/' AS well as this '/42
```

All of these incorrect comments will cause an ```unrecognized word``` or
```ended comment while it's still open. need closing '/``` error to be thrown.

Despite some pickyness with comment endings and beginnings, 
whatever text is put as ```COMMENT_TEXT``` can be in any format with as many whitespace characters 
as desired. When done correctly, comments are ignored by the EcksDee interpreter.

## How to Run
Running EcksDee isn't too difficult but does require some doing.

The first big thing needed is a Haskell compiler since EcksDee is interpreted via a Haskell program.
Ideally get GHC (Glasgow Haskell Compiler) since that's what I've used to develop the interpreter.
Likely other Haskell compilers would also work but it's not known for sure.

### How to Run: The Interpreter method
With a Haskell compiler, compile the ecksdee.hs file to turn it into an executable. 

Once the interpreter executable has been made, then you just have to have some code file,
conventionally with the .xd extension though not required. 

On unix based systems in the terminal run it like so: <br> 
```./ecksdee DIR_TO_PROGRAM``` <br>
Where ```DIR_TO_PROGRAM``` is the EcksDee program you want to run.

For example say you have a classic hello world program.  <br>
The commands and outputs in Bash would look like this:
```
$> ghc ecksdee
[1 of 1] Compiling Main             ( ecksdee.hs, ecksdee.o )
Linking ecksdee ...
$> ./ecksdee helloWorld.xd
Hello, World!
``` 

Where helloWorld.xd looks like:
```
"Hello, World!"
printLine drop
```

That's an example of running it in bash on a unix based system. 
It's likely running it on windows is trickier because windows can be annoying 
for this kind of thing. More information on how to run it 
in windows will be gathered and shared here later.
Also, if `ecksdee` is already an existing executable, then you don't need to type in the `ghc ecksdee` line.

### How to Run: The Compiler Method
That's right! This language has a compiler now! 

The compiler is known as `xdc` which is short for `XD Compiler` or `EcksDee Compiler` in full.
It's a compiler that's kind of cheaty with how it works 
because it basically translates EcksDee programs to Haskell 
programs and then has ghc compile those which, let's be honest, is the bulk of the work.
HOWEVER, this still means that xdc is an indirect compiler!

Because xdc is a cheaty cheater who cheats, you still need a Haskell compiler, though this time specifically
GHC since that's what `xdc.hs` calls when compiling the `.hs` file generated. Also, I'm really not sure how
this would run on Windows since `xdc.hs` also runs commands in a bash format but it might be okay. 

All that aside, here's how to compile and then run a basic hello world program:
```
$> ghc xdc
[1 of 1] Compiling Main             ( xdc.hs, xdc.o )
Linking xdc ...
$> ./xdc helloWorld.xd
Opening and reading helloWorld.xd
Generating abstract syntax tree of helloWorld.xd
Writing to helloWorld.hs
Compiling helloWorld.hs
[1 of 1] Compiling Main             ( helloWorld.hs, helloWorld.o )
Linking helloWorld ...
Cleanup
Cleanup Successful
Compilation complete!
$> ./helloWorld
Hello, World!
```
This looks like a lot, but it's mostly command output. 
Once complete, the executable `helloWorld` exists and can be run by simply typing `./helloWorld` 
which is faster and easier than having to call the `ecksdee` interpreter executable every time.

#### How to Run: The Compiler Method: Performance
Compiled EcksDee programs are certainly faster than interpreted, ranging from a few factors faster to a couple of orders of magnitude faster!
This is definitely worth doing therefore to reap the benefits of a faster program when you're sure your code works.

However, compilation time can often take a while since the code generated is clunky and inefficient.
On top of that, it's not as much of a speedup as I believe it could potentially be with more tinkering on my part.

#### How to Run: The Compiler Method: The `--no-cleanup` Flag
As a last little aside to the compilation section, if you want to see the Haskell code generated 
by xdc for some reason, you can add the `--no-cleanup` flag to the end of your xdc program running command
to make it not cleanup generated files. 

For example, in a directory containing
`xdc` and `helloWorld.xd`, normal compilation proceedures previously shown would leave the directory
with one extra file: `helloWorld`, with the rest cleaned up.

If the user were to instead run the command as: `./xdc helloWorld.xd --no-cleanup`,
the compilation output would omit the cleanup dialog since it doesn't happen and the 
directory would contain the files: `xdc`, `helloWorld.xd`, `helloWorld`, `helloWorld.hs`, `helloWorld.hi`, and `helloWorld.o`.
This is bulkier for sure but can be insightful in seeing more of the behind-the-scenes process in how xdc compiles EcksDee programs.
Also, I derive use from this for debugging and ideally future performance improvements.

#### How to Run: The Compiler Method: When to Use and Conclusion
Typically, you don't need to compile your programs and can instead just use the `ecksdee` executable to act as an interpreter.
You only really want to compile for repeated use of the program itself since it's faster and more streamlined to call in the first place.

Overall, this was a really cool thing to create to help speedup EcksDee a bit and feel like a wizard making.
Is it that useful? Eh. But it's sitll cool, at least to me.

### How to Run: Conclusion 
There you have it. If you have a unix based system, a Haskell compiler, 
and these GitHub files, you absolutely can run your own EcksDee programs. Have fun! 

## Random Cool Extra Thing: A Syntax Highlighter
If you use Sublime Text there now exists a syntax highlighter useable for EcksDee!

### To use it:
1. Go to `Preferences` -> `Browse Packages...`
2. Copy the `ecksdeeSyntax.sublime-syntax` file located in the `syntax` directory to the 
   `User` folder located in the package browsing window.
3. In the bottom right corner of the window in a `.xd` file, 
   select `EcksDee` from the parsing options and the highlighting will appear.

This makes EcksDee significantly nicer to code in since 
it's easier to see mistakes and it just looks prettier overall.

## Extra Thing II: Convention
In writing code in EcksDee, there is a convention to naming stuff for some consistency within EcksDee code.
This comes with the obvious caveat that the user can do whatever they want and it's not that big of a deal
but it's still a good idea to have a consistent look in a language when possible.

### Variables
Variables should be named using `camelCase` where it's all letters with capitals separating each word and the initial word of the chain starts with lowercase,
ex: `isLeapYear`, `timeUntilApocalypse`, `ageOfUniverse`, etc.

Variables intended to be constants should be `PascalCase` or `UPPER_CASE_SNAKE_CASE` to help set them apart from regular variables since constants are meant to *never* change.

### Functions
Functions are also named using `camelCase` however, helper functions, functions that you don't want to call directly, should start with an `_`,
ex: `_findSuccessor`, `_buildCache`, etc. This `_` at the start of a function helps make calling it more awkward 
and sets a convention that it's something you should think about calling before doing so, as in, 
a function that should only be privately called by another function as a helper. This is similar to how Python or C does it.

Functions that represent constants, as in data not meant to change, should be `CAPITAL_SNAKE_CASE` or at least `PascalCase`.
When named differently like this, it helps indicate that these aren't traditional functions but are meant to hold some set of fixed actions that
don't alter any data on the stack. This would be useful for macro-esque constants that definitely don't change.
For example, a physics program could have: `func def C 299792458.0 ;` at the top of the program, where the constant `C` could then be used anywhere in code by doing: `func call C ;`.
Alternatively, the user could just do: `299792458.0 var mak C ; drop ` at the top of the program and just `var get` this variable whenever needed but there's the tiny chance 
`C` could be changed since `var` isn't immutable since it can be changed using `mut`. 

### Comments
Put comments above the code you're describing, not below.

If a comment is short, it is constructed like so: `/' This is a comment '/` 
where a space seperates comment start, the comment text, and the comment end.
A larger multi-line comment however, should be constructed instead like this:
```
/'
This is a 
multi-line comment!
!
!!!!!
Wow!
'/
```
This helps distinguish where the comment starts and stops since the comment starters and stoppers are on their own lines.

*More information on the EcksDee convention will be put here once more information is thought of.*

## Conclusion:
EcksDee has been a fun programming language to develop 
from the very simple Forth interpretor it started as. 
Adding data types and IO were both definitely challenges in their own right; monads are freaky!
Also, adding `attempt onError` felt like an act of wizardry in and of itself.

Is EcksDee a revolutionary language? No. Is it a fast language? No. 
Is it even that good of a language? Definitely no! But was it fun to make and keep adding onto? 
Absolutely yes!

I have enjoyed the surreal sensation of making programs in my own programming language 
and seeing my expertise in my own goofy language grow 
and develop as well as see EcksDee itself grow in complexity with new features. 

Wrapping up, this language isn't anything amazing or innovative 
but I'd say it's worth checking out if you're bored and a nerd like me. 
Plus, it's good Reverse Polish Notation practice! 

