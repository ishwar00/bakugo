# Subset of Go Programming Language Specification
- - -

### Table Of Contents
* [Notation](#notation)
* [Source code representation](#source-code-representation)
	* [Characters](#characters)
	* [Letters and digits](#letters-and-digits)
* [Lexical elements](#lexical-elements)
	* [Comments](#comments)
	* [Tokens](#tokens)
	* [Semicolons](#semicolons)
	* [Identifier](#identifier)
	* [Keywords](#keywords)
	* [Operators and punctuation](#operators-and-punctuation)
	* [Integer literals](#integer-literals)
	* [Rune literals](#rune-literals)
	* [String literals](#string-literals)
* [Constants](#constants)
* [Variable](#variables)
* [Types](#types)
	* [Boolean types](#boolean-types)
	* [Numeric types](#numeric-types)
	* [Function types](#function-types)
* [Properites of types and values](#properties-of-types-and-values)
	* [Underlying types](#underlying-types)
	* [Core types](#core-types)
	* [Type identitfy](#type-identity)
	* [Assignability](#assignability)
	* [Representablity](#representability)
* [Blocks](#blocks)
* [Declaration and scope](#declarations-and-scope)
	* [Predeclared identifiers](#predeclared-identifiers)
	* [Uniqueness of identifiers](#uniqueness-of-identifiers)
	* [Constant declarations](#constant-declarations)
	* [Type declarations](#type-declarations)
	* [Type definitions](#type-definitions)
	* [Variable declaration](#variable-declarations)
	* [Function declaration](#function-declarations)
* [Expressions](#expressions)
	* [Operands](#operands)
	* [Primary expression](#primary-expressions)
	* [Calls](#calls)
	* [Operators](#operators)
	* [Operator precedence](#operator-precedence)
	* [Arithmetic operators](#arithmetic-operators)
	* [Integer operators](#integer-operators)
	* [Integer overflow](#integer-overflow)
	* [Comparison operators](#comparison-operators)
	* [Logical operators](#logical-operators)
	* [Address operators](#address-operators)
	* [Constant expression](#constant-expressions)
	* [Order of evaluation](#order-of-evaluation)
* [Statments](#statements)
	* [Terminating statements](#terminating-statements)
	* [Empty statements](#empty-statements)
	* [Expression statements](#expression-statements)
	* [IncDec statements](#incdec-statements)
	* [Assignment statements](#assignment-statements)
	* [If statements](#if-statements)
	* [Return statements](#return-statements)
* [Program initialization and execution](#program-initialization-and-execution)
	* [The zero value](#the-zero-value)
	* [Program execution](#program-execution)
	* [Run-time panics](#run-time-panics)

## Notation
The syntax is specified using a variant of Extended Backus-Naur Form (EBNF):
```
Syntax      = { Production } .
Production  = production_name "=" [ Expression ] "." .
Expression  = Term { "|" Term } .
Term        = Factor { Factor } .
Factor      = production_name | token [ "…" token ] | Group | Option | Repetition .
Group       = "(" Expression ")" .
Option      = "[" Expression "]" .
Repetition  = "{" Expression "}" .
```
Productions are expressions constructed from terms and the following operators, in increasing precedence:
```
|   alternation
()  grouping
[]  option (0 or 1 times)
{}  repetition (0 to n times)
```
Lowercase production names are used to identify lexical (terminal) tokens. Non-terminals
are in CamelCase. Lexical tokens are enclosed in double quotes ``""`` or back quotes ``` `` ``` .

The form ``a … b`` represents the set of characters from ``a`` through ``b`` as alternatives.
The horizontal ellipsis ``…`` is also used else where in the spec to informally denote various 
enumerations or code snippets that are not further specified. The character ``…`` (as opposed 
to the three characters ``...``) is not a token of the Go language.

## Source code representation
Source code is Unicode text encoded in UTF-8. The text is not canonicalized,
so a single accented code point is distinct from the same character
constructed from combining an accent and a letter; those are treated as two
code points. For simplicity, this document will use the unqualified term
character to refer to a Unicode code point in the source text.

Each code point is distinct; for instance, uppercase and lowercase letters
are different characters.

| Implementation restriction |
| :-------- |
| For compatibility with other tools, a compiler may disallow the NUL character (U+0000) in the source text. |

| Implementation restriction |
| :-------- |
| For compatibility with other tools, a compiler may ignore a UTF-8-encoded byte order mark (U+FEFF) if it is the first Unicode code point in the source text. A byte order mark may be disallowed anywhere else in the source. |

## Characters
The following terms are used to denote specific Unicode character categories:
```
newline        = /* the Unicode code point U+000A */ .
unicode_char   = /* an arbitrary Unicode code point except newline */ .
unicode_letter = /* a Unicode code point categorized as "Letter" */ .
unicode_digit  = /* a Unicode code point categorized as "Number, decimal digit" */ .
```
In _The Unicode Standard 8.0_, Section 4.5 "General Category" defines a set of character categories. 
Go treats all characters in any of the Letter categories Lu, Ll, Lt, Lm, or Lo as Unicode letters, and those in the Number category Nd as Unicode digits.

## Letters and digits
The underscore character `_` is considered a lowercase letter.
```
letter        = letter | "_" .
decimal_digit = "0" … "9" .
```
# Lexical elements

## Comments

Comments serve as program documentation. There are two forms:

1. _Line comments_ start with the character sequence `//` and stop at the end of the line.
2. _General comments_ start with the character sequence `/*` and stop with the first
subsequent character sequence `*/`.

A comment cannot start inside a _rune_ or _string_ literal, or inside a comment. A general
comment containing no newlines acts like a space. Any other comment acts like a newline.

## Tokens
Tokens form the vocabulary of the Go language. There are four classes: _identifiers_, _keywords_, _operators_ and _punctuation_, and _literals_. 
_White space_, formed from spaces (U+0020), horizontal tabs (U+0009), carriage returns (U+000D), and newlines (U+000A), is ignored except as it separates tokens that would 
otherwise combine into a single token. Also, a newline or end of file may trigger the insertion of a _semicolon_.
While breaking the input into tokens, the next token is the longest sequence of characters that form a valid token.

## Semicolons
| NOTE | 
| :-------- |
| Floating-point, rune or string literal, keywords `break`, `continue` and `fallthrough`, and punctuation `]` are not yet supported. |

The formal syntax uses semicolons `";"` as terminators in a number of productions. Go programs may omit most of these semicolons using the following two rules:
1. When the input is broken into tokens, a semicolon is automatically inserted into the token stream immediately after a line's final token if that token is
    - an identifier
    - an integer, floating-point, rune, or string literal
    - one of the keywords `break`, `continue`, `fallthrough`, or `return`
    - one of the operators and punctuation `++`, `--`, `)`, `]`, or `}`

2. To allow complex statements to occupy a single line, a semicolon may be omitted before a closing `")"` or `"}"`.

## Identifier

Identifiers name program entities such as variables and types. An identifier is a sequence of one or more letters and digits. The first character in an 
identifier must be a letter.

```
identifier = letter { letter | unicode_digit } .
```
examples of _identifier_
```
a
_x9
ThisVariableIsExported
αβ
this_is_Bakugo
```
Some identifiers are _predeclared._

## Keywords

The following keywords are reserved and may not be used as _identifiers_.
```
func  const  if  type  return  var  else
```
</details>
<details><summary>Reserved keywords</summary>
<p>

```
break        default      interface    select       case
defer        go           map          struct       chan
goto         package      switch       fallthrough  range
continue     for          import
```

<p>
</details>

## Operators and punctuation

The following character sequences represent operators (including assignment operators) and punctuation:

```
+    &    &&    ==    !=    (    )    -    |    ||    <    <=    *
>    >=   {     }     /     ++   =    ,    ;    %     --   !
```
<details><summary>Reserved operators and punctuations </summary>

```
+=    &=    -=    |=    [    ]
^     *=    ^=    <-    <<   /=
<<=   :=    >>    %=    >>=  ...
.     :     &^    &^=   ~
```

</details>

## Integer literals
| NOTE |
| :--- |
| Binary, Octal, Hexadecimal literals are not supported yet. |

An integer literal is a sequence of digits representing an _integer constant_. 

For readability, an underscore character `_` may appear between successive digits; such underscores do not change the literal's value.

```
int_lit        = decimal_lit.
decimal_lit    = "0" | ( "1" … "9" ) [ [ "_" ] decimal_digits ] .
decimal_digits = decimal_digit { [ "_" ] decimal_digit } .
```
## Rune literals

A rune literal represents a _rune constant_, an integer value identifying a Unicode code point. 
A rune literal is expressed as one or more characters enclosed in single quotes, as in 'x' or '\n'. 
Within the quotes, any character may appear except newline and unescaped single quote. 
A single quoted character represents the Unicode value of the character itself, while multi-character sequences beginning with a backslash encode values in various formats.

The simplest form represents the single character within the quotes; since Go source text is Unicode characters encoded in UTF-8, multiple UTF-8-encoded bytes may represent a single integer value. For instance, the literal 'a' holds a single byte representing a literal `a`, Unicode U+0061, value 0x61, while 'ä' holds two bytes (0xc3 0xa4) representing a literal a-dieresis, U+00E4, value 0xe4.

Several backslash escapes allow arbitrary values to be encoded as ASCII text. There are four ways to represent the integer value as a numeric constant: \x followed by exactly two hexadecimal digits; \u followed by exactly four hexadecimal digits; \U followed by exactly eight hexadecimal digits, and a plain backslash \ followed by exactly three octal digits. In each case the value of the literal is the value represented by the digits in the corresponding base.

Although these representations all result in an integer, they have different valid ranges. Octal escapes must represent a value between 0 and 255 inclusive. Hexadecimal escapes satisfy this condition by construction. The escapes \u and \U represent Unicode code points so within them some values are illegal, in particular those above 0x10FFFF and surrogate halves.

After a backslash, certain single-character escapes represent special values:
```
\a   U+0007 alert or bell
\b   U+0008 backspace
\f   U+000C form feed
\n   U+000A line feed or newline
\r   U+000D carriage return
\t   U+0009 horizontal tab
\v   U+000B vertical tab
\\   U+005C backslash
\'   U+0027 single quote  (valid escape only within rune literals)
\"   U+0022 double quote  (valid escape only within string literals)
```

An unrecognized character following a backslash in a rune literal is illegal.

```
rune_lit         = "'" ( unicode_value | byte_value ) "'" .
unicode_value    = unicode_char | little_u_value | big_u_value | escaped_char .
byte_value       = octal_byte_value | hex_byte_value .
octal_byte_value = `\` octal_digit octal_digit octal_digit .
hex_byte_value   = `\` "x" hex_digit hex_digit .
little_u_value   = `\` "u" hex_digit hex_digit hex_digit hex_digit .
big_u_value      = `\` "U" hex_digit hex_digit hex_digit hex_digit
                           hex_digit hex_digit hex_digit hex_digit .
escaped_char     = `\` ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | `\` | "'" | `"` ) .
```

```
'a'
'ä'
'本'
'\t'
'\000'
'\007'
'\377'
'\x07'
'\xff'
'\u12e4'
'\U00101234'
'\''         // rune literal containing single quote character
'aa'         // illegal: too many characters
'\k'         // illegal: k is not recognized after a backslash
'\xa'        // illegal: too few hexadecimal digits
'\0'         // illegal: too few octal digits
'\400'       // illegal: octal value over 255
'\uDFFF'     // illegal: surrogate half
'\U00110000' // illegal: invalid Unicode code point
```

## String literals

A string literal represents a _string constant_ obtained from concatenating a sequence of characters. There are two forms: raw string literals and interpreted string literals.

Raw string literals are character sequences between back quotes, as in ``` `foo` ```. Within the quotes, any character may appear except back quote. The value of a raw string literal is the string composed of the uninterpreted (implicitly UTF-8-encoded) characters between the quotes; in particular, backslashes have no special meaning and the string may contain newlines. Carriage return characters (`'\r'`) inside raw string literals are discarded from the raw string value.
```
string_lit             = raw_string_lit | interpreted_string_lit .
raw_string_lit         = "`" { unicode_char | newline } "`" .
interpreted_string_lit = `"` { unicode_value | byte_value } `"` .
```

```
`abc`                // same as "abc"
`\n
\n`                  // same as "\\n\n\\n"
"\n"
"\""                 // same as `"`
"Hello, world!\n"
"日本語"
"\u65e5本\U00008a9e"
"\xff\u00FF"
"\uD800"             // illegal: surrogate half
"\U00110000"         // illegal: invalid Unicode code point
```
If the source code represents a character as two code points, such as a combining form involving an accent and a letter, the result will be an error if placed in a rune literal (it is not a single code point), and will appear as two code points if placed in a string literal.

## Constants
| NOTE | 
| :------- |
| Floating-point and complex constants are not supported yet. |

There are _boolean constants_, _rune constants_, _integer constants_ and _string constants_. Rune, integer, ~~floating-point~~, and ~~complex constants~~ are collectively called numeric constants.

A constant value is represented by a _rune_, _integer_ or _string_ literal, an identifier denoting a constant, a _constant expression_, The boolean truth values are represented by the
predeclared constants `true` and `false`.

Numeric constants represent exact values of arbitrary precision and do not overflow. Consequently, there are no constants denoting the IEEE-754 negative zero, 
infinity, and not-a-number values.

Constants may be typed or untyped. Literal constants, `true`, `false` and certain _constant expressions_ containing only untyped constant operands are untyped.

A constant may be given a type explicitly by a _constant declaration_ or implicitly when used in a _variable declaration_ or an _assignment statement_ or 
as an operand in an _expression_. It is an error if the constant value cannot be _represented_ as a value of the respective type.

An untyped constant has a _default_ type which is the type to which the constant is implicitly converted in contexts where a typed value is required, for instance, 
in a _variable declarations_ such as `var i = 0` where there is no explicit type. The default type of an untyped constant is `bool`, `rune`, `int` or `string` respectively, depending on whether it is a boolean, rune, integer, or string constant.

| Implementation restriction |
| ----------------------------- |
| Although numeric constants have arbitrary precision in the language, a compiler may implement them using an internal representation with limited precision. That said, every implementation must: 
| Represent integer constants with at least 256 bits.|
| Give an error if unable to represent an integer constant precisely.|

These requirements apply both to literal constants and to the result of evaluating _constant expressions_.

## Variables

A variable is a storage location for holding a value. The set of permissible values is determined by the variable's type.

A variable declaration or, for function parameters and results, the signature of a function declaration reserves storage for a named variable.
The static type (or just type) of a variable is the type given in its declaration.  

A variable's value is retrieved by referring to the variable in an _expression_; it is the most recent value _assigned_ to the variable. 
If a variable has not yet been assigned a value, its value is the _zero value_ for its type.

# Types 

A type determines a set of values together with operations specific to those
values. A type may be denoted by a _type name_, if it has one. 

```
Type      = TypeName | "(" Type ")" .
TypeName  = identifier. 
```
The language predeclares certain type names. Others are introduced with _type declarations_.

Predeclared types and defined types are called _named types_. An alias denotes a named type if the type given in the alias declaration is a named type.

## Boolean types
A boolean type represents the set of Boolean truth values denoted by the predeclared constants _true_ and _false_. The predeclared boolean type is _bool_; it is a _defined type_.

## Numeric types
An _integer_ type represents the set of integer values. They are collectively 
called numeric types. The predeclared architecture-independent numeric types are:

The value of an _n_-bit integer is _n_ bits wide and represented using two's complement arithmetic.

To avoid portability issues all numeric types are _defined types_ and thus distinct 
```
uint8       the set of all unsigned  8-bit integers (0 to 255)
uint16      the set of all unsigned 16-bit integers (0 to 65535)
uint32      the set of all unsigned 32-bit integers (0 to 4294967295)
uint64      the set of all unsigned 64-bit integers (0 to 18446744073709551615)

int8        the set of all signed  8-bit integers (-128 to 127)
int16       the set of all signed 16-bit integers (-32768 to 32767)
int32       the set of all signed 32-bit integers (-2147483648 to 2147483647)
int64       the set of all signed 64-bit integers (-9223372036854775808 to 9223372036854775807)
```
<details><summary>unsupported numeric types</summary>

```
float32     the set of all IEEE-754 32-bit floating-point numbers
float64     the set of all IEEE-754 64-bit floating-point numbers

complex64   the set of all complex numbers with float32 real and imaginary parts
complex128  the set of all complex numbers with float64 real and imaginary parts

byte        alias for uint8
rune        alias for int32
```

</details>

## Function types
| NOTE | 
| :------- |
| FunctionType is not supported yet. |

```
Signature      = Parameters [ Result ] .
Result         = Type .
Parameters     = "(" [ ParameterList [ "," ] ] ")" .
ParameterList  = ParameterDecl { "," ParameterDecl } .
ParameterDecl  = [ IdentifierList ] Type .
```

Within a list of parameters, the names (IdentifierList) must either all be present or all be absent. If present, each name stands for one item 
(parameter) of the specified type and all non-_blank_ names in the signature must be unique. If absent, each type stands for one item of that type. 
Parameter lists are always parenthesized. A single return type can be unparenthesized.

```
func()
func(x int) int
func(a, _ int, z bool) bool
func(a, b int, z bool) (bool)
func(prefix bool, values ...int) (bool)
```

# Properties of types and values

## Underlying types
Each type T has an _underlying type_: If T is one of the predeclared boolean, numeric, or string types,
or a type literal, the corresponding underlying type is T itself. Otherwise, T's underlying type is the
underlying type of the type to which T refers in its declaration.
```
type (
	A1 = string
	A2 = A1
)

type (
	B1 string
	B2 B1
)
```
The underlying type of string, A1, A2, B1, and B2 is string.

## Core types
Each non-interface type T has a _core type_, which is the same as the _underlying type_ of T.

## Type identity
Two types are either _identical_ or _different_.

A _named type_ is always different from any other type. Otherwise, two types are identical if their
_underlying_ type literals are structurally equivalent; that is, they have the same literal structure and
corresponding components have identical types.
```
type (
	A0 = float32
	A1 = A0
	A2 = bool
	A3 = int

	B0 A0
	B1 float32

	C0 = B0
)
```
these types are identical:
```
A0, A1, and float32
A2 and bool
A3 and int

B0 and C0
int and int
```

B0 and B1 are different because they are new types created by distinct type definitions.

## Assignability
A value x of type V is _assignable_ to a _variable_ of type T ("x is assignable to T") if one of the
following conditions applies:

- V and T are identical.
- V and T have identical _underlying types_ and atleast one of V or T is not a _named type_.
- x is an untyped _constant representable_ by a value of type T.

## Representability
A _constant_ x is _representable_ by a value of tye T, if one of the following conditions applies:

- x is in the set of value _determined_ by T 

```
x                   T           x is representable by a value of T because

1024                int16       1024 is in the set of 16-bit integers
1e10                uint64      10000000000 is in the set of unsigned 64-bit integers
```
```
x                   T           x is not representable by a value of T because

0                   bool        0 is not in the set of boolean values
-1                  uint16      -1 is not in the set of unsigned 16-bit integers
```

# Blocks

A block is a possibly empty sequence of declarations and statements within matching brace brackets.
```
Block = "{" StatementList "}" .
StatementList = { Statement ";" } .
```

In addition to explicit blocks in the source code, there are implicit blocks:

1. The _universe block_ encompasses all Go source text.
2. Each _package_ has a _package block_ containing all Go source text for that package.
3. Each file has a _file block_ containing all Go source text in that file.
4. _"if"_ statement is considered to be in it's own implicit block

Blocks nest and influence _scoping_.

# Declarations and scope

A _declaration_ binds a non-_blank_ identifier to a _constant_, _type_, _variable_, _function_. Every
identifier in a program must be declared.

An identifier may be re-declared, possibly with a different type, in the same block.

```
Declaration   = ConstDecl | TypeDecl | VarDecl .
TopLevelDecl  = Declaration | FunctionDecl . 
```

The _scope_ of a declared identifier is the extent of source text in which the identifier denotes the specified
constant, type, variable, function, label, or package.


Go is lexically scoped:

1. The scope of a _predeclared identifier_ is the universe block.
1. The scope of an identifier denoting a constant, type, variable, or function declared at top level (outside any function) is the universe block.
1. The scope of an identifier denoting a function parameter begins at the beginning of the function body and ends at the first re-declaration of the same identifier, or at the end of the function body if there are no re-declarations.
1. The scope of a constant or variable identifier declared inside a function begins at the end of the ConstSpec or VarSpec and ends at the beginning of the next re-declaration of the same identifier, or  at the end of the innermost containing block if there are no re-declarations.
1. The scope of a type identifier declared inside a function begins at the identifier in the TypeSpec and ends at the end of the innermost containing block.

An identifier declared in a block may be redeclared in an inner block too. While the identifier of the inner declaration is in scope, it denotes the entity declared by the inner declaration.

## Predeclared identifiers
The following identifiers are implicitly declared in the _universe block_:
```
Types:
	bool int int8 int16 int32 int64 
	uint uint8 uint16 uint32 uint64 

Constants:
	true false

Functions:
	print println
```
<details><summary>Yet to be added</summary>

```
Types:
	any byte comparable complex64 complex128 
    error float32 float64 rune string uintptr

Constants:
	iota

Zero value:
	nil

Functions:
	append cap close complex copy delete imag len
	make new panic real recover
```
</details>

## Uniqueness of identifiers
Given a set of identifiers, an identifier is called _unique_ if it is _different_ from every other in the set.
Two identifiers are different if they are spelled differently. Otherwise, they are the same.

## Constant declarations
A constant declaration binds a list of identifiers (the names of the constants) to the values of a list of
_constant expressions_. The number of identifiers must be equal to the number of expressions, and the _n_th
identifier on the left is bound to the value of the _n_th expression on the right.

```
ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
ConstSpec      = IdentifierList [ [ Type ] "=" ExpressionList ] .

IdentifierList = identifier { "," identifier } .
ExpressionList = Expression { "," Expression } .
```

If the type is present, all constants take the type specified, and the expressions must be _assignable_ to that
type. If the type is omitted, the constants take the individual types of the corresponding expressions. If the
expression values are untyped _constants_, the declared constants remain untyped and the constant identifiers
denote the constant values. For instance, if the expression is a floating-point literal, the constant
identifier denotes a floating-point constant, even if the literal's fractional part is zero.
```
const Pi float64 = 3.14159265358979323846
const zero = 0.0         // untyped floating-point constant
const (
	size int64 = 1024
	eof        = -1  // untyped integer constant
)
const a, b = 3, 4    // a = 3, b = 4 untyped integer and string constants
const u, v float32 = 0, 3    // u = 0.0, v = 3.0
```
Within a parenthesized const declaration list the expression list may be omitted from any but the first
ConstSpec. Such an empty list is equivalent to the textual substitution of the first preceding non-empty
expression list and its type if any. Omitting the list of expressions is therefore equivalent to repeating the
previous list. The number of identifiers must be equal to the number of expressions in the previous list.

## Type declarations
| NOTE |
| :--- |
| Alias declaration is not supported yet |

A type declaration binds an identifier, the _type name_, to a _type_. Type declarations come in two forms: alias
declarations and type definitions.
```
TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
TypeSpec = TypeDef .
```

## Type definitions
A type definition creates a new, distinct type with the same _underlying type_ and operations as the given type and binds an identifier, the _type name_, to it.
```
TypeDef = identifier [ TypeParameters ] Type .
```

The new type is called a _defined type_. It is _different_ from any other type, including the type it is created from.

```
type (
    integer int
    boolean bool
)
```
## Variable declarations
A variable declaration creates one or more _variables_, binds corresponding identifiers to them, and gives each a type and an initial value.

```
VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
```

```
var U, V, W int64
var k = 0
var (
	i      bool
	u, v = 2, 3
)
```
If a list of expressions is given, the variables are initialized with the expressions following the rules for 
_assignment statements_. Otherwise, each variable is initialized to its zero value.

If a type is present, each variable is given that type. Otherwise, each variable is given the type of the
corresponding initialization value in the assignment. If that value is an untyped constant, it is first
implicitly _converted_ to its _default type_; if it is an untyped boolean value, it is first implicitly iconverted
to type bool.

```
var i int = 45
var j bool = true
```
| Implementation restriction |
| :------------------------- |
| A compiler may make it illegal to declare a variable inside a function body if the variable is never used.|

## Function declarations
A function declaration binds an identifier, the _function name_, to a function.
```
FunctionDecl = "func" FunctionName Signature [ FunctionBody ] .
FunctionName = identifier .
FunctionBody = Block .
```

A function declaration without type parameters may omit the body. Such a declaration provides the signature for a function implemented outside Go, such as an assembly routine.
```
func flushICache(begin, end int)  // implemented externally
```

# Expressions
An expression specifies the computation of a value by applying operators and functions to operands.

## Operands
Operands denote the elementary values in an expression. An operand may be a literal, a (possibly _qualified_) non-_blank_ identifier denoting a _constant_, _variable_, or _function_, or a parenthesized expression.
```
Operand     = Literal | OperandName | "(" Expression ")" .
Literal     = BasicLit .
BasicLit    = int_lit .
OperandName = identifier .
```

## Primary expressions
Primary expressions are the operands for unary and binary expressions.

```
PrimaryExpr =
	Operand |
	PrimaryExpr Arguments .

Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "," ] ] ")" .
```

## Calls
Given an expression f with a core type F of function type,

```
f(a1, a2, … an)
```

calls f with arguments `a1, a2, … an`. Except for one special case, arguments must be
single-valued expressions _assignable_ to the parameter types of F and are evaluated before the
function is called. The type of the expression is the result type of F.

In a function call, the function value and arguments are evaluated in the usual order. After they
are evaluated, the parameters of the call are passed by value to the function and the called
function begins execution. The return parameters of the function are passed by value back to the
caller when the function returns.

<details><summary>unsupported case</summary>
As a special case, if the return values of a function or method g are equal in number and
individually assignable to the parameters of another function or method f, then the call
`f(g(parameters_of_g))` will invoke f after binding the return values of g to the parameters of
f in order. The call of f must contain no parameters other than the call of g, and g must have at
least one return value. If f has a final `...` parameter, it is assigned the return values of g that
remain after assignment of regular parameters.
</details>

## Operators
Operators combine operands into expressions.
```
Expression = UnaryExpr | Expression binary_op Expression .
UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .

binary_op  = rel_op | add_op | mul_op .
rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
add_op     = "+" | "-" | "|" .
mul_op     = "*" | "/" | "%" | "&" .

unary_op   = "+" | "-" | "!" | "*" | "&" .
```

Comparisons are discussed _elsewhere_. For other binary operators, the operand types must be
_identical_ unless the operation involves untyped _constants_. For operations involving
constants only, see the section on _constant expressions_.

| NOTE |
| :--- |
| Conversion is not supported yet |

If one operand is an untyped _constant_ and the other operand is not, the constant is implicitly
_converted_ to the type of the other operand.

## Operator precedence

Unary operators have the highest precedence. As the ++ and -- operators form statements, not
expressions, they fall outside the operator hierarchy.

There are five precedence levels for binary operators. Multiplication operators bind strongest,
followed by addition operators, comparison operators: 

```
Precedence    Operator
    5             *  /  %  &
    4             +  -  | 
    3             ==  !=  <  <=  >  >=
```
Binary operators of the same precedence associate from left to right. For instance, x / y * z is
the same as (x / y) * z.
```
+x
23 + 3*x
x <= f()
f() | g()
x == y+1
```

## Arithmetic operators

Arithmetic operators apply to numeric values and yield a result of the same type as the first
operand. The four standard arithmetic operators (+, -, *, /) apply to integer. The bitwise logical
apply to integers only.

```
+    sum                    integers
-    difference             integers
*    product                integers
/    quotient               integers
%    remainder              integers

&    bitwise AND            integers
|    bitwise OR             integers
```

## Integer operators
For two integer values x and y, the integer quotient q = x / y and remainder r = x % y satisfy the
following relationships:
```
x = q*y + r  and  |r| < |y|
```

with x / y truncated towards zero (["truncated division"](https://en.wikipedia.org/wiki/Modulo_operation)).
```
 x     y     x / y     x % y
 5     3       1         2
-5     3      -1        -2
 5    -3      -1         2
-5    -3       1        -2
```

The one exception to this rule is that if the dividend x is the most negative value for the int
type of x, the quotient q = x / -1 is equal to x (and r = 0) due to two's-complement _integer
overflow_:
```
                         x, q
int8                     -128
int16                  -32768
int32             -2147483648
int64    -9223372036854775808
```
If the divisor is a constant, it must not be zero. If the divisor is zero at run time, a run-time
panic occurs.

For integer operands, the unary operators +, -, and ^ are defined as follows:
```
+x                          is 0 + x
-x    negation              is 0 - x
```
## Integer overflow
For _unsigned integer_ values, the operations +, -, and * are computed modulo 2n, where n is the
bit width of the unsigned integer's type. Loosely speaking, these unsigned integer operations
discard high bits upon overflow, and programs may rely on "wrap around".

For signed integers, the operations +, -, *, / may legally overflow and the resulting
value exists and is deterministically defined by the signed integer representation, the operation,
and its operands. Overflow does not cause a _run-time panic_. A compiler may not optimize code
under the assumption that overflow does not occur. For instance, it may not assume that x < x + 1
is always true.

## Comparison operators

Comparison operators compare two operands and yield an untyped boolean value.
```
==    equal
!=    not equal
<     less
<=    less or equal
>     greater
>=    greater or equal
```

In any comparison, the first operand must be _assignable_ to the type of the second operand, or
vice versa.

The equality operators == and != apply to operands that are _comparable_. The ordering operators <,
<=, >, and >= apply to operands that are _ordered_. These terms and the result of the comparisons
are defined as follows:

- Boolean values are comparable. Two boolean values are equal if they are either both true or both false.
- Integer values are comparable and ordered, in the usual way.
```
const c = 3 < 4            // c is the untyped boolean constant true

type MyBool bool
var x, y int
var (
	// The result of a comparison is an untyped boolean.
	// The usual assignment rules apply.
	b3        = x == y // b3 has type bool
	b4 bool   = x == y // b4 has type bool
	b5 MyBool = x == y // b5 has type MyBool
)
```

## Logical operators
Logical operators apply to _boolean_ values and yield a result of the same type as the operands.
The right operand is evaluated conditionally.

```
!     NOT                !p      is  "not p"
&&    conditional AND    p && q  is  "if p then q else false"
||    conditional OR     p || q  is  "if p then true else q"
```

</details>

## Address operators
| NOTE | 
| :--- |
| This is added to understand _addressable_|

For an operand x of type T, the address operation &x generates a pointer of type *T to x. The
operand must be _addressable_, that is, a variable.

## Constant expressions
Constant expressions may contain only _constant_ operands and are evaluated at compile time.

Untyped boolean and numeric constants may be used as operands wherever it is legal to use an
operand of boolean and numeric type, respectively.

A constant comparison always yields an untyped boolean constant.

Any other operation on untyped constants results in an untyped constant of the same kind; that is, a
boolean and integer constant. If the untyped operands of a binary
operation are of different kinds, the result is of the operand's kind that
appears later in this list: integer.

```
const a = 2 + 3.0          // a == 5.0   (untyped floating-point constant)
const b = 15 / 4           // b == 3     (untyped integer constant)
const c = 15 / 4.0         // c == 3.75  (untyped floating-point constant)
const Θ float64 = 3/2      // Θ == 1.0   (type float64, 3/2 is integer division)
const Π float64 = 3/2.     // Π == 1.5   (type float64, 3/2. is float division)
const j = true             // j == true  (untyped boolean constant)
```
Constant expressions are always evaluated exactly; intermediate values and the constants themselves
may require precision significantly larger than supported by any predeclared type in the language.
The following are legal declarations:
| NOTE |
| :--- |
| Shift operator is not supported yet |

```
const Huge = 1 << 100         // Huge == 1267650600228229401496703205376  (untyped integer constant)
const Four int8 = Huge >> 98  // Four == 4                                (type int8)
```

The divisor of a constant division or remainder operation must not be zero:
```
3.14 / 0.0   // illegal: division by zero
```
The values of typed constants must always be accurately _representable_ by values of the constant
type. The following constant expressions are illegal:
| NOTE |
| :--- |
| Conversions are not supported yet |

```
uint(-1)     // -1 cannot be represented as a uint
int(3.14)    // 3.14 cannot be represented as an int
int64(Huge)  // 1267650600228229401496703205376 cannot be represented as an int64
Four * 300   // operand 300 cannot be represented as an int8 (type of Four)
Four * 100   // product 400 cannot be represented as an int8 (type of Four)
```

| Implementation restriction |
| :------------------------- |
| A compiler may use rounding while computing untyped floating-point or complex constant expressions; see the implementation restriction in the section on _constants_. This rounding may cause a floating-point constant expression to be invalid in an integer context, even if it would be integral when calculated using infinite precision, and vice versa. |

## Order of evaluation
| NOTE |
| :--- |
| Needs discussion |

When evaluating the operands of an expression, assignment, or return
statement, all function calls are evaluated in lexical left-to-right order.

For example, in the (function-local) assignment
```
y, ok = g(h(), i()+x+j(), c), k()
```

the function calls happen in the order `h()`, `i()`, `j()`, `g()`, and `k()`.

In the expression `x + (y + z)` the addition `y + z` is performed before adding `x`.

# Statements
Statements control execution.
```
Statement =
	Declaration | SimpleStmt |
	ReturnStmt  | Block | IfStmt .

SimpleStmt = EmptyStmt | ExpressionStmt | IncDecStmt | Assignment  
```
<details><summary>unsupported statements</summary>

```
GoStmt | LabeledStmt | ContinueStmt | GotoStmt | FallThroughStmt 
| SwitchStmt | SelectStmt | ForStmt | DeferStmt
```

</details>

## Terminating statements

A _terminating_ statement interrupts the regular flow of control in a _block_. The following
statements are terminating:
1. A "return" statement.
2. A _block_ in which the statement list ends in a terminating statement.
3. An _"if" statement_ in which:
    - the "else" branch is present, and
    - both branches are terminating statements.

All other statements are not terminating.

A _statement list_ ends in a terminating statement if the list is not empty and its final non-empty
statement is terminating.

## Empty statements
The empty statement does nothing.
```
EmptyStmt = .
```

## Expression statements
function calls can appear in statement context. Such statements may be parenthesized.
```
ExpressionStmt = Expression .
```

## IncDec statements

The "++" and "--" statements increment or decrement their operands by the untyped _constant_ 1.
As with an assignment, the operand must be _addressable_.
```
IncDecStmt = Expression ( "++" | "--" ) .
```

The following _assignment statements_ are semantically equivalent:
```
IncDec statement    Assignment
x++                 x += 1
x--                 x -= 1
```
## Assignment statements

An assignment replaces the current value stored in a variable with a new value specified by an
expression. An assignment statement may assign a single value to a single variable, or multiple
values to a matching number of variables.
```
Assignment = ExpressionList assign_op ExpressionList .

assign_op = "=" .
```

Each left-hand side operand must be _addressable_. Operands may be parenthesized.
```
x = 1
y, (z) = 2, 3   // same as y, z = 2, 3
```

A tuple assignment assigns the individual elements of a multi-valued operation to a list of
variables. There are two forms. In the first, the right hand operand is a single multi-valued
expression such as a function call. The number of operands on the left hand side must match the
number of values. For instance, if f is a function returning two values,
```
x, y = f()
```
assigns the first value to x and the second to y. In the second form, the number of operands on the
left must equal the number of expressions on the right, each of which must be single-valued, and the
_n_th expression on the right is assigned to the _n_th operand on the left:
```
one, two, three = 'I', 'II', 'III'
```

The assignment proceeds in two phases. First, the expressions on the right are all _evaluated in the
usual order_. Second, the assignments are carried out in left-to-right order.
```
a, b = b, a  // exchange a and b
x, y = 4 * 4, 3
i = 0
```
In assignments, each value must be _assignable_ to the type of the operand to which it is assigned.

## If statements

"If" statements specify the conditional execution of two branches according to the value of a boolean
expression. If the expression evaluates to true, the "if" branch is executed, otherwise, if present,
the "else" branch is executed.
```
IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
```
```
if x > max {
	x = max
}
```

The expression may be preceded by a simple statement, which executes before the expression is evaluated.

```
if x := f(); x < y {
	return x
} else if x > z {
	return z
} else {
	return y
}
```

## Return statements
A "return" statement in a function F terminates the execution of F, and optionally provides one or more result values.
```
ReturnStmt = "return" [ ExpressionList ] .
```

In a function without a result type, a "return" statement must not specify any result values.
```
func noResult() {
	return
}
```

There are two ways to return values from a function with a result type:
1. The return value or values may be explicitly listed in the "return" statement. Each expression
must be single-valued and assignable to the corresponding element of the function's result type.

```
func simpleF() int {
	return 2
}

func complexF1() (float64, float64) {
	return -7.0, -4.0
}
```

2. The expression list in the "return" statement may be a single call to a multi-valued function. 
The effect is as if each value returned from that function were assigned to a temporary variable with
the type of the respective value, followed by a "return" statement listing these variables, at which
point the rules of the previous case apply.

```
func complexF2() (float64, float64) {
	return complexF1()
}
```

# Program initialization and execution
## The zero value
When storage is allocated for a _variable_, through a declaration and no explicit
initialization is provided, the variable or value is given a default value. Each element of such a
variable or value is set to the _zero value_ for its type: `false` for booleans, `0` for numeric
types. 

These two simple declarations are equivalent:
```
var i int
var i int = 0
```

## Program execution
| NOTE | 
| :--- |
| Packages are not supported yet |

A complete program is created by linking a single, unimported package called the _main_ package with
all the packages it imports, transitively. The main package must have package name main and declare
a function main that takes no arguments and returns no value.
```
func main() { … }
```

Program execution begins by initializing the main package and then invoking the function main. When
that function invocation returns, the program exits. It does not wait for other (non-main)
goroutines to complete.

## Run-time panics
| TODO |
| :--- |
| Discussion |

Execution errors such as attempting to index an array out of bounds trigger a run-time panic
equivalent to a call of the built-in function panic with a value of the implementation-defined
interface type runtime.Error. That type satisfies the predeclared interface type error. The exact
error values that represent distinct run-time error conditions are unspecified.

```
package runtime

type Error interface {
	error
	// and perhaps other methods
}
```
