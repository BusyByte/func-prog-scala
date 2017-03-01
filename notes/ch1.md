# What is functional programming?

pure functions - functions without side effects

something other than return a result

examples of side effects
- Modifying a variable
- Modifying a data structure in place
- Setting a field on an object
- Throwing an exception or halting with an error  Printing to the console or reading user input
- Reading from or writing to a file
- Drawing on the screen

restriction on `how` we write programs
not `what` we can express

modularity

## 1.1 The benefits of FP: a simple example

### 1.1.1 A program with side effects

### 1.1.1 Removing side effects

can call buyCoffee multiple times and fold charges together to charge card once
 
can group by credit card

must actually have a side effect at some point

## 1.2 Exactly what is a (pure) function?

A => B

No observable side effects other than producing result (B)

referential transparency

property of expressions and not just functions

could replace the call of the function with the result without changing meaning of the program

## 1.3 Referential transparency, purity, and the substitution model

`substitution model`

RT enables `equational reasoning`

`local reasoning` - don't need to mentally track code before or after our function to tell what 
the function does

modular programs

composable

