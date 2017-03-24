# Functional data structures

`pattern-matching`

## 3.1 Defining functional data structures

functional data structures are by definition immutable

sealed trait

`sealed` all implementations in this file

`data constructors` of Cons (short for construct) and Nil

pattern used for `pattern matching`

`+` used with type parameter A indicates covariant

invariant if no `+` or `-`

## 3.2 Pattern matching

fancy `switch`

`pattern`

companion object - module

## 3.3 Data sharing in functional data structures

reusing data since immutable instead of copying is `data sharing`

### 3.3.1 The efficiency of data sharing

### 3.3.2 Improving type inference for higher-order functions

## 3.4 Recursion over lists and generalizing to higher-order functions
 
### 3.4.1 More functions for working with lists

### 3.4.2 Loss of efficiency when assembling list functions from simpler components

## 3.5 Trees

List is an example of an ADT (Algebraic Data Type) 

sum type or union of data constructors

each data constructor is product of it's arguments

