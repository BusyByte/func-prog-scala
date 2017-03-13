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
 
 