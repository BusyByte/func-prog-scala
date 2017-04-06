# Getting started with FP in Scala

`tail recursive functions`

`higher order functions`

`polymorphic HOFs`

## 2.1 Introducing Scala the language: an example

`object` - singleton type

left of equal sign is `left-handed side` or `signature`

after equal sign is `right-handed side` or `definition`

`block` is statements in braces

`val` is immutable

main method

## 2.2 Running our program

`sbt`

`scalac`

`scala`

## 2.3 Modules, objects, and namespaces

`objects` which serve to namespace called a module

no notion of `operators` - just methods which can be mathematical like `+`

importing `module` code into scope

## 2.4 Higher-order functions: passing functions to functions

`functions are values`

### 2.4.1 Writing loops recursively

`self-recursion`

`tail position`

`tail call elimination`

`@tailrec annotation` - compile error if fails to eliminate tail calls

### 2.4.2 Writing our first higher-order function

function as parameter can pass `abs` or `factorial` functions

Variable-naming conventions like `f`, `g` or `h` because types important and not what function does

## 2.5 Polymorphic functions: abstracting over types

`monomorphic function` - work on one type of data

`polymorphic functions` - usually when implementing HOFs, code which works for `any` type 

### 2.5.1 An example of a polymorphic function

`generic function`

`abstracting over the type`

`type paremeters` - in square brackets, normally like `A`, `B` or `C`

### 2.5.2 Calling HOF with anonymous functions

`function literal` or `anonymous function`

## 2.6 Following types to implementations

`currying`

`function composition`

`f andThen g` is the same as `g compose f`
 
 compose is same as `g . f` or `g after f`
 
 
