# Parser combinators

`algebraic design`

## 9.1 Designing an algebra, first

`algebra` - a collection of functions operating over some data type(s), along with a set of laws specifying relationships between these functions

`algebraic design` - start with the algebra (including its laws) and decide on a representation later

## 9.2 A possible algebra

`structure preserving`

### 9.2.1 Slicing and nonempty repetition

Exercise 9.5 It seems like overkill to introduce something like Par/Future/Task or actors

## 9.3 Handling context sensitivity

## 9.4 Writing a JSON parser

### 9.4.1 The JSON format

### 9.4.2 A JSON parser

## 9.5 Error Reporting

### 9.5.1 A possible design

### 9.5.2 Error nesting   

### 9.5.3 Controlling branching and backtracking

Exercise 9.11 It seems like the parser index or the order in the chain of parsers might be helpful 
if there are multiple patterns with slight differences

## 9.6 Implementing the algebra

### 9.6.1 One possible implementation

## 9.6.2 Sequencing parsers

### 9.6.3 Labeling parsers

### 9.6.4 Failover and backtracking

### 9.6.5 Context-sensitive parsing


