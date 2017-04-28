# Purely functional state

`random number generator`

## 6.1 Generating random numbers using side effects

`eschew side effects on principle`

## 6.2 Purely functional random number generation

`we separate the concern of computing what the next state is from the concern of communicating the new state to the rest of the program`

also potentially dangerous if you don't realize the original isn't mutated and need to use the updated one

## 6.3 Making stateful APIs pure

## 6.4 A better API for state actions

`type Rand[+A] = RNG => (A, RNG)`

## 6.5 A general state action data type

`type State[S,+A] = S => (A,S)`

`case class State[S,+A](run: S => (A,S))`

`type Rand[A] = State[RNG, A]`

## 6.6 Purely functional imperative programming

for-comprehension

modify 

get 

set



