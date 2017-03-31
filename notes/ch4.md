# Handling errors without exceptions

RT with `consolidation` of `error handling`

## 4.1 The good and bad aspects of exceptions

RT - does not depend on context

`Exceptions break RT and introduce context dependence`

`Exceptions are not type-safe`

`Consolidate and centralize error-handling logic`

we'll use `completely type-safe` strategy

## 4.2 Possible alternatives to exceptions

mean function with a default value if illegal computation

## 4.3 The Option data type

I don't like option as a return type normally to indicate errors

Option better than `Sentinel` value (special value)

Better to have an Xor

turn mean from a `partial function` into a `total function`

### 4.3.1 Usage patterns for Option

if methods on collections and such which return an Option then use them (eg Map.get)

Options can be converted to Lists

getOrElse

lookupByName("Joe") is proper usage of Option
because there may be nobody named Joe
it does not indicate an error/throwable condition though

common idiom `o.getOrElse(throw new Exception("FAIL"))` if no reasonable program would ever catch it

### 4.3.2 Option composition, lifting, and wrapping exception-oriented APIs

lift A => B produces Option[A] => Option[B]

never need modify existing functions to make them Option aware

sequence and traverse

for comps - sugar for flatmap and map

`Between map, lift, sequence, traverse, map2, map3, and so on, you should never have to modify any existing functions to work with optional values`

nice for Option but what about other things in Cats/ScalaZ don't seem to have this quality/goal

## 4.4 The Either data type

Option doesn't tell us what went wrong

Left is used by convention for Error as the word `Left` has an `E` in it

Seems like distorting a disjunction for exception handling

If exception in left same as Try only try has better naming, Success/Failure vs Left/Right

talks about operating on right but doesn't really talk about left vs right bias

talks about being able to use Either in for comps but not that the standard Scala Either can't

need to be careful about sequence and traverse if effectful because it short circuits/stops on first Left it sees

Exercise 4.8 - If we have the left be a list structure and not short circuit
Something like ValidatedNEL
Applicative for with grouping (Monoid?) of Lefts into List?


