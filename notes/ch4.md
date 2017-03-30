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


