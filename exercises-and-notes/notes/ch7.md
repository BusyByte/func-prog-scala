# Purely functional parallel

separating the concern of `describing` a computation from actually `running` it

`absolutely no side effects`

## 7.1 Choosing data types and functions

### 7.1.1 A data type for parallel computations

### 7.1.2 Combining parallel computations

### 7.1.3 Explicit forking

`Par itself doesn’t need to know how to actually implement the parallelism. It’s more a description of a parallel computation that gets interpreted at a later time by something like the get function`

`program` which can be ran

## 7.2 Picking a representation

## 7.3 Refining the API

derive map in terms of map2

lift

## 7.4 The algebra of an API

### 7.4.1 The law of mapping

identity law