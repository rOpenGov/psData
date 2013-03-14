# DataFrameConst

This **R** package defines two S4 classes 

- `HomogList`: a list in which all elements must be the same class
- `DataFrameConst`: a data frame with optional required columns and 
  classes, or general constraints.

It also defines the most common methods
`[<-`, `[[<-`, `$<-`, `c`, `cbind2`, `rbind2` for these classes so that the constraints
are checked when data in the objects are updated.

## HomogList

Create a list in which all elements must be functions


```r
library("DataFrameConstr")
foo <- HomogList(list(sum = sum, max = max, min = min), "function")
print(foo)
```

```
## List of "function" objects:
## [[1]]
## function (..., na.rm = FALSE)  .Primitive("sum")
## 
## [[2]]
## function (..., na.rm = FALSE)  .Primitive("max")
## 
## [[3]]
## function (..., na.rm = FALSE)  .Primitive("min")
```


Since `HomogList` extends `list`, it can be used like any other list.

```r
x <- 1:10
lapply(foo, function(f) f(x))
```

```
## $sum
## [1] 55
## 
## $max
## [1] 10
## 
## $min
## [1] 1
```


It can be updated,

```r
foo[["mean"]] <- mean
print(foo)
```

```
## List of "function" objects:
## [[1]]
## function (..., na.rm = FALSE)  .Primitive("sum")
## 
## [[2]]
## function (..., na.rm = FALSE)  .Primitive("max")
## 
## [[3]]
## function (..., na.rm = FALSE)  .Primitive("min")
## 
## [[4]]
## function (x, ...) 
## UseMethod("mean")
## <bytecode: 0x1b986f8>
## <environment: namespace:base>
```

but the object will return an error if an element other than the specified
class name is returned.

```r
foo[["a"]] <- 1
```

```
## Error: invalid class "HomogList" object: Not all elements have class
## function
```

The methods
`[<-`, `[[<-`, `$<-` and `c` are all defined to return `HomogList` objects, 
and by extension, check the class types of the elements in the new list.

The function `subclass_homog_list` can be used to create subclasses of
`HomogList` for a specified class. The function `subclass_homog_list`, 
will create the class and all its associated methods, and return a
function which creates new objects of that class.

For example, the following creates a new class "FunctionList", in
which all elements must be `function` objects.

```r
FunctionList <- subclass_homog_list("FunctionList", "function")
```


Then a new object of class `FunctionList` can be created either by

```r
FunctionList(list(sum = sum, mean = mean))
```

```
## List of "function" objects:
## [[1]]
## function (..., na.rm = FALSE)  .Primitive("sum")
## 
## [[2]]
## function (x, ...) 
## UseMethod("mean")
## <bytecode: 0x1b986f8>
## <environment: namespace:base>
```

or, more verbosely,

```r
new("FunctionList", list(sum = sum, mean = mean))
```

```
## List of "function" objects:
## [[1]]
## function (..., na.rm = FALSE)  .Primitive("sum")
## 
## [[2]]
## function (x, ...) 
## UseMethod("mean")
## <bytecode: 0x1b986f8>
## <environment: namespace:base>
```


What is important about this class is that it will not accept any
non-function elements either on creation,

```r
FunctionList(list(a = 1))
```

```
## Error: invalid class "FunctionList" object: Not all elements have class
## function
```

or when updating an existing object,

```r
foo <- FunctionList(list(sum = sum, mean = mean))
foo[["a"]] <- 1
```

```
## Error: invalid class "HomogList" object: Not all elements have class
## function
```


This makes classes extending `HomogList` particularly useful with S4
objects, either to define lists of S4 objects, or as the slot class
for a class definition.

For example, in the **coda** package, `mcmc.list` is a S3 class
consisting of a list of `mcmc` objects. An equivalent S4 class, which
I'll call `NewMcmcList`, could be created with one function call, 
```r
NewMcmcList <- subclass_homog_list("NewMcmcList", "mcmc.list") 
```

## DataFrameConstr

The `DataFrameConstr` class extends the `data.frame` class, but allows
for required columns with specified classes, and for general
constraints on the `data.frame`. 

For example, let's create a data frame which must have an `numeric` column named
`"a"`, a column named `"b"`, which can be of any class, and a
`"factor` column named `"c"`. Additionally, require that all
values of `"a"` are positive.


```r
foo <- DataFrameConstr(data.frame(a = runif(3), b = runif(3), c = letters[1:3]), 
    columns = c(a = "numeric", b = "ANY", c = "factor"), constraints = list(function(x) {
        x$a > 0
    }))
```


The new object `foo` acts just like any other `data.frame`,

```r
print(foo)
```

```
##         a      b c
## 1 0.03765 0.6625 a
## 2 0.33810 0.4121 b
## 3 0.32484 0.2429 c
```

```r
summary(foo)
```

```
##        a                b         c    
##  Min.   :0.0376   Min.   :0.243   a:1  
##  1st Qu.:0.1812   1st Qu.:0.328   b:1  
##  Median :0.3248   Median :0.412   c:1  
##  Mean   :0.2335   Mean   :0.439        
##  3rd Qu.:0.3315   3rd Qu.:0.537        
##  Max.   :0.3381   Max.   :0.662
```


However, it will validate updates to ensure that the data meets the specified constraints,
This will return an error because `a` was defined as `numeric`,

```r
foo$a <- as.character(foo$a)
```

```
## Error: invalid class "DataFrameConstr" object: column a does not inherit
## from numeric
```

This returns an error because `a` is constrained to be strictly positive,

```r
foo["a", 1] <- -1
```

```
## Error: invalid class "DataFrameConstr" object: Constraint failed: function
## (x) { x$a > 0 }
```

```r
# Unfortunately, this syntax, does not work, and alters foo foo[['a']][1]
# <- -1 I can't figure out how to avoid that, so if anyone knows, can you
# let me know?
```


This will not cause an error because the column `b` is allowed to have any class (more formally, 
it is of class "ANY"),

```r
foo$b <- as.character(foo$b)
```


Since `foo` was created with `exclusive=FALSE` (by default) then the data frame can contain more rows 
than `a`, `b`, and `c`. The following is valid,

```r
foo$d <- runif(3)
```

However, `foo` is guaranteed to always contain columns `a`, `b`, and `c`, and thus these columns cannot 
be deleted. This will return an error,

```r
foo$a <- NULL
```

```
## Error: invalid class "DataFrameConstr" object: column a not in 'object'
```


The methods
`[<-`, `[[<-`, `$<-`, `cbind2` (use instead of `cbind`), and `rbind2` (use instead of `rbind`), are defined so that they return `DataFrameConstr` objects, and by extension check the column classes, and constraints of the new object.

The function `constrained_data_frame` can be used to create subclasses
of `DataFrameConstr`. For example, to create a class, which I'll call `"Foo"`, which 
has the same columns and constraints as the `foo` object previously created,

```r
Foo <- constrained_data_frame("Foo", columns = c(a = "numeric", b = "ANY", c = "factor"), 
    constraints = list(function(x) {
        x$a > 0
    }))
```

Now there is a new class, `"Foo"`, which inherits from `DataFrameConstr`,

```r
showClass("Foo")
```

```
## Class "Foo" [in ".GlobalEnv"]
## 
## Slots:
##                                                                   
## Name:                .Data             columns           exclusive
## Class:                list           character             logical
##                                                                   
## Name:          constraints               names           row.names
## Class:        FunctionList           character data.frameRowLabels
##                           
## Name:             .S3Class
## Class:           character
## 
## Extends: 
## Class "DataFrameConstr", directly
## Class "data.frame", by class "DataFrameConstr", distance 2
## Class "list", by class "DataFrameConstr", distance 3
## Class "oldClass", by class "DataFrameConstr", distance 3
## Class "vector", by class "DataFrameConstr", distance 4
```


Then create a new object, `bar` of class `Foo`,

```r
bar <- Foo(data.frame(a = runif(3), b = runif(3), c = letters[1:3]))
```

```
## [1] "not missing"
##         a      b c
## 1 0.11581 0.1650 a
## 2 0.09174 0.1191 b
## 3 0.98917 0.7465 c
```

This new object will validate any new data, so the following will produce errors,

```r
bar[["a"]] <- as.character(bar[["a"]])
```

```
## Error: invalid class "DataFrameConstr" object: column a does not inherit
## from numeric
```

```r
bar[["a"]] <- -1
```

```
## Error: invalid class "DataFrameConstr" object: Constraint failed: function
## (x) { x$a > 0 }
```

```r
bar[["a"]] <- NULL
```

```
## Error: invalid class "DataFrameConstr" object: column a not in 'object'
```


This will validate the object on creation, so the following will return an error, 
because it does not contain the columns `b` or `c`,

```r
Foo(data.frame(a = runif(3)))
```

```
## [1] "not missing"
##        a
## 1 0.3689
## 2 0.5876
## 3 0.9814
```

```
## Error: invalid class "Foo" object: column b not in 'object'
```


The additional capabilities that `DataFrameConstr` adds to
`data.frames` make it useful for the following,

- slot class types within S4 objects
- data validation
- creating an **R** ORM to databases



<!--  LocalWords:  runif DataFrameConst HomogList cbind rbind lapply
 -->
<!--  LocalWords:  DataFrameConstr HomogClass homog FunctionList mcmc
 -->
<!--  LocalWords:  NewMcmcList showClass ORM
 -->
