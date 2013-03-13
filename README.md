# DataFrameConst

This **R** package defines two S4 classes 

- `HomogList`: a list in which all elements must be the same class
- `DataFrameConst`: a data frame with optional required columns and 
  classes, or general constraints.

It also defines the most common methods
`[<-`, `[[<-`, `$<-`, `c`, `cbind2`, `rbind2` for these classes so that the constraints
are checked when data in the objects are updated.

