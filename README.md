psData
==========

### Christopher Gandrud

### Version 0.1

---

This [R](http://www.r-project.org/) package includes functions for gathering commonly used and regularly maintained data set in political science. It also includes functions for combining components from these data sets into variables that have been suggested in the literature, but are not regularly maintained. 

Functions include:

- `PolityGet`: a function to download the [Polity IV](http://www.systemicpeace.org/polity/polity4.htm) data set. It keeps specified variables and creates a standard country ID variable that can be used for merging the data with other data sets.

- `DpiGet`: a function to download the [Database of Political Institutions](http://go.worldbank.org/2EAGGLRZ40) data set. It keeps specified variables and creates a standard country ID variable that can be used for merging the data with other data sets.

- `WinsetCreator`: Creates the winset (W) and a modified version of the selectorate (S) variable from [Bueno de Mesquita et al. (2003)](http://www.nyu.edu/gsas/dept/politics/data/bdm2s2/Logic.htm) using the most recent data available from Polity IV and the Database of Political Institutions.

---

## Examples 

To create **winset** and **selectorate** data use the following code:

```
WinData <- WinsetCreator()

head(WinData)

##   iso2c     country year    W ModS
## 1     AF Afghanistan 1975 0.25    0
## 2     AF Afghanistan 1976 0.25    0
## 3     AF Afghanistan 1977 0.25    0
## 15    AF Afghanistan 1989 0.50    0
## 16    AF Afghanistan 1990 0.50    0
## 17    AF Afghanistan 1991 0.50    0

```

--- 

## Suggestions

Please feel free to suggest other data set downloading and variable creating functions. To do this just leave a note on the package's [Issues page]()

---

## Install

**pdData** will be on [CRAN](http://cran.r-project.org/) soon, but while it is in the development stage you can install it with the [devtools](https://github.com/hadley/devtools) package:

```
devtools::install_github('psData', 'christophergandrud')
```