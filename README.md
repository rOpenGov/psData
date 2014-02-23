psData
==========

### Christopher Gandrud

### Version 0.1.2

---

This [R](http://www.r-project.org/) package includes functions for gathering commonly used and regularly maintained political science data sets. It also includes functions for combining components from these data sets into variables that have been suggested in the political science literature, but are not regularly updated. 

*psData* includes two primary function types: **Getters** and **Variable Builders**. Getter functions automate the gathering and cleaning of particular data sets so that they can easily be merged with other data. They do not transform the underlying data. Variable Builders use Getters to gather data and then transform it into new variables suggested by the political science literature. The functions currently part of *psData* include:

#### Getters

- `DpiGet`: a function to download the [Database of Political Institutions](http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/0,,contentMDK:20649465~pagePK:64214825~piPK:64214943~theSitePK:469382,00.html) data set. It keeps specified variables and creates a standard country ID variable that can be used for merging the data with other data sets.

- `PolityGet`: a function to download the [Polity IV](http://www.systemicpeace.org/polity/polity4.htm) data set. It keeps specified variables and creates a standard country ID variable that can be used for merging the data with other data sets.

- `RRCrisisGet`: download and combine [Reinhart and Rogoff's (2010)](http://www.carmenreinhart.com/data/browse-by-topic/topics/7/) crisis dummy variables into one data frame.

#### Variable Builders

- `WinsetCreator`: Creates the winset (W) and a modified version of the selectorate (S) variable from [Bueno de Mesquita et al. (2003)](http://www.nyu.edu/gsas/dept/politics/data/bdm2s2/Logic.htm) using the most recent data available from Polity IV and the Database of Political Institutions.

#### Others

Other functions included that might be useful to people working with political science data:

- `CountryID`: Function for creating standardised country names and ID variables. This builds on [countrycode](https://github.com/vincentarelbundock/countrycode) and includes extra capabilities for reporting and dealing with duplicates.

---

## Updates

Most of the Getter functions currently included in *psData* download data from a specific URL that links to a data file. Hopefully, the data sets' authors will keep their data up-to-date. When they make updates, they will likely link to the updated file with a new URL. All of the functions in *psData* that gather data from a file at a specific URL allow the user to specify a new URL, if they want to. 

If you notice an updated version of one of the data sets, feel free to submit a [Pull Request](https://help.github.com/articles/using-pull-requests) with the new URL. It would be great if you make sure that the function still works, as the data set's authors may change the format breaking the Getter function. 

## Suggestions

Please feel free to suggest other data set downloading and variable creating functions. To do this just leave a note on the package's [Issues page](https://github.com/christophergandrud/psData/issues)

---

## Examples 

To create **winset** (**W**) and **selectorate** (**ModS**) data use the following code:


```r
library(psData)

WinData <- WinsetCreator()

head(WinData)
```

```
##    iso2c     country year    W ModS
## 1     AF Afghanistan 1975 0.25    0
## 2     AF Afghanistan 1976 0.25    0
## 3     AF Afghanistan 1977 0.25    0
## 15    AF Afghanistan 1989 0.50    0
## 16    AF Afghanistan 1990 0.50    0
## 17    AF Afghanistan 1991 0.50    0
```


Note that the **iso2c** variable refers to the [ISO two letter country code country ID](http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2). This can be changed with the `OutCountryID` argument.

To download only the **polity2** variable from [Polity IV](http://www.systemicpeace.org/polity/polity4.htm):


```r
PolityData <- PolityGet(vars = "polity2")

head(PolityData)
```

```
##   iso2c     country year polity2
## 1    AF Afghanistan 1800      -6
## 2    AF Afghanistan 1801      -6
## 3    AF Afghanistan 1802      -6
## 4    AF Afghanistan 1803      -6
## 5    AF Afghanistan 1804      -6
## 6    AF Afghanistan 1805      -6
```


---

## Install

**pdData** will be on [CRAN](http://cran.r-project.org/) soon, but while it is in the development stage you can install it with the [devtools](https://github.com/hadley/devtools) package:

```
devtools::install_github('psData', 'christophergandrud')
```
