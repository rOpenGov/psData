`psdata` (a very early draft) manipulates panel/time series data frames. It is built on top of Jeffrey Arnold's [DataFrameConstr](https://github.com/jrnold/DataFrameConstr) and accepts the same arguments, plus two optional lists of settings:

* `design` to specify a panel design for the data
* `meta` to add metadata (authors, URL, etc.)

[goal] The package understands country codes from `countrycode` (ISO, COW), region codes (NUTS-2/3), U.S. states, and dates in all formats supported by `lubridate`. It can deal with other forms of dyadic data with multilevel designs.

All objects created by `psdata` are manipulable as data frames.

```{S}
# install
devtools::install_github("briatte/psData")
library(psData)

# example
data(debt)
head(debt)
    Country Year    growth     ratio
1 Australia 1946 -3.557951 190.41908
2 Australia 1947  2.459475 177.32137
3 Australia 1948  6.437534 148.92981
4 Australia 1949  6.611994 125.82870
5 Australia 1950  6.920201 109.80940
6 Australia 1951  4.272612  87.09448

# convert
debt = psData(debt,
              design = list(
                # data design
                panel = c("Country"),
                format = c(Country = "name"),
                time = c("Year"),
                date = c(Year = "%Y")
              ),
              meta = list(
                # descriptive
                name = "Reinhart and Rogoff data, edited from Herndon et al.'s script.",
                # similar to R packages
                meta = c(aut = "Cosma Shalizi"),
                # versioning
                date = "2013-04-29", 
                version = "",
                url = "http://www.stat.cmu.edu/~cshalizi/uADA/13/hw/11/debt.csv"
              ))

# example show
head(debt)
Reinhart and Rogoff data, edited from Herndon et al.'s script. 
Panel data frame [ 6 rows x  4 columns, 1 Country x 6 Year ]

    Country Year    growth     ratio
1 Australia 1946 -3.557951 190.41908
2 Australia 1947  2.459475 177.32137
3 Australia 1948  6.437534 148.92981
4 Australia 1949  6.611994 125.82870
5 Australia 1950  6.920201 109.80940
6 Australia 1951  4.272612  87.09448

Source: Cosma Shalizi  2013-04-29 
 http://www.stat.cmu.edu/~cshalizi/uADA/13/hw/11/debt.csv
 
# example subset
summary(debt[1:10, ])
      Country        Year          growth           ratio       
 Australia:10   Min.   :1946   Min.   :-3.558   Min.   : 74.98  
 Austria  : 0   1st Qu.:1948   1st Qu.: 2.624   1st Qu.: 81.42  
 Belgium  : 0   Median :1950   Median : 4.867   Median : 98.45  
 Canada   : 0   Mean   :1950   Mean   : 3.885   Mean   :115.72  
 Denmark  : 0   3rd Qu.:1953   3rd Qu.: 6.382   3rd Qu.:143.15  
 Finland  : 0   Max.   :1955   Max.   : 6.920   Max.   :190.42  
 (Other)  : 0      
```