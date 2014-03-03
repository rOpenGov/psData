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
```

This data so far conform to the `data.frame` method:

```
> head(debt)
    Country Year    growth     ratio
1 Australia 1946 -3.557951 190.41908
2 Australia 1947  2.459475 177.32137
3 Australia 1948  6.437534 148.92981
4 Australia 1949  6.611994 125.82870
5 Australia 1950  6.920201 109.80940
6 Australia 1951  4.272612  87.09448
```

We now convert the data to `psData`:

```
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
```

By default, a `psData` object shows its panel settings, the head of the data and its source:

```
> debt
Reinhart and Rogoff data, edited from Herndon et al.'s script. 
Panel data frame [ 1171 rows x  4 columns, 20 Country x 64 Year ]

    Country Year    growth     ratio
1 Australia 1946 -3.557951 190.41908
2 Australia 1947  2.459475 177.32137
3 Australia 1948  6.437534 148.92981
4 Australia 1949  6.611994 125.82870
5 Australia 1950  6.920201 109.80940
6 Australia 1951  4.272612  87.09448

Source: Cosma Shalizi  2013-04-29 
 http://www.stat.cmu.edu/~cshalizi/uADA/13/hw/11/debt.csv
```

All `data.frame` operations are supported:

```
> summary(debt[sample(1:nrow(debt), nrow(debt) %/% 2), "growth"])
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -9.922   1.934   3.315   3.454   5.059  15.220
> subset(debt, growth < -5)
Reinhart and Rogoff data, edited from Herndon et al.'s script. 
Panel data frame [ 9 rows x  4 columns, 7 Country x 5 Year ]

    Country Year    growth     ratio
352 Finland 1991 -6.243667  16.90087
370 Finland 2009 -6.355948  33.41948
483 Germany 2009 -5.296834  43.71877
586 Ireland 2009 -7.500000  44.37137
645   Italy 2009 -5.145000  97.13512
699   Japan 2009 -5.369365 181.51101

Source: Cosma Shalizi  2013-04-29 
 http://www.stat.cmu.edu/~cshalizi/uADA/13/hw/11/debt.csv
> print(subset(debt, growth < -7.5))
         Country Year     growth     ratio
755  New Zealand 1948  -9.922099 117.20596
758  New Zealand 1951  -7.635102  91.75113
1108          US 1946 -10.942159 121.25207
```
