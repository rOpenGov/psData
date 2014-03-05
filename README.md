## `psData` creates data frames with attributes that makes it easy to manipulate panel/time series data frames.

The `psData` S4 class is built on top of Jeffrey Arnold's [DataFrameConstr](https://github.com/jrnold/DataFrameConstr) and accepts the same arguments, plus two optional lists of settings:

* `design` to specify a panel design for the data
* `meta` to add metadata (authors, URL, etc.)

The goals of the associated package are:

* to offer simple download methods for panel datasets;
* to offer simple panel data methods by handling common panel units, like the `countrycode` taxonomies;
* to offer simple time series functions by handling time units from the `lubridate` package;
* to handle other forms of dyadic data, _k_-adic and multilevel designs from the same class

## DEMO

```{S}
devtools::install_github("briatte/psData")
library(psData)
as.panel(debt, "Country", "Year")
```
```
Panel data frame [ 1171 rows x  4 columns, 20 Country x 64 Year ]

    Country Year    growth     ratio
1 Australia 1946 -3.557951 190.41908
2 Australia 1947  2.459475 177.32137
3 Australia 1948  6.437534 148.92981
4 Australia 1949  6.611994 125.82870
5 Australia 1950  6.920201 109.80940
6 Australia 1951  4.272612  87.09448
```

Add `quiet = TRUE` to display the country-year format detection reports.

## TODO

[ ] assign both `data.frame` and `psData` classes, as in `data.table`?
[ ] fix `setMethod("merge")` (not working yet, S4 declaration contains bugs)
[ ] improve `setMethod("design")` (allow assignment as in `igraph::V`)
[ ] improve `setMethod("meta")` (provide output for DataPackage, or route to `pander`)
[ ] add `setMethod("summarize")` (see `xtsum` and `xtdes` in `qogdata` or Stata)
[ ] add `setMethod("sample")` (is it acceptable to sample on panel units by default?)
[x] add `xtsubset` capabilities
[ ] add `xtcountry` capabilities (substitute to `CountryID`)?
[ ] add `find` and `names` method to find and label variables?
[ ] rewrite `DropNA.psData` as `setMethod("na.omit", "psData")`?
[ ] switch from panel-time method to generic dyadic method, and later _k_-adic
