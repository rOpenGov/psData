# psData (development version)

-   Update broken API links.
-   Use `readxl` instead of `xlsx` to avoid dependency on `rJava`.

# psData 0.2.2

-   Remove inactive vignettes. Dynamic documentation in README.

# psData 0.2.1

-   Updated URL for `PolityGet` to 2015 version. Thanks to Xavier 
    Fernández i Marín.

# psData 0.1.6

-   More informative drop message when converting country IDs. Thank you to 
    Anh Le.

# psData 0.1.5

-   A number of getters now rely on import from rio.

-   Download Democracy and Dictatorship data set with `DDGet`.
    Thank you to Anh Le.

# psData 0.1.4

`CountryID` distinguishes between Cyprus and Turkish Cyprus

# psData 0.1.3

-   Added `IMF_WBGet` to download Axel Dreher's data set of IMF programs and 
    World Bank projects (1970-2011).

-   Improved message control for `RRCrisisGet`.

-   Minor documentation improvements.

# psData 0.1.2

-   Added `RRCrisisGet` for gathering and cleaning Reinhart and Rogoff (2010) financial crisis dummy variables.

# psData 0.1.1

-   Added ability to report and inspect duplicated values created by standardising country names in `CountryID`, `PolityGet`, and `DpiGet`. Also added a choice to these variables of whether or not to standardise country names along with IDs.

# psData 0.1

First development version of **psData**.
The package includes the following main functions:

-   `DpiGet`

-   `PolityGet`

-   `Wincreator`
