<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{psData Markdown Vignette made with knitr}
-->

# psData Motivation and Tutorial

A lot of progress has been made on improving political scientists' ability to access data 'programmatically', e.g. data can be downloaded with source code R. Packages such as [WDI](http://cran.r-project.org/package=WDI) for World Bank Development Indicator and [dvn](http://cran.r-project.org/package=dvn) for many data sets stored on the [Dataverse Network](http://thedata.org/) make it much easier for political scientists to use this data as part of a highly [integrated and reproducible workflow](http://christophergandrud.blogspot.de/2013/07/getting-started-with-reproducible.html). 

There are nonetheless still many commonly used political science data sets that aren't easily accessible to researchers. Recently, I've been using the [Database of Political Institutions (DPI)](http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/0,,contentMDK:20649465~pagePK:64214825~piPK:64214943~theSitePK:469382,00.html), [Polity IV](http://www.systemicpeace.org/polity/polity4.htm) democracy indicators, and [Reinhart and Rogoff's (2010)](http://www.carmenreinhart.com/data/browse-by-topic/topics/7/) financial crisis occurrence data. All three of these data sets are freely available for download online. However, getting them, cleaning them up, and merging them together is kind of a pain. This is especially true for the Reinhart and Rogoff data, which is in 4 Excel files with over 70 individual sheets, one for each country's data. 

Also, I've been using variables that are combinations and/or transformations of indicators in regularly updated data sets, but which themselves aren't regularly updated. In particular, [Bueno de Mesquita et al. (2003)](http://www.nyu.edu/gsas/dept/politics/data/bdm2s2/Logic.htm) devised two variables that they called the 'winset' and the 'selectorate'. These are basically specific combinations of data in DPI and Polity IV. However, the winset and selectorate variables haven't been updated alongside the yearly updates of DPI and Polity IV. 

There are two big problems here:

1. A lot of time is wasted by political scientists (and their RAs) downloading, cleaning, and transforming these data sets for their own research.

2. There are many opportunities while doing this work to introduce errors. Imagine the errors that might be introduced and go unnoticed if a copy-and-paste approach is used to merge the 70 Reinhart and Rogoff Excel sheets. 

As a solution, I've been working on a new R package called *psData*. This package includes functions that automate the gathering, cleaning, and creation of common political science data and variables. So far (February 2014) it gathers DPI, Polity IV, and Reinhart and Rogoff data, as well as creates winset and selectorate variables. Hopefully the package will save political scientists a lot of time and reduce the number of data management errors. 

There certainly could be errors in the way *psData* gathers data. However, once spotted the errors could be easily reported on the package's [Issues Page](https://github.com/christophergandrud/psData/issues). Once fixed, the correction will be spread to all users via a package update.

## Types of functions

There are two basic types of functions in *psData*: **Getters** and **Variable Builders**. Getter functions automate the gathering and cleaning of particular data sets so that they can easily be merged with other data. They do not transform the underlying data. Variable Builders use Getters to gather data and then transform it into new variables suggested by the political science literature.

## Examples

To download only the **polity2** variable from [Polity IV](http://www.systemicpeace.org/polity/polity4.htm):


```r
# Load package
library(psData)

# Download polity2 variable
PolityData <- PolityGet(vars = 'polity2')
```

Note that the **iso2c** variable refers to the [ISO two letter country code country ID](http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2). This standardised country identifier could be used to easily merge the Polity IV data with another data set. Another country ID type can be selected with the `OutCountryID` argument. See the package documentation for details.

To create **winset** (**W**) and **selectorate** (**ModS**) data use the following code:


```r
WinData <- WinsetCreator()
```

---

## Install

*psData* is on [CRAN](http://cran.r-project.org/package=psData). You can also download the development version with the [devtools](https://github.com/hadley/devtools) package:

```
devtools::install_github('psData', 'christophergandrud')
```

## Suggestions

Please feel free to suggest other data set downloading and variable creating functions. To do this just leave a note on the package's [Issues page](https://github.com/christophergandrud/psData/issues) or make a pull request with a new function added.
