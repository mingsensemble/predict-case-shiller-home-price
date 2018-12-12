Predict Case-Shiller Home Price Index (Chicago)
================

Motivation
==========

**Question** Can we use a real-time selected sample to predict the overall market trend?
Redfin data center provides monthly real estate information of houses listed on their website. The data are updated each month. On the other hand, Case-Shiller Home Price Index measures the value of the residential real estate and is reported with two month lag. Redfin data are an almost real-time selected sample while case-Shiller Home Price Index is a general survey with lags.

Data Preprocessing
==================

This exercise uses Case-Shiller Home Price Index of Chicago downloaded from FRED database using *quantmod* package and Redfin single-family home Chicago listing data downloaded from [Redfin Data Center](https://www.redfin.com/blog/data-center).

``` r
# data preprocessing
require(quantmod)
require(zoo)
# download case-shiller housing price index Chicago seasonally adjusted
getSymbols('CHXRSA', src='FRED')
```

    ## [1] "CHXRSA"

``` r
# ================================
# load redfin data
rfDf <- subset(redfin, Region == "Chicago, IL", select = c("Month.of.Period.End", "Median.Sale.Price", "Homes.Sold", "New.Listings", "Inventory", "Days.on.Market", "Average.Sale.To.List"))

rfDf$Month.of.Period.End <- as.yearmon(rfDf$Month.of.Period.End)
# process strings into numbers
rfDf$New.Listings <- as.numeric(gsub(",", "", rfDf$New.Listings))
rfDf$Inventory <- as.numeric(gsub(",", "", rfDf$Inventory))
rfDf$Median.Sale.Price <- as.numeric(gsub("\\D", "", rfDf$Median.Sale.Price))
rfDf$Average.Sale.To.List <- as.numeric(gsub("\\D", "", rfDf$Average.Sale.To.List)) * 0.001
```

Data Visualization
==================

![](predict-case-shiller-index_files/figure-markdown_github/eda_plot-1.png)
