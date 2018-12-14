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

The following graphic shows the median sale price on Redfin and the Case-Shiller Index. Between Jan 2012 and Sep 2018, Case-Shiller index increases around 29% while the median sale price increases around 85%. This result implies that Redfin data may not be an representative sample of the Chicago housing market for reasons such as: Redfin expanded their representation overtime - therefore, the changes in Redfin's price not only capture the changes of the market but also the changes of Redfin's business itself.

![](predict-case-shiller-index_files/figure-markdown_github/eda_plot-1.png)

Empirical Strategy
==================

We can think of Case-Shiller index as a function of Redfin data where the disturbance term captures the changes in Redfin's business. This assumption implies the use of the ARIMAX model, where we allow the disturbance term to have AR or MA structure.

``` r
modDf <- merge(df, rfDf, by.x = "mon", by.y = "Month.of.Period.End")
modDf$logCHXRSA <- log(modDf$CHXRSA)
modDf$logmsp <- log(modDf$Median.Sale.Price)
```

Use Hyndman-Khandakar algorithm to determine the order:

``` r
require(forecast)
```

    ## Loading required package: forecast

``` r
fit <- auto.arima(modDf$logCHXRSA, xreg = modDf$logmsp, allowdrift = T)
summary(fit)
```

    ## Series: modDf$logCHXRSA 
    ## Regression with ARIMA(2,1,2) errors 
    ## 
    ## Coefficients:
    ##          ar1      ar2      ma1     ma2   drift     xreg
    ##       0.7906  -0.6291  -0.0460  0.3966  0.0035  -0.0112
    ## s.e.  0.2298   0.1909   0.2604  0.1857  0.0008   0.0045
    ## 
    ## sigma^2 estimated as 2.209e-05:  log likelihood=318.43
    ## AIC=-622.85   AICc=-621.3   BIC=-606.18
    ## 
    ## Training set error measures:
    ##                         ME        RMSE         MAE          MPE     MAPE
    ## Training set -6.216907e-06 0.004492621 0.003146538 8.141297e-05 0.065304
    ##                   MASE        ACF1
    ## Training set 0.6344225 -0.02725734

Residual Diagnostics
--------------------

``` r
# Box-Ljung Serial Correlation Test
Box.test(resid(fit)) # Pass!
```

    ## 
    ##  Box-Pierce test
    ## 
    ## data:  resid(fit)
    ## X-squared = 0.06018, df = 1, p-value = 0.8062

``` r
# Augmented Dickey-Fuller Test
require(tseries) 
```

    ## Loading required package: tseries

``` r
adf.test(resid(fit)) # On the margin
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  resid(fit)
    ## Dickey-Fuller = -3.1407, Lag order = 4, p-value = 0.1087
    ## alternative hypothesis: stationary

``` r
# Shapiro Normaliry Test
shapiro.test(resid(fit)) # Fail
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  resid(fit)
    ## W = 0.90842, p-value = 2.527e-05

Cross-Validation
----------------

Cross-validation is conducted by estimating training data with 60 months and predict the next three month.

``` r
out <- rep(0, nrow(modDf) -3 - 60 + 1)
for(k in 60:(nrow(modDf) - 3)) {
    train <- modDf[(k-59):k, ]
    test <- modDf[(k+1):(k+3), ]
    mod <- Arima(train$logCHXRSA, xreg = train$logmsp, 
                 order = c(2, 1, 2), 
                 seasonal = list(period = 1, order = c(0, 0, 0)), 
                 include.drift  =T) 
    pred <- predict(mod, newxreg = test$logmsp)$pred[, 1]
    out[k-59] <- mean((pred - test$logCHXRSA)^2)
}
print(paste("rmse:", round(mean(out), 2)))
```

    ## [1] "rmse: 0.02"

The root mean squared logarithm error of this model is 0.02. However, the model consistently underforecast the actuals.
