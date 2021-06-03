
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arkr

<!-- badges: start -->
<!-- badges: end -->

The goal of arkr is to obtain data on the structure of the ETF proposed
by ark.

## Installation

``` r
remotes::install_github("ropensci/tabulizer")
devtools::install_github("benjaminguinaudeau/arkr")
```

``` r
library(arkr)
```

## Innovation

``` r
# get_holding("innovation")
ex_innov %>% dplyr::glimpse()
#> Rows: 54
#> Columns: 8
#> $ rank           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ company        <chr> "TESLA INC", "TELADOC HEALTH INC", "ROKU INC", "SQUARE…
#> $ ticker         <chr> "TSLA", "TDOC", "ROKU", "SQ", "SHOP", "ZM", "TWLO", "C…
#> $ cusip          <chr> "88160R101", "87918A105", "77543R102", "852234103", "8…
#> $ shares         <dbl> 3473087, 8562416, 3489209, 4429724, 723074, 2690075, 2…
#> $ market_value   <dbl> 2101634405, 1307823420, 1187168470, 976355467, 8890845…
#> $ weight_percent <dbl> 9.99, 6.21, 5.64, 4.64, 4.22, 4.18, 3.62, 3.61, 3.52, …
#> $ day            <date> 2021-06-03, 2021-06-03, 2021-06-03, 2021-06-03, 2021-…
```

## Genomic

``` r
# get_holding("genomic") 
ex_genom %>%
  dplyr::glimpse()
#> Rows: 60
#> Columns: 8
#> $ rank           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ company        <chr> "TELADOC HEALTH INC", "EXACT SCIENCES CORP", "CAREDX I…
#> $ ticker         <chr> "TDOC", "EXAS", "CDNA", "REGN", "PACB", "NVS", "VRTX",…
#> $ cusip          <chr> "87918A105", "30063P105", "14167L103", "75886F107", "6…
#> $ shares         <dbl> 3901867, 3811578, 4519949, 686961, 12985503, 3682474, …
#> $ market_value   <dbl> 595971166, 415919391, 361686319, 347162611, 347232350,…
#> $ weight_percent <dbl> 7.21, 5.03, 4.38, 4.20, 4.20, 3.96, 3.79, 3.41, 3.36, …
#> $ day            <date> 2021-06-03, 2021-06-03, 2021-06-03, 2021-06-03, 2021-…
```

## FinTech

``` r
# get_holding("fintech") 
ex_fintech %>%
  dplyr::glimpse()
#> Rows: 43
#> Columns: 8
#> $ rank           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ company        <chr> "SQUARE INC - A", "SHOPIFY INC - CLASS A", "SEA LTD-AD…
#> $ ticker         <chr> "SQ", "SHOP", "SE", "PYPL", "Z", "JD", "PINS", "TCEHY"…
#> $ cusip          <chr> "852234103", "82509L107", "81141R100", "70450Y103", "9…
#> $ shares         <dbl> 1639959, 154615, 711846, 639357, 1491489, 1665776, 195…
#> $ market_value   <dbl> 361463363, 190113058, 183905414, 167620225, 166644066,…
#> $ weight_percent <dbl> 9.83, 5.17, 5.00, 4.56, 4.53, 3.47, 3.41, 3.34, 3.31, …
#> $ day            <date> 2021-06-03, 2021-06-03, 2021-06-03, 2021-06-03, 2021-…
```

## Space

``` r
# get_holding("space") 
ex_space %>%
  dplyr::glimpse()
#> Rows: 39
#> Columns: 8
#> $ rank           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ company        <chr> "TRIMBLE INC", "THE 3D PRINTING ETF", "KRATOS DEFENSE …
#> $ ticker         <chr> "TRMB", "PRNT", "KTOS", "IRDM", "LHX", "LMT", "6301", …
#> $ cusip          <chr> "896239100", "00214Q500", "50077B207", "46269C102", "5…
#> $ shares         <dbl> 696519, 1119726, 1448653, 886181, 153692, 71199, 91518…
#> $ market_value   <dbl> 54627985, 43635722, 36824759, 33683740, 33615514, 2741…
#> $ weight_percent <dbl> 8.68, 6.94, 5.85, 5.35, 5.34, 4.36, 4.34, 4.05, 3.59, …
#> $ day            <date> 2021-06-03, 2021-06-03, 2021-06-03, 2021-06-03, 2021-…
```

## Israel

``` r
# get_holding("israel") 
ex_israel %>%
  dplyr::glimpse()
#> Rows: 59
#> Columns: 8
#> $ rank           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ company        <chr> "ELECTRA CONSUMER PRODUCTS 19", "ITURAN LOCATION AND C…
#> $ ticker         <chr> "ECP", "ITRN", "CAMT", "CEL", "PLSN", "BEZQ", "DANE", …
#> $ cusip          <chr> "B536CY7", "M6158M104", "M20791105", "B23WQK8", "60947…
#> $ shares         <dbl> 124411, 265928, 170464, 1586815, 114567, 5408954, 3153…
#> $ market_value   <dbl> 6475349, 6435458, 6438425, 6211401, 6174692, 6163576, …
#> $ weight_percent <dbl> 2.05, 2.04, 2.04, 1.97, 1.96, 1.95, 1.94, 1.93, 1.93, …
#> $ day            <date> 2021-06-03, 2021-06-03, 2021-06-03, 2021-06-03, 2021-…
```

## Internet

``` r
# get_holding("internet") 
ex_internet %>%
  dplyr::glimpse()
#> Rows: 48
#> Columns: 8
#> $ rank           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ company        <chr> "TESLA INC", "SHOPIFY INC - CLASS A", "TWITTER INC", "…
#> $ ticker         <chr> "TSLA", "SHOP", "TWTR", "TDOC", "SQ", "GBTC", "SPOT", …
#> $ cusip          <chr> "88160R101", "82509L107", "90184L102", "87918A105", "8…
#> $ shares         <dbl> 896524, 213378, 4431776, 1637904, 1130310, 7432320, 87…
#> $ market_value   <dbl> 542504603, 262367455, 253320316, 250173457, 249131627,…
#> $ weight_percent <dbl> 9.85, 4.76, 4.60, 4.54, 4.52, 4.20, 3.82, 3.79, 3.67, …
#> $ day            <date> 2021-06-03, 2021-06-03, 2021-06-03, 2021-06-03, 2021-…
```

## Robot

``` r
# get_holding("robot") 
ex_robot %>%
  dplyr::glimpse()
#> Rows: 0
#> Columns: 2
#> $ rank <dbl> 
#> $ day  <date>
```

## 3D Printing

``` r
# get_holding("printing") 
ex_printing %>%
  dplyr::glimpse()
#> Rows: 58
#> Columns: 8
#> $ rank           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ company        <chr> "3D SYSTEMS CORP", "EXONE CO/THE", "STRAUMANN HOLDING …
#> $ ticker         <chr> "DDD", "XONE", "STMN", "AM3D", "DM", "ALTR", "ALMDG", …
#> $ cusip          <chr> "88554D205", "302104104", "7156832", "BMHTHK2", "25058…
#> $ shares         <dbl> 1245906, 1404628, 19414, 1196444, 1894776, 400858, 437…
#> $ market_value   <dbl> 40292600, 32348583, 30506324, 27841604, 27246879, 2684…
#> $ weight_percent <dbl> 7.27, 5.84, 5.51, 5.02, 4.92, 4.84, 4.47, 4.44, 4.26, …
#> $ day            <date> 2021-06-03, 2021-06-03, 2021-06-03, 2021-06-03, 2021-…
```
