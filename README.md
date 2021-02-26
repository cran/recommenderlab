# recommenderlab - Lab for Developing and Testing Recommender Algorithms - R package

[![CRAN version](https://www.r-pkg.org/badges/version/recommenderlab)](https://cran.r-project.org/package=recommenderlab)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/recommenderlab)](https://cran.r-project.org/package=recommenderlab)
  [![R-CMD-check](https://github.com/mhahsler/recommenderlab/workflows/R-CMD-check/badge.svg)](https://github.com/mhahsler/recommenderlab/actions)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/recommenderlab?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/recommenderlab)

This R package provides an infrastructure to test and develop
    recommender algorithms. The package supports rating (e.g., 1-5 stars) and 
    unary (0-1) data sets. Supported algorithms are:
    
* User-based collborative filtering (UBCF)
* Item-based collborative filtering (IBCF)
* SVD with column-mean imputation (SVD)
* Funk SVD (SVDF)
* Alternating Least Squares (ALS)
* MAtrix factorization with LIBMF (LIBMF)
* Association rule-based recommender (AR)
* Popular items (POPULAR)
* Randomly chosen items for comparison (RANDOM)
* Re-recommend liked items (RERECOMMEND)
* Hybrid recommendations (HybridRecommender)

For evaluation, the framework supports given-n and all-but-x protocols with

* Train/test split
* Cross-validation
* Repeated bootstrap sampling

Evaluation measures are:

* Rating errors: MSE, RMSE, MAE
* Top-N recommendations: TPR/FPR (ROC), precision and recall

## Installation

__Stable CRAN version:__ install from within R with
```R
install.packages("recommenderlab")
```
__Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/recommenderlab/build/artifacts) or install from GitHub (needs devtools).
```R 
devtools::install_github("mhahsler/recommenderlab")
```

## Usage

Load the package and prepare a dataset (included in the package).
```R
library("recommenderlab")
data("MovieLense")
### use only users with more than 100 ratings
MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
MovieLense100
```

```
358 x 1664 rating matrix of class ‘realRatingMatrix’ with 73610 ratings.
```

Train a user-based collaborative filtering recommender using a small training set.
```R
train <- MovieLense100[1:50]
rec <- Recommender(train, method = "UBCF")
rec
```

```
Recommender of type ‘UBCF’ for ‘realRatingMatrix’ 
learned using 50 users.
```


Create top-N recommendations for new users (users 101 and 102)
```R
pre <- predict(rec, MovieLense100[101:102], n = 10)
pre
```

```
Recommendations as ‘topNList’ with n = 10 for 2 users. 
```

```R
as(pre, "list")
```

```
$`291`
 [1] "Alien (1979)"              "Titanic (1997)"           
 [3] "Contact (1997)"            "Aliens (1986)"            
 [5] "Amadeus (1984)"            "Godfather, The (1972)"    
 [7] "Henry V (1989)"            "Sting, The (1973)"        
 [9] "Dead Poets Society (1989)" "Schindler's List (1993)"  

$`292`
 [1] "Usual Suspects, The (1995)" "Amadeus (1984)"            
 [3] "Raising Arizona (1987)"     "Citizen Kane (1941)"       
 [5] "Titanic (1997)"             "Brazil (1985)"             
 [7] "Stand by Me (1986)"         "M*A*S*H (1970)"            
 [9] "Babe (1995)"                "GoodFellas (1990)"   
```

A simple Shiny App running recommenderlab can be found at  [https://mhahsler-apps.shinyapps.io/Jester/](https://mhahsler-apps.shinyapps.io/Jester/) 
([source code](https://github.com/mhahsler/recommenderlab/tree/master/Work/apps)). 


## References

* Michael Hahsler (2016). [recommenderlab: A Framework for Developing and
Testing Recommendation Algorithms](https://CRAN.R-project.org/package=recommenderlab/vignettes/recommenderlab.pdf), R package. https://CRAN.R-project.org/package=recommenderlab
* recommenderlab [reference manual](https://CRAN.R-project.org/package=recommenderlab/recommenderlab.pdf)
*  Suresh K. Gorakala and Michele Usuelli (2015) [Building a Recommendation System with R](https://www.amazon.com/Building-Recommendation-System-Suresh-Gorakala/dp/1783554495) features the package recommenderlab.
