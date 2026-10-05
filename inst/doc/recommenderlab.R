## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(recommenderlab)
set.seed(1234)

## ----install, eval=FALSE------------------------------------------------------
# install.packages("recommenderlab")

## ----optional-packages, eval=FALSE--------------------------------------------
# install.packages(c("irlba", "recosystem"))

## ----load-package-------------------------------------------------------------
library(recommenderlab)

## ----data---------------------------------------------------------------------
data("MovieLense")
MovieLense

MovieLense100 <- MovieLense[rowCounts(MovieLense) > 100, ]
MovieLense100

## ----inspect-data-------------------------------------------------------------
summary(rowCounts(MovieLense100))
summary(getRatings(MovieLense100))

## ----recommendations----------------------------------------------------------
train <- MovieLense100[1:300, ]
rec <- Recommender(train, method = "UBCF")
rec

recommendations <- predict(rec, MovieLense100[301:302, ], n = 5)
recommendations
as(recommendations, "list")

## ----predicted-ratings--------------------------------------------------------
predicted_ratings <- predict(
  rec,
  MovieLense100[301:302, ],
  type = "ratings"
)
as(predicted_ratings, "matrix")[, 1:6]

## ----evaluation-scheme--------------------------------------------------------
evaluation_data <- MovieLense100[1:200, ]
scheme <- evaluationScheme(
  evaluation_data,
  method = "cross-validation",
  k = 5,
  given = -5,
  goodRating = 4
)
scheme

## ----evaluate-----------------------------------------------------------------
algorithms <- list(
  `popular items` = list(name = "POPULAR", param = NULL),
  `random items` = list(name = "RANDOM", param = NULL)
)

results <- evaluate(
  scheme,
  algorithms,
  type = "topNList",
  n = c(1, 3, 5, 10),
  progress = FALSE
)
getResults(results[[1]])

## ----plot-results, fig.width=7, fig.height=5----------------------------------
plot(results, annotate = TRUE, legend = "topleft")

