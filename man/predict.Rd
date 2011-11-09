\name{predict}
\Rdversion{1.1}
\alias{predict}
\alias{predict,Recommender-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict Recommendations
}
\description{
Creates recommendations using a recommender model and data about new users.
}
\usage{
\S4method{predict}{Recommender}(object, newdata, n = 10, type="topNList", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a recommender model (class \code{"Recommender"}).}
  \item{newdata}{data about new users (class \code{"ratingMatrix"}).}
  \item{n}{ number of recommendations in the top-N list.}
  \item{type}{ type of recommendation. The default type is 
  \code{"topNList"} which creates
  a top-N recommendation list with recommendations. 
  Some recommenders can also create other results (e.g., predicted ratings with
  the type \code{"ratings"}).}
  \item{\dots}{further arguments.}
}
%\details{
%}
\value{
Returns an object of class \code{"topNList"} or of other appropriate 
classes.
}
\seealso{
\code{\linkS4class{Recommender}},
\code{\linkS4class{ratingMatrix}}.
}
\examples{
data("MSWeb")
MSWeb10 <-MSWeb[rowCounts(MSWeb) >10,]

rec <- Recommender(MSWeb10[1:100,], method = "POPULAR")

## create recommendations
pre <- predict(rec, MSWeb10[101:102], n = 10)
pre
as(pre, "list")
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
