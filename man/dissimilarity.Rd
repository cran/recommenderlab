\name{dissimilarity}
\Rdversion{1.1}
\alias{dissimilarity}
\alias{dissimilarity,binaryRatingMatrix-method}
\alias{dissimilarity,realRatingMatrix-method}
\title{
Dissimilarity Calculation Between Rating Data
}
\description{
Calculate dissimilarities between ratings by users and for items.
}
\usage{
\S4method{dissimilarity}{binaryRatingMatrix}(x, y = NULL, method = NULL, args = NULL, which="users")
\S4method{dissimilarity}{realRatingMatrix}(x, y = NULL, method = NULL, args = NULL, which="users")
}
\arguments{
  \item{x}{a rating matrix.}
  \item{y}{\code{NULL} or a second rating matrix to calculate 
	cross-dissimilarities.}
  \item{method}{dissimilarity measure to use. Available measures
  are typically \code{"cosine"}, \code{"pearson"}, \code{"jaccard"}, etc.
  See \code{dissimilarity} for
  class \code{itemMatrix} in \pkg{arules} for details about measures
  for \code{binaryRatingMatrix}. }
  \item{args}{a list of additional arguments for the methods.}
  \item{which}{a character string indicating if the dissimilarity should be 
            calculated between \code{"users"} or \code{"items"}.}
}
%\details{
%}
\value{
  returns an object of class \code{dist} of an appropriate object (e.g., 
  a matrix) to represent a cross-dissimilarity.
}
\seealso{
\code{\linkS4class{ratingMatrix}},
\code{\link[arules]{dissimilarity}} in \pkg{arules}.
}
\examples{
data(MSWeb)

## between 5 users
dissimilarity(sample(MSWeb, 5), method = "jaccard")

## between first 3 items
dissimilarity(MSWeb[,1:3], method = "jaccard", which = "items")

## cross-dissimilarity between first 2 users and users 10-20
dissimilarity(MSWeb[1:2,], MSWeb[10:20,])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
