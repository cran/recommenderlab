\name{LIST}
\Rdversion{1.1}
\alias{LIST,binaryRatingMatrix-method}
\alias{LIST,realRatingMatrix-method}
\alias{LIST,topNList-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  List Representation for Recommender Matrix Objects
}
\description{
Create a list representation for variouse objects used in \pkg{recommenderlab}.
}
\usage{
\S4method{LIST}{realRatingMatrix}(from, decode = TRUE, ratings = TRUE, ...)
\S4method{LIST}{binaryRatingMatrix}(from, decode = TRUE, ...)
\S4method{LIST}{topNList}(from, decode = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{ .}
  \item{decode}{ use item names or item IDs (col numbers) for items? }
  \item{ratings}{ create list of ratings?}
  \item{...}{ further arguments (currently unused).}
}
%\details{
%}
\value{
Returns a list.
}
\seealso{
\code{\linkS4class{binaryRatingMatrix}},
\code{\linkS4class{realRatingMatrix}},
\code{\linkS4class{topNList}}.
}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
