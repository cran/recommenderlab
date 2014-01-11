\name{evaluate}
\Rdversion{1.1}
\alias{evaluate}
\alias{evaluate,evaluationScheme,character-method}
\alias{evaluate,evaluationScheme,list-method}
\title{
Evaluate a Recommender Models
}
\description{
Evaluates a single or a list of recommender model given an evaluation scheme.
}
\usage{
evaluate(x, method, ...)

\S4method{evaluate}{evaluationScheme,character}(x, method, n=1:10, parameter=NULL, 
    progress = TRUE, keepModel=FALSE)
\S4method{evaluate}{evaluationScheme,list}(x, method, n=1:10, parameter=NULL, 
    progress = TRUE, keepModel=FALSE)
}
\arguments{
  \item{x}{an evaluation scheme (class \code{"evaluationScheme"}).}
  \item{method}{a character string or a list. If
  a single character string is given it defines the recommender method 
  used for evaluation. If several recommender methods need to be compared,
  \code{method} contains a nested list. Each element describes a recommender
  method and consists of a list with two elements: a character string
  named \code{"method"} containing the method and a list names 
  \code{"parameters"} containing the parameters used for this recommender method.
  See \code{Recommender} for available methods.}
  \item{n}{N (number of recommendations) of the top-N lists generated.}
  \item{parameter}{a list with parameters for the recommender algorithm (only
    used when \code{method} is a single method).}
  \item{progress}{logical; report progress?}
  \item{keepModel}{logical; store used recommender models?}
  \item{\dots}{further arguments. }
}
\value{
Returns an object of class \code{"evaluationResults"}
or if \code{method} is a list an object of class \code{"evaluationResultList"}.
}
\seealso{
\code{\linkS4class{evaluationScheme}},
\code{\linkS4class{evaluationResults}}.
\code{\linkS4class{evaluationResultList}}.
}
\examples{
data("MSWeb")
MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 50)

## create an evaluation scheme
es <- evaluationScheme(MSWeb10, method="cross-validation",
        k=4, given=3)

## run evaluation
ev <- evaluate(es, "POPULAR")
ev

## look at the results
avg(ev)
plot(ev, type="o", annotate = TRUE)

## now run evaluate with a list
algorithms <- list(
		RANDOM = list(name = "RANDOM", param = NULL),
		POPULAR = list(name = "POPULAR", param = NULL),
		UBCF = list(name = "UBCF", param = NULL)
		)

evlist <- evaluate(es, algorithms)
plot(evlist, legend="topright")

## select the first results
evlist[[1]]
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
