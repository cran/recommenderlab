\name{evaluationScheme}
\Rdversion{1.1}
\alias{evaluationScheme}
\alias{evaluationScheme,ratingMatrix-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create an Evaluation Scheme}
\description{
Creates from a data set an evaluation scheme. The scheme can be a simple split into training and test data, k-fold cross-evaluation or using k
bootstrap samples.
}
\usage{
evaluationScheme(data, ...)
\S4method{evaluationScheme}{ratingMatrix}(data, method="split", train=0.9, 
k=10, given=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{data}{data set.}
	\item{method}{a character string defining the recommender method to use (see details).}
	\item{train}{fraction of the data set used for training.}
	\item{k}{number of folds/times to run the evaluation.}
	\item{given}{single number of items given for evaluation or
	  a vector of length of data giving the number of items given for each
	    observation. }
	\item{\dots}{further arguments. }
}
\details{
\code{evaluationScheme} creates an evaluation scheme with \code{k} runs
following the given method:

\code{"split"} randomly assigns
the proportion of objects given by \code{train} to the training set and
the rest is used for the test set.

\code{"cross-validation"} creates a k-fold cross-validation scheme. The data
is randomly split into k parts and in each run k-1 parts are used for
training and the remaining part is used for testing. After all runs each
part was used as test set once.

\code{"bootstrap"} creates the training set by taking a bootstrap sample
(sampling with replacement) of size \code{train} times size of the data set.
All objects not in the training set are used for testing.
}
\value{
Returns an object of class \code{"evaluationScheme"}.
}
\references{
Kohavi, Ron (1995). "A study of cross-validation and bootstrap for accuracy
estimation and model selection". Proceedings of  the Fourteenth International
Joint Conference on Artificial Intelligence, pp. 1137-1143.
}
\seealso{
\code{\linkS4class{evaluationScheme}},
\code{\linkS4class{ratingMatrix}}.
}
\examples{
data("MSWeb")

MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 100)
MSWeb10 

esSplit <- evaluationScheme(MSWeb10, method="split",
        train = 0.9, k=4, given=3)
esSplit

esCross <- evaluationScheme(MSWeb10, method="cross-validation",
        k=4, given=3)
esCross
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
