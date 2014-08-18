\name{evaluationScheme}
\Rdversion{1.1}
\alias{evaluationScheme}
\alias{evaluationScheme,ratingMatrix-method}
\title{Creator Function for evaluationScheme}
\description{
Creates an evaluationScheme object from a data set. The scheme can be a 
simple split into training and test data, k-fold cross-evaluation or using k
independent bootstrap samples.
}
\usage{
evaluationScheme(data, ...)

\S4method{evaluationScheme}{ratingMatrix}(data, method="split", 
    train=0.9, k=NULL, given, goodRating = NA)
}
\arguments{
	\item{data}{data set as a ratingMatrix.}
	\item{method}{a character string defining the evaluation 
	method to use (see details).}
	\item{train}{fraction of the data set used for training.}
	\item{k}{number of folds/times to run the evaluation (defaults to 10 
	    for cross-validation and bootstrap and 1 for split).}
	\item{given}{single number of items given for evaluation or
	  a vector of length of data giving the number of items given for each
	    observation. }
	\item{goodRating}{numeric; threshold at which ratings are considered 
	good for evaluation. E.g., with \code{goodRating=3} all items 
	with actual user rating of greater or equal 3 are 
	considered positives in the evaluation process. 
	Note that this argument is only used if 
	the ratingMatrix is
	a of subclass realRatingMatrix!}
	\item{\dots}{further arguments. }
}
\details{
\code{evaluationScheme} creates an evaluation scheme (training and test data) 
with \code{k} runs and one of the given methods:

\code{"split"} randomly assigns
the proportion of objects given by \code{train} to the training set and
the rest is used for the test set.

\code{"cross-validation"} creates a k-fold cross-validation scheme. The data
is randomly split into k parts and in each run k-1 parts are used for
training and the remaining part is used for testing. After all k runs each
part was used as the test set exactly once.

\code{"bootstrap"} creates the training set by taking a bootstrap sample
(sampling with replacement) of size \code{train} times number of users in
the data set.
All objects not in the training set are used for testing.

For evaluation, the scheme chooses from the test data for each user
\code{given} items are randomly as "known" items, the remaining
items are "unknown." The known items are used to create a prediction and
it is evaluated how well the algorithm predicts the unknown items.
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
\code{\link{getData}},
\code{\linkS4class{evaluationScheme}},
\code{\linkS4class{ratingMatrix}}.
}
\examples{
data("MSWeb")

MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 50)
MSWeb10 

esSplit <- evaluationScheme(MSWeb10, method="split",
        train = 0.9, k=1, given=3)
esSplit

esCross <- evaluationScheme(MSWeb10, method="cross-validation",
        k=4, given=3)
esCross
}
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
