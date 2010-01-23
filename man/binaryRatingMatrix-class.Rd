\name{binaryRatingMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{binaryRatingMatrix-class}
\alias{coerce,matrix,binaryRatingMatrix-method}
\alias{coerce,itemMatrix,binaryRatingMatrix-method}
\alias{coerce,binaryRatingMatrix,matrix-method}
\alias{coerce,binaryRatingMatrix,dgTMatrix-method}
\alias{coerce,binaryRatingMatrix,ngCMatrix-method}
\alias{coerce,binaryRatingMatrix,dgCMatrix-method}
\alias{coerce,binaryRatingMatrix,itemMatrix-method}
\alias{coerce,binaryRatingMatrix,list-method}
%\alias{dissimilarity,binaryRatingMatrix-method}
%\alias{LIST,binaryRatingMatrix-method}

\title{Class "binaryRatingMatrix": A Binary Rating Matrix}
\description{A matrix to represent binary ratings. Here 1 codes for a possitive
rating and 0 codes no positive rating (either no or a negative rating.) This 
coding is common for market basked data where products are either bought or
not.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("binaryRatingMatrix", data = im)}, where \code{im} is an \code{itemMatrix} as defined in package
\pkg{arules}.
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"itemMatrix"} (see package~\pkg{arules})}
  }
}
\section{Extends}{
Class \code{"\linkS4class{ratingMatrix}"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "matrix", to = "binaryRatingMatrix")}}
    \item{coerce}{\code{signature(from = "itemMatrix", to = "binaryRatingMatrix")}; coerces itemMatrix or transactions from \pkg{arules} to binaryRatingMatrix.}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "matrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "dgTMatrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "ngCMatrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "dgCMatrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "itemMatrix")}}
    \item{coerce}{\code{signature(from = "binaryRatingMatrix", to = "list")}}
%    \item{dissimilarity}{\code{signature(x = "binaryRatingMatrix")}}
%    \item{LIST}{\code{signature(from = "binaryRatingMatrix")}: ... }
	 }
}
\seealso{
	\code{\link[arules:itemMatrix-class]{itemMatrix}}.
	%\code{\linkS4class[arules]{itemMatrix}}.
}
%\examples{
%}
\keyword{classes}
