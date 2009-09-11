\name{topNList-class}
\Rdversion{1.1}
\docType{class}
\alias{topNList-class}
\alias{bestN,topNList-method}
\alias{coerce,topNList,dgTMatrix-method}
\alias{coerce,topNList,dgCMatrix-method}
\alias{coerce,topNList,ngCMatrix-method}
\alias{coerce,topNList,matrix-method}
\alias{coerce,topNList,list-method}
\alias{colCounts,topNList-method}
%\alias{LIST,topNList-method}
\alias{rowCounts,topNList-method}
\alias{show,topNList-method}
%% generics
\alias{bestN}

\title{Class "topNList": Top-N List}
\description{Recommendations a Top-N list. }
\section{Objects from the Class}{
Objects can be created by 
\code{predict} with a recommender model and a new data.
}
\section{Slots}{
	\describe{
		\item{\code{items}:}{Object of class \code{"list"}.
		Each represents a top-N recommendation (an integer vector) 
		with item IDs 
		(column numbers in the rating matrix). The items are ordered in
		each vector.}
		\item{\code{itemLabels}:}{Object of class \code{"character"}}
		\item{\code{n}:}{Object of class \code{"integer"} specifying the
		number of items in each recommendation. Note that the actual number
		on recommended items can be less depending on the data and the used
		algorithm.}
	}
}
\section{Methods}{
	\describe{
		\item{bestN}{\code{signature(x = "topNList")}: returns only the best
		n recommendations (second argument is n which defaults to 10). }
		\item{coerce}{\code{signature(from = "topNList", to = "dgTMatrix")}}
		\item{coerce}{\code{signature(from = "topNList", to = "dgCMatrix")}}
		\item{coerce}{\code{signature(from = "topNList", to = "ngCMatrix")}}
		\item{coerce}{\code{signature(from = "topNList", to = "matrix")}}
		\item{coerce}{\code{signature(from = "topNList", to = "list")}}
		\item{colCounts}{\code{signature(x = "topNList")}: in how many top-N 
		does each item occur? }
%		\item{LIST}{\code{signature(from = "topNList")}: ... }
		\item{rowCounts}{\code{signature(x = "topNList")}: number of recommendations per user. }
		\item{show}{\code{signature(object = "topNList")}}
	}
}
\seealso{
\code{\link{evaluate}}
}
%\examples{
%}
\keyword{classes}
