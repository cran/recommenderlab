\name{ratingMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{ratingMatrix-class}
\alias{coerce,ratingMatrix,list-method}
\alias{[,ratingMatrix,ANY,ANY,ANY-method}
\alias{colCounts,ratingMatrix-method}
\alias{colMeans,ratingMatrix-method}
\alias{dim,ratingMatrix-method}
\alias{dimnames<-,ratingMatrix,list-method}
\alias{dimnames,ratingMatrix-method}
%\alias{evaluationScheme,ratingMatrix-method}
\alias{image,ratingMatrix-method}
\alias{nitems,ratingMatrix-method}
\alias{nratings,ratingMatrix-method}
\alias{nratings}
\alias{nusers,ratingMatrix-method}
%\alias{Recommender,ratingMatrix-method}
\alias{rowCounts,ratingMatrix-method}
\alias{rowMeans,ratingMatrix-method}
\alias{sample,ratingMatrix-method}
\alias{show,ratingMatrix-method}
%% generics
\alias{colCounts}
\alias{nitems}
\alias{nusers}
\alias{rowCounts}

\title{Class "ratingMatrix": Virtual Class for Rating Data}
\description{Defines a common class for rating data.}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
	\describe{
		\item{[}{\code{signature(x = "ratingMatrix", i = "ANY", j = "ANY", drop = "ANY")}: subset the rating matrix (\code{drop} is ignorred). }
		\item{coerce}{\code{signature(from = "ratingMatrix", to = "list")}}

		\item{colCounts}{\code{signature(x = "ratingMatrix")}:  number of ratings per column.}
		\item{colMeans}{\code{signature(x = "ratingMatrix")}: column-wise rating means. }
		\item{dim}{\code{signature(x = "ratingMatrix")}: dimensions of the rating matrix. }
		\item{dimnames<-}{\code{signature(x = "ratingMatrix", value = "list")}: replace dimnames. }
		\item{dimnames}{\code{signature(x = "ratingMatrix")}: retrieve dimnames. }
%		\item{evaluationScheme}{\code{signature(data = "ratingMatrix")}: ... }
		\item{image}{\code{signature(x = "ratingMatrix")}: plot the matrix. }
		\item{nitems}{\code{signature(x = "ratingMatrix")}: number of items (ncol). }
		\item{nratings}{\code{signature(x = "ratingMatrix")}: number of ratings in the matrix. }
		\item{nusers}{\code{signature(x = "ratingMatrix")}: number of users (nrow). }
%		\item{Recommender}{\code{signature(data = "ratingMatrix")}: ... }
		\item{rowCounts}{\code{signature(x = "ratingMatrix")}:  number of ratings per row.}
		\item{rowMeans}{\code{signature(x = "ratingMatrix")}: row-wise rating means. }
		\item{sample}{\code{signature(x = "ratingMatrix")}: sample from users (rows). }
		\item{show}{\code{signature(object = "ratingMatrix")} }

}
}
\seealso{
	See implementing classes
	\code{\linkS4class{realRatingMatrix}}
	and
	\code{\linkS4class{binaryRatingMatrix}}.
}
\keyword{classes}
