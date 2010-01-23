## Each implementing class has to implement coercion to 
## matrix, dgTMatrix, ngCMatrix, dgCMatrix

## object in data slot needs to have dim, dimnames and dimnames<- implemented

## dim
setMethod("dim", signature(x = "ratingMatrix"), 
	function(x) dim(x@data))
setMethod("nitems", signature(x = "ratingMatrix"), 
	function(x, ...) ncol(x))
setMethod("nusers", signature(x = "ratingMatrix"), 
	function(x, ...) nrow(x))

## dimnames
setMethod("dimnames", signature(x = "ratingMatrix"), 
	function(x) dimnames(x@data))

setReplaceMethod("dimnames", signature(x = "ratingMatrix",     
		value = "list"), function(x, value) {
		dimnames(x@data) <- value
		x
	})

## row/col counts, sums, etc.
## MAtrix does not handle dimnames well
setMethod("colCounts", signature(x = "ratingMatrix"), 
	function(x, ...) {
		s <- colSums(as(x, "ngCMatrix"))
		names(s) <- colnames(x)
		s
	})

setMethod("rowCounts", signature(x = "ratingMatrix"), 
	function(x, ...) {
		s <- rowSums(as(x, "ngCMatrix"))
		names(s) <- rownames(x)
		s
	})

setMethod("colSums", signature(x = "ratingMatrix"), 
	function(x, na.rm = FALSE, dims = 1, ...) {
		s <- colSums(as(x, "dgCMatrix"), na.rm, dims, ...)
		names(s) <- colnames(x)
		s
	})

setMethod("rowSums", signature(x = "ratingMatrix"), 
	function(x, na.rm = FALSE, dims = 1, ...) {
		s <- rowSums(as(x, "dgCMatrix"), na.rm, dims, ...)
		names(s) <- rownames(x)
		s
	})

## we need to ignore 0s
setMethod("colMeans", signature(x = "ratingMatrix"),
	function(x, na.rm = FALSE, dims = 1, ...) {
		s <- colSums(x, dims, na.rm, ...) / colCounts(x, dims, na.rm, ...)
		names(s) <- colnames(x)
		s
	})

setMethod("rowMeans", signature(x = "ratingMatrix"),
	function(x, na.rm = FALSE, dims = 1, ...) { 
		s <- rowSums(x, dims, na.rm, ...) / rowCounts(x, dims, na.rm, ...)
		names(s) <- rownames(x)
		s
	})

## total ratings
setMethod("nratings", signature(x = "ratingMatrix"), 
	    function(x, ...) sum(rowCounts(x)))

## subset
setMethod("[", signature(x = "ratingMatrix"),
		function(x, i, j, ..., drop) {
			if(missing(i)) i <- 1:nrow(x)
			if(missing(j)) j <- 1:ncol(x)

			x@data <- x@data[i,j, ..., drop=FALSE]
			x
		})


## sample
setMethod("sample", signature(x = "ratingMatrix"),
	function(x, size, replace = FALSE, prob = NULL){
		index <- sample(c(1:nrow(x)), size = size,
			replace = replace, prob = prob)

		x[index,]
	})

## show
setMethod("show", signature(object = "ratingMatrix"),
	function(object) {
		cat(nrow(object), 'x', ncol(object), "rating matrix of class",
			sQuote(class(object)), "\nwith",
			nratings(object), "ratings.\n")
		invisible(NULL)
	})

## image
setMethod("image", signature(x = "ratingMatrix"),
	function(x, xlab = "Items (Columns)", ylab = "Users (Rows)", ...)
	image(as(x, "dgTMatrix"), ylab = ylab, xlab = xlab, ...)
)


