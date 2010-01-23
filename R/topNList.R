
setAs("topNList", "dgTMatrix",
	function(from) {
		i <- rep(1:length(from@items), lapply(from@items, length))
		j <- unlist(from@items)
		x <- rep(1, length(j))

		new("dgTMatrix", i = i-1L, j = as.integer(j)-1L,
			x = x,
			Dim = c(length(from@items), length(from@itemLabels)),
			Dimnames = list(names(from@items), from@itemLabels))
	})


setAs("topNList", "dgCMatrix",
	function(from) as(as(from, "dgTMatrix"),"dgCMatrix"))

setAs("topNList", "ngCMatrix",
	function(from) as(as(from, "dgCMatrix"), "ngCMatrix"))

setAs("topNList", "matrix",
	function(from) as(as(from, "dgTMatrix"),"matrix"))

setMethod("LIST", signature(from = "topNList"),
	function(from, decode = TRUE, ...)
	if(decode) lapply(from@items, function(y) from@itemLabels[y])
	else from@items)

setAs("topNList", "list", function(from) LIST(from, decode = TRUE))

setMethod("show", signature(object = "topNList"),
	function(object) {
		cat("Recommendations as", sQuote(class(object)), 
			"with n =", object@n, "for",
			length(object@items),"users.","\n")
		invisible(NULL)
	})

setMethod("bestN", signature(x = "topNList"),
	function(x, n = 10) new("topNList", items = lapply(x@items, head, n), 
		itemLabels = x@itemLabels, n = as.integer(n)))
	
setMethod("colCounts", signature(x = "topNList"),
	function(x, ...) {
		s <- colSums(as(x, "ngCMatrix"))
		names(s) <- x@itemLabels
		s
	})

setMethod("rowCounts", signature(x = "topNList"),
	function(x, ...) sapply(x@items, length))


