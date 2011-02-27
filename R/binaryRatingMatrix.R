
setAs("matrix", "binaryRatingMatrix",
	function(from) new("binaryRatingMatrix", data = as(from, "itemMatrix")))

setAs("itemMatrix", "binaryRatingMatrix",
	function(from) new("binaryRatingMatrix", data = from))

setAs("binaryRatingMatrix", "matrix",
	function(from) as(from@data, "matrix"))

setAs("binaryRatingMatrix", "dgTMatrix",
	function(from) as(as(from, "dgCMatrix"), "dgTMatrix"))

## itemMatrix is transposed!
setAs("binaryRatingMatrix", "ngCMatrix",
	function(from) t(as(from@data, "ngCMatrix")))

## itemMatrix is transposed!
setAs("binaryRatingMatrix", "dgCMatrix",
	function(from) t(as(from@data, "dgCMatrix")))

setAs("binaryRatingMatrix", "itemMatrix",
	function(from) from@data)


setMethod("LIST", signature(from = "binaryRatingMatrix"),
	function(from, decode = TRUE, ...) {
		LIST(from@data, decode = decode)
	}
)

setAs("binaryRatingMatrix", "list", function(from) LIST(from))

## split test data
setMethod(".splitKnownUnknown", signature(data="binaryRatingMatrix"),
	function(data, given) {

		## given might of length one or length(data)
		if(length(given)==1) given <- rep(given, nrow(data))

		l <- LIST(data@data, decode=FALSE)
		known_index <- lapply(1:length(l),
			FUN = function(i) sample(1:length(l[[i]]), given[i]))

		known <- encode(
			lapply(1:length(l), FUN = function(x)
				l[[x]][known_index[[x]]]),
			itemLabels = itemLabels(data@data))

		unknown <- encode(
			lapply(1:length(l), FUN = function(x)
				l[[x]][-known_index[[x]]]),
			itemLabels = itemLabels(data@data))


		known <- new("binaryRatingMatrix", data = known)
		unknown <- new("binaryRatingMatrix", data = unknown)

		list(
			known = known,
			unknown = unknown
		)
	})


