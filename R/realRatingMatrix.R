## in the sparse representation 0 means missing

setAs("matrix", "realRatingMatrix",
	function(from) new("realRatingMatrix", data = as(from, "dgCMatrix")))

setAs("realRatingMatrix", "matrix",
	function(from) { 
		m <- as(from@data, "matrix")
		m[m==0] <- NA
		m
	})

setAs("realRatingMatrix", "dgTMatrix",
	function(from) as(as(from, "dgCMatrix"), "dgTMatrix"))

setAs("realRatingMatrix", "ngCMatrix",
	function(from) as(as(from, "dgCMatrix"), "ngCMatrix"))

setAs("realRatingMatrix", "dgCMatrix",
	function(from) from@data)



## from a data.frame with columns user, item, rating
setAs("data.frame", "realRatingMatrix", function(from) {
		user	<- from[,1]
		item	<- from[,2]
		rating	<- as.numeric(from[,3])

		if(any(rating==0)) warning("Zero ratings will be lost! Add one to the ratings.")

		i <- factor(user)
		j <- factor(item)


		dgT <- new("dgTMatrix", i = as.integer(i)-1L, j = as.integer(j)-1L, 
			x = rating,
			Dim = c(length(levels(i)), length(levels(j))),
			Dimnames = list(levels(i),levels(j)))
	
		new("realRatingMatrix", data = as(dgT, "dgCMatrix"))
	})


setMethod("LIST", signature(from = "realRatingMatrix"),
	function(from, decode = TRUE, ratings = TRUE, ...) {
		trip <- as(from@data, "dgTMatrix")
		lst <- split(trip@j+1L, trip@i)
		rts <- split(trip@x, trip@i)

		if(decode) lst <- lapply(lst, function(y) colnames(from)[y])
		
		if(ratings) {
			for(i in 1:length(rts)) {
				names(rts[[i]]) <- lst[[i]]
			}
		}else{
			rts <- lst
		}
		
		names(rts) <- rownames(from)
		rts
	}
)

#ratingMatrixFromList <- function(from, itemLabels, userLabels = NULL) {
#	i <- rep(1:length(from), lapply(from, length))
#	j <- unlist(from)
#
#	if(is.null(userLabels)) userLabels <- names(from)
#
#	dgT <- new("dgTMatrix", i = i-1L, j = as.integer(j)-1L,
#		x = rep(1, length(j)),
#		Dim = c(length(from), length(itemLabels)),
#		Dimnames = list(userLabels, itemLabels))
#
#	as(as(dgT, "dgCMatrix"), "ratingMatrix")
#}


## from a list with userID as labels and items (only binary)
#setAs("list", "ratingMatrix", function(from) {
#		i <- rep(1:length(from), lapply(from, length))
#		j <- as.factor(unlist(from))
#
#		dgT <- new("dgTMatrix", i = i-1L, j = as.integer(j)-1L, 
#			x = rep(1, length(j)),
#			Dim = c(length(from), length(levels(j))),
#			Dimnames = list(labels(from),levels(j)))
#	
#		as(as(dgT, "dgCMatrix"), "ratingMatrix")
#	})
#

setAs("ratingMatrix", "list", function(from) LIST(from))



### FIXME: I dont know how that works
#setMethod("subset", signature(x = "realRatingMatrix"),
	#	function(x, subset, ...){
		#		if (missing(subset)) return(x)
		#i <- eval(substitute(subset, list(x= as(x, "dgCMatrix"))))
		#
		#new("realRatingMatrix", as(drop0(as(x, "dgCMatrix")*as(i,"dgCMatrix"))))
		#	})

setMethod("binarize", signature(x = "realRatingMatrix"),
	function(x, threshold = 3, ...){
		x <- x@data
		x <- drop0(x*(x>=threshold))
		x <- new("itemMatrix", data = t(as(x, "ngCMatrix")), 
			itemInfo = data.frame(labels=colnames(x)))
		new("binaryRatingMatrix", data = x)
	})


## create test data
setMethod(".splitKnownUnknown", signature(data="realRatingMatrix"),
	function(data, given) {

		## given might of length one or length(data)
		if(length(given)==1) given <- rep(given, nrow(data))

		## we create a logical mask via a triplet Matrix
		trip <- as(data, "dgTMatrix")
		items <- lapply(0:(nrow(data)-1), function(i) which(trip@i == i))
		take <- unlist(lapply(items, sample, given))

		tripUnknown <- trip
		tripUnknown@x[take] <- 0
		tripKnown <- trip
		tripKnown@x[-take] <- 0

		known <- new("realRatingMatrix", 
			data = as(tripKnown, "dgCMatrix"))
		unknown <- new("realRatingMatrix", 
			data = as(tripUnknown, "dgCMatrix"))

		list(
			known = known,
			unknown = unknown
		)
	})


