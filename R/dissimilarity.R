##*******************************************************************
## dissimilarity for binaryRatingMatrix
setMethod("dissimilarity", signature(x = "binaryRatingMatrix"),
	function(x, y = NULL, method = NULL, args = NULL, which = "users") {
	    ## dissimilarity is defined in arules for itemMatrix
	    if(which == "users") which <- "transactions" ## "items" is ok
	    x <- x@data
	    if(!is.null(y)) y <- y@data

	    dissimilarity(x, y, method, args, which)
	}
	)



##*******************************************************************
## wrapper for realRatingMatrix (transactions)
## by Christopher Köb

setMethod("dissimilarity", signature(x = "realRatingMatrix"),
function(x, y = NULL, method = NULL, args = NULL, 
which = "users") {
	
#	print("realRatingMatrix wrapper");
	    ## dissimilarity is defined in arules for itemMatrix
	    if(which == "users") which <- "transactions" ## "items" is ok

	    ## items instead of transactions?
	    items <- FALSE
	    if (pmatch(tolower(which), c("transactions", "items")) == 2) 
		items <- TRUE

	    x <- as(x, "matrix")
	    x[is.na(x)]<-0
	    if(items) x <- t(x) 

	    if(!is.null(y)) { 
		y <- as(y, "matrix")
		y[is.na(y)]<-0
		if(items) y <- t(y) 
	    }

	    dissimilarity(x = x, y = y, method = method, args = args)
	})


