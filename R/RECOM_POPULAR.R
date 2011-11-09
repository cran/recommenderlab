## always recommends the top-N popular items (without known items)
BIN_POPULAR <- function(data, parameter = NULL) {

    topN <- new("topNList", 
	    items = list(order(colCounts(data), decreasing=TRUE)),
	    itemLabels = colnames(data),
	    n = ncol(data))

    model <- list( topN = topN )

    predict <- function(model, newdata, n=10, ...) {
	topN <- removeKnownItems(model$topN, newdata, replicate=TRUE)
	topN <- bestN(topN, n)
	return(topN)
    }

    ## construct recommender object
    new("Recommender", method = "POPULAR", dataType = class(data),
	    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
	method="POPULAR", dataType = "binaryRatingMatrix", fun=BIN_POPULAR, 
	description="Recommender based on item popularity (binary data)."
)




## always recommends the top-N popular items (without known items)
REAL_POPULAR <- function(data, parameter = NULL) {

    p <- .get_parameters(list(
		    normalize="center",
		    aggregation=colSums ## could also be colMeans
		    ), parameter)

    ## normalize data
    if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)
    
    topN <- new("topNList", 
	    items = list(order(p$aggregation(data), decreasing=TRUE)),
	    itemLabels = colnames(data),
	    n= ncol(data))

    ratings <- new("realRatingMatrix", data = dropNA(t(colMeans(data))))

    model <- c(list(
	    topN = topN,
	    ratings = ratings
	    ), p)

    predict <- function(model, newdata, n=10,
	    type=c("topNList", "ratings"), ...) {

	type <- match.arg(type)
    
	if(type=="topNList") {
	    topN <- removeKnownItems(model$topN, newdata, replicate=TRUE)
	    topN <- bestN(topN, n)
	    return(topN)
	}

	## ratings always use colMeans!
	if(!is.null(model$normalize)) 
	    newdata <- normalize(newdata, method=model$normalize)

	ratings <- removeKnownRatings(model$ratings, newdata, replicate=TRUE)
	ratings <- denormalize(ratings, factors=getNormalize(newdata))

	return(ratings)
    }

    ## construct recommender object
    new("Recommender", method = "POPULAR", dataType = class(data),
	    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
	method="POPULAR", dataType = "realRatingMatrix", fun=REAL_POPULAR, 
	description="Recommender based on item popularity (real data).")
