## recommend random unknown items

RANDOM <- function(data=NULL, parameter=NULL) {

    model <- list(range = range(as(data, "dgCMatrix")))

    predict <- function(model=NULL, newdata, n=10, 
	    type=c("topNList", "ratings"), ...) {

	type <- match.arg(type)

	## create random ratings 
	ratings <- matrix(runif(nrow(newdata)*ncol(newdata), 
			model$range[1], model$range[2]),
		nrow=nrow(newdata), ncol=ncol(newdata), 
		dimnames=dimnames(newdata))

	ratings <- as(ratings, "realRatingMatrix")
	ratings <- removeKnownRatings(ratings, newdata)

	if(type=="ratings") return(ratings)
	
	return(getTopNLists(ratings, n))
    }

    ## this recommender has no model
    new("Recommender", method = "RANDOM", dataType = "ratingMatrix", 
	    ntrain = nrow(data),
	    model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(method="RANDOM", dataType="realRatingMatrix", 
	fun=RANDOM, 
	description="Produce random recommendations (real ratings).")

recommenderRegistry$set_entry(method="RANDOM", dataType="binaryRatingMatrix", 
	fun=RANDOM, 
	description="Produce random recommendations (binary ratings).")

