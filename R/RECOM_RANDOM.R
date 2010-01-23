## recommend random unknown items
RANDOM <- function(data=NULL, parameter=NULL) {

    predict <- function(model=NULL, newdata, n=10) {
		n <- as.integer(n)
		itemlist <- 1:ncol(newdata)
        
        ## remove known items and sample
        reclist <- lapply(LIST(newdata, decode = FALSE, ratings = FALSE), 
            FUN = function(x) sample(itemlist[-x], 
				min(length(itemlist[-x]), n)))
        
		new("topNList", items = reclist, itemLabels = colnames(newdata), n = n)
	}

	## this recommender has no model
	new("Recommender", method = "RANDOM", dataType = "ratingMatrix", 
		ntrain = nrow(data),
		model = list(), predict = predict)
}

## register recommender
recommenderRegistry$set_entry(method="RANDOM", dataType="realRatingMatrix", 
	fun=RANDOM, 
	description="Produce random recommendations (real ratings).")

recommenderRegistry$set_entry(method="RANDOM", dataType="binaryRatingMatrix", 
	fun=RANDOM, 
	description="Produce random recommendations (binary ratings).")

