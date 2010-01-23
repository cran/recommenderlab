## always recommends the top-N popular items (without known items)
BIN_POPULAR <- function(data, parameter = NULL) {

	model <- list(
		description = "Order of items by popularity",
		popOrder = order(colCounts(data), decreasing=TRUE)
		## the following would be non-binary
		#popOrder = order(colMeans(data), decreasing=TRUE)
	)

    predict <- function(model, newdata, n=10) {
		n <- as.integer(n)

		## remove known items and take highest
		reclist <- lapply(LIST(newdata, decode= FALSE),
			function(x) head(model$popOrder[!(model$popOrder %in% x)], n))

        new("topNList", items = reclist, itemLabels = colnames(newdata), n = n)
    }

	## construct recommender object
	new("Recommender", method = "POPULAR", dataType = "binaryRatingMatrix",
		ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
	method="POPULAR", dataType = "binaryRatingMatrix", fun=BIN_POPULAR, 
	description="Recommender based on item popularity (binary data)."
)

