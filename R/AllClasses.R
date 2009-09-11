## Ratings
setClass("ratingMatrix")

setClass("binaryRatingMatrix",
	contains="ratingMatrix",
	representation(
		data = "itemMatrix"
	))

setClass("realRatingMatrix",
	contains="ratingMatrix",
	representation(
		data = "dgCMatrix"
	))


## Recommender
setClass("Recommender",
	representation(
		method	= "character", 
		dataType= "character", 
		ntrain	= "integer",
		model	= "list",
		predict = "function"
	)
)

## Top-N list
setClass("topNList",
	representation(
		items   = "list",
		itemLabels= "character",
		n       = "integer"
	)
)


## Evaluation
setClass("evaluationScheme",
	representation(
		method	= "character",
		given	= "integer",
		k	= "integer",
		train	= "numeric",	
		runsTrain= "list",
		data	= "ratingMatrix",
		knownData= "ratingMatrix",
		unknownData= "ratingMatrix"
	)
)


setClassUnion("listOrNull", c("list", "NULL"))

setClass("confusionMatrix",
	representation(
		cm	= "matrix",
		model	= "listOrNull"
	)
)

setClass("evaluationResults",
	representation(
		results	= "list",	## list of confusionMatrix
		method	= "character"
	)
)

setClass("evaluationResultList",
	contains="list"			## list of evaluationResults
)


