setMethod("predict", signature(object = "Recommender"),
	function(object, newdata, n = 10, ...) 
	object@predict(object@model, newdata, n = n)
)


