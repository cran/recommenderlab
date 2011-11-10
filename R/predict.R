setMethod("predict", signature(object = "Recommender"),
	function(object, newdata, n = 10, type="topNList", ...) 
	object@predict(object@model, newdata, n = n, type= type, ...)
)


