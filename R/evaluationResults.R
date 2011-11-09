setMethod("show", signature(object = "evaluationResults"),
	function(object) {
		writeLines(sprintf("Evaluation results for %d runs using method %s.",
				getRuns(object), sQuote(object@method)))
		if(any(!sapply(getModel(object), is.null)))
		writeLines("Result contains predictive models!")

		invisible(NULL)
	})


setMethod("getRuns", signature(x = "evaluationResults"),
	function(x, ...) length(x@results))

setMethod("getModel", signature(x = "evaluationResults"),
	function(x, ...) {
		lapply(x@results, function(y) y@model)
	})

setMethod("getConfusionMatrix", signature(x = "evaluationResults"),
	function(x, ...) {
		lapply(x@results, function(y) y@cm)
	})

setMethod("avg", signature(x = "evaluationResults"),
	function(x, trim = 0, na.rm = FALSE, ...) {

	    x <- getConfusionMatrix(x)

	    if(length(x)>1) {
		avg <- x[[1]]
		for(i in 2:length(x)) avg <- avg+x[[i]]
		x <- avg/length(x)
	    }else{
		x <- x[[1]]
	    }
	    
	    x
	})

setMethod("plot", signature(x = "evaluationResults"),
	function(x, y=NULL, plot_type=c("ROC", "prec/rec"),
		avg = TRUE, add=FALSE, type= "l", annotate = FALSE, ...) {

		plot_type <- match.arg(plot_type)
		## if not ROC then prec/recall
		if(plot_type == "ROC") take <- c("FPR", "TPR")
		else take <- c("recall", "precision")

		if(avg) {
			x <- avg(x)

			x <- x[,take]
			if(add) lines(x, type=type,...) 
			else plot(x, type=type, ...)

			## add annodations
			if(annotate) text(x[,1], x[,2], pos=3, rownames(x))
		}else{
			cm <- getConfusionMatrix(x)
			
			## plot first
			x <- cm[[1]][,take]
			if(add) lines(x, type=type,...)
			else plot(x, type=type, ...)
			
			## add annodations
			if(annotate) text(x[,1], x[,2], pos=3, rownames(x))

			## plot rest
			x <- cm[-1, drop = FALSE]

			tmp <- lapply(x, function(y) {
					y<-y[,take]
					lines(y, type=type,...)
				})
		}
	})


