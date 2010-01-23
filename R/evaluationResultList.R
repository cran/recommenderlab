
## we have to make sure it's all evaluationResults
setAs("list", "evaluationResultList",
	function(from) {
		if(!all(sapply(from, is, "evaluationResults"))) stop("List can only contain evaluationResults!")
		
		new("evaluationResultList", from)
	})

setMethod("show", signature(object = "evaluationResultList"),
	function(object) {
		writeLines(sprintf("List of evaluation results for %d recommenders:",
				length(object)))
		lapply(object, show)
		invisible(NULL)
	})


## plot
setMethod("plot", signature(x = "evaluationResultList"),
	function(x, y=NULL, plot_type=c("ROC", "prec/rec"),
    xlim=NULL, ylim=NULL, col = NULL, pch = 1, lty = 1, 
    annotate= 0, legend="bottomright", ...) {

    ## find best xlim, ylim
    plot_type <- match.arg(plot_type)
    take <- if(plot_type == "ROC") c("FPR", "TPR") else c("recall", "precision")

    max_lim <- apply(sapply(x, FUN = 
            function(y) apply(avg(y)[,take], MARGIN=2, max)), MARGIN=1, max)

    if(is.null(xlim)) xlim <- c(0, max_lim[1])
    if(is.null(ylim)) ylim <- c(0, max_lim[2])
    
    ## fix pch, lty and col
    if(length(pch)==1) pch <- rep(pch, length(x))
    if(length(lty)==1) lty <- rep(lty, length(x))
    
    if(is.null(col)) col <- 1:length(x)
    if(length(col)==1) col <- rep(col, length(x))
        

    plot(NA, xlab=take[1], ylab=take[2], ylim=ylim, xlim=xlim)
    legend(x=legend, legend=names(x), col=col, 
        pch = pch, lty=lty, bty="n")
    for(i in 1:length(x)) plot(x[[i]], plot_type=plot_type, 
        add=TRUE, col=col[i], type="o", annotate = i %in% annotate, 
        pch = pch[i], lty=lty[i])
})

## avg
setMethod("avg", signature(x = "evaluationResultList"), 
	function(x, trim = 0, na.rm = FALSE, ...) { 
		lapply(x, avg)
	})

setMethod("[", signature(x = "evaluationResultList", i = "ANY", j = "missing",
			drop = "missing"),
	function(x, i, j, ..., drop) {
            l <- as(as(x, "list")[i], "evaluationResultList")
            names(l) <- names(x)[i]
            l
        })

## work out of the box: names, [[


