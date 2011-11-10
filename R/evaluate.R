
setMethod("evaluate", signature(x = "evaluationScheme", method = "character"),
	function(x, method, n=1:10, parameter=NULL, 
		progress = TRUE, keepModel=FALSE) {

		scheme <- x
		runs <- 1:scheme@k

		if(progress) cat(method, "run ")
		
		cm <- list()
                for(r in runs) {
                    if(progress) cat("\n\t",r, " ")

		    cm[[r]] <- .do_run_by_n(scheme, method, 
			    run=r, n=n, parameter=parameter, 
			    progress=progress, keepModel=keepModel)
                }
			
		#if(progress) cat("\n")

		new("evaluationResults", results = cm, 
			method=recommenderRegistry$get_entry(method)$method)
	})

setMethod("evaluate", signature(x = "evaluationScheme", method = "list"),
	function(x, method, n=1:10, parameter=NULL, 
		progress = TRUE, keepModel=FALSE) {
	
		## method is a list of lists
		#list(RANDOM = list(name = "RANDOM", parameter = NULL), 
		#	POPULAR = list(...
	
		results <- lapply(method, FUN = function(a) try(evaluate(x, a$n,
				n = n , parameter = a$p)))	

		## handle recommenders that have failed
		errs <- sapply(results, is, "try-error")
		if(any(errs)) {
		    warning(paste("\n  Recommender '", names(results)[errs], 
				    "' has failed and has been removed from the results!", sep=''))
			results[errs] <- NULL
		}

		as(results, "evaluationResultList")
	})


## evaluation work horse
.do_run_by_n <- function(scheme, method, run, n, parameter = NULL,
	progress=FALSE, keepModel=TRUE) {

	## prepare data
	train <- getData(scheme, type="train", run=run)
	test_known <- getData(scheme, type="known", run=run)
	test_unknown <- getData(scheme, type="unknown", run=run)

	### binarize a realRatingMatrix?
	if (is(test_known, "realRatingMatrix")) {
	    if(is.na(scheme@goodRating)) stop("You need to set goodRating in the evaluationScheme for a realRatingMatrix!")

	    test_unknown <- binarize(test_unknown, scheme@goodRating)
	}

	## train recommender
	time_model <- system.time(
		r <- Recommender(train, method, parameter=parameter)
		)
	
	cm <- matrix(NA, nrow=length(n), ncol=9, 
		dimnames= list(n=n, 
			c("TP", "FP", "FN", "TN", "PP", "recall","precision","FPR","TPR")))

	
	time_predict <- system.time(
		topN <- predict(r, test_known, n=max(n))
		)
	
	for(i in 1:length(n)) {
		NN <- n[i]

		## get best N
		pred <- bestN(topN, NN)

		## create confusion matrix
		tp <- rowSums(as(pred, "ngCMatrix") * as(test_unknown, "ngCMatrix"))
		## The algorithm predicted known items!!!
		pred_known <- rowSums(as(pred, "ngCMatrix") * as(test_known, "ngCMatrix"))
		if(any(pred_known>0)) warning(paste("The algorithm ", 
				r@model," predicted known items!!!"))

		tp_fn <- rowCounts(test_unknown)
		tp_fp <- rowCounts(pred)

		cm[i, "TP"] <- mean(tp)
		cm[i, "FP"] <- mean(tp_fp - tp)
		cm[i, "FN"] <- mean(tp_fn - tp)
		## Reduced TN by the number of given items. Bug
		## reported by ("Zhang, Martin F" <Martin.F.Zhang@asia.ccb.com>)
		cm[i, "TN"] <- ncol(train) - scheme@given + mean(pred_known) - cm[i, "TP"] -  cm[i, "FP"] - cm[i, "FN"]
		cm[i, "PP"] <- mean(tp_fp)

		## calculate some important measures
		cm[i, "precision"] <- cm[i, "TP"] / (cm[i, "TP"] + cm[i, "FP"]) 
		cm[i, "recall"] <- cm[i, "TP"] / (cm[i, "TP"] + cm[i, "FN"]) 
		cm[i, "TPR"] <- cm[i, "recall"] 
		cm[i, "FPR"] <- cm[i, "FP"] / (cm[i, "FP"] + cm[i, "TN"]) 

	}
                    
	time_usage <- function(x) x[1]+x[2]

	if(progress) cat("[", 
		time_usage(time_model), "sec/",
		time_usage(time_predict),"sec] ", sep="")

	new("confusionMatrix", cm = cm, model = 
		if(keepModel) getModel(r) else NULL)
}

