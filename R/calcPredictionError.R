setMethod("calcPredictionError", signature(x= "realRatingMatrix", 
		data = "realRatingMatrix"),

	function(x, data, byUser=FALSE) {
	    if(byUser) fun <- rowMeans
	    else fun <- mean

	    MAE <- fun(abs(as(x, "matrix") - as(data,"matrix")), 
		    na.rm=TRUE)
	    MSE <- fun((as(x, "matrix") - as(data,"matrix"))^2, 
		    na.rm=TRUE)
	    RMSE <- sqrt(MSE)

	    cbind(MAE, MSE, RMSE)
	})
