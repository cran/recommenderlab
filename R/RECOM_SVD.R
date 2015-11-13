### Contributed by Saurabh Bathnagar (sbhatnagar@book.com)

.REAL_SVD_param <- list(
  categories = 50,
  method="Cosine",
  normalize = "center",
  normalize_sim_matrix = FALSE,
  alpha = 0.5,
  treat_na = "median",
  minRating = NA
)


REAL_SVD <- function(data, parameter= NULL) {
  
  p <- .get_parameters(.REAL_SVD_param, parameter)
  
  
  if(!is.null(p$normalize))
    data <- normalize(data, method=p$normalize)
  
  model <- c(list(
    description = "full matrix",
    data = data
    ), p)
  
  predict <- function(model, newdata, n = 10,
	  data=NULL, type=c("topNList", "ratings"), ...) {
    
    type <- match.arg(type)
    
    ## newdata are userid
    if(is.numeric(newdata)) {
	if(is.null(data) || !is(data, "ratingMatrix"))
	    stop("If newdata is a user id then data needes to be the training dataset.")
	newdata <- data[newdata,]
    }
    
    n <- as.integer(n)
    
    if(!is.null(model$normalize))
      newdata <- normalize(newdata, method=model$normalize)

    # Perform SVD
    data <- model$data@data
    data <- rBind(data, newdata@data)
    
    ### FIXME: svd does as.matrix which sets all missing values to 0!
    data <- as(data, "matrix")

    if(model$treat_na=="min") data[is.na(data)] <- min(data, na.rm=TRUE)
    else if(model$treat_na=="mean") data[is.na(data)] <- mean(data, na.rm=TRUE)
    else if(model$treat_na=="median") data[is.na(data)] <- median(data, na.rm=TRUE)
    else if(model$treat_na=="max") data[is.na(data)] <- max(data, na.rm=TRUE)
    else if(model$treat_na=="0") data[is.na(data)] <- 0
    else stop("No valid way to treat NAs specified (treat_na)!")

    s<-svd(data)
    # Get Diag
    D <- diag(s$d[1:p$categories])
    
    ratings <- s$u[,1:p$categories] %*% D %*% t(s$v[,1:p$categories])
    # Put back correct names
    rownames(ratings) <- rownames(data)
    colnames(ratings) <- colnames(data)
    # Only need to give back new users
    ratings <- ratings[(dim(model$data@data)[1]+1):dim(ratings)[1],]
    if(is.null(dim(ratings))) {
        ratings <- matrix(ratings, nrow=1)
        rownames(ratings) <- rownames(data[(dim(model$data@data)[1]+1):(dim(model$data@data)[1]+1)])
        colnames(ratings) <- colnames(data)
    }
    
    ratings <- new("realRatingMatrix", data=dropNA(ratings))
    ## prediction done
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(!is.null(model$normalize))
      ratings <- denormalize(ratings)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  ## construct recommender object
  new("Recommender", method = "SVD", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}

recommenderRegistry$set_entry(
  method="SVD", dataType = "realRatingMatrix", fun=REAL_SVD,
  description="Recommender based on SVD approximation (real data).",
  parameters = .REAL_SVD_param)


