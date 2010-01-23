library("recommenderlab")
data(Groceries)

Groceries <- as(Groceries, "binaryRatingMatrix")

Groceries5 <- Groceries[rowCounts(Groceries) >5,]
Groceries5

scheme1 <- evaluationScheme(Groceries5, method="cross", k=4, given=3)
scheme2 <- evaluationScheme(Groceries5, method="cross", k=4, given=1)

algorithms <- list(
        "random items" = list(name="RANDOM", param=NULL),
        "popular items" = list(name="POPULAR", param=NULL),
        "user-based CF" = list(name="UBCF", param=list(method="Jaccard", nn=50)),
        "item-based CF" = list(name="IBCF", param=list(method= "Jaccard", k=50)),
        "association rules" = list(name="AR", param=list(supp=0.002, conf=0.2, maxlen=2, verbose=TRUE))
        )

results1 <- evaluate(scheme1, algorithms, n=c(1,3,5,10,15,20))
results2 <- evaluate(scheme2, algorithms, n=c(1,3,5,10,15,20))


save(results1, results2, file="comp_Groceries.Rda")

plot(results1, legend="right")
plot(results2, legend="right")




