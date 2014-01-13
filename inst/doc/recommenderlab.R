### R code from vignette source 'recommenderlab.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: recommenderlab.Rnw:101-104
###################################################
options(scipen=3, digits=4, prompt="R> ", eps=FALSE, width=75)
### for sampling
set.seed(1234)


###################################################
### code chunk number 2: recommenderlab.Rnw:1056-1057
###################################################
library("recommenderlab")


###################################################
### code chunk number 3: recommenderlab.Rnw:1065-1070
###################################################
m <- matrix(sample(c(as.numeric(0:5), NA), 50, 
    replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
    dimnames=list(user=paste("u", 1:5, sep=''), 
	item=paste("i", 1:10, sep='')))
m


###################################################
### code chunk number 4: recommenderlab.Rnw:1077-1080
###################################################
r <- as(m, "realRatingMatrix")
r
#as(r,"dgCMatrix")


###################################################
### code chunk number 5: recommenderlab.Rnw:1085-1086
###################################################
identical(as(r, "matrix"),m)


###################################################
### code chunk number 6: recommenderlab.Rnw:1094-1096
###################################################
as(r, "list")
head(as(r, "data.frame"))


###################################################
### code chunk number 7: recommenderlab.Rnw:1109-1111
###################################################
r_m <- normalize(r)
r_m


###################################################
### code chunk number 8: recommenderlab.Rnw:1117-1119 (eval = FALSE)
###################################################
## image(r, main = "Raw Ratings")
## image(r_m, main = "Normalized Ratings")


###################################################
### code chunk number 9: image1
###################################################
print(image(r, main = "Raw Ratings"))


###################################################
### code chunk number 10: image2
###################################################
print(image(r_m, main = "Normalized Ratings"))


###################################################
### code chunk number 11: recommenderlab.Rnw:1152-1154
###################################################
r_b <- binarize(r, minRating=4)
as(r_b, "matrix")


###################################################
### code chunk number 12: recommenderlab.Rnw:1166-1168
###################################################
data(Jester5k)
Jester5k


###################################################
### code chunk number 13: recommenderlab.Rnw:1176-1178
###################################################
r <- sample(Jester5k, 1000) 
r


###################################################
### code chunk number 14: recommenderlab.Rnw:1185-1188
###################################################
rowCounts(r[1,])
as(r[1,], "list")
rowMeans(r[1,])


###################################################
### code chunk number 15: hist1
###################################################
hist(getRatings(r), breaks=100)


###################################################
### code chunk number 16: hist2
###################################################
hist(getRatings(normalize(r)), breaks=100)


###################################################
### code chunk number 17: hist3
###################################################
hist(getRatings(normalize(r, method="Z-score")), breaks=100)


###################################################
### code chunk number 18: hist4
###################################################
hist(rowCounts(r), breaks=50)


###################################################
### code chunk number 19: hist5
###################################################
hist(colMeans(r), breaks=20)


###################################################
### code chunk number 20: recommenderlab.Rnw:1275-1276
###################################################
recommenderRegistry$get_entries(dataType = "realRatingMatrix")


###################################################
### code chunk number 21: recommenderlab.Rnw:1284-1286
###################################################
r <- Recommender(Jester5k[1:1000], method = "POPULAR")
r


###################################################
### code chunk number 22: recommenderlab.Rnw:1290-1292
###################################################
names(getModel(r))
getModel(r)$topN


###################################################
### code chunk number 23: recommenderlab.Rnw:1307-1309
###################################################
recom <- predict(r, Jester5k[1001:1002], n=5)
recom


###################################################
### code chunk number 24: recommenderlab.Rnw:1314-1315
###################################################
as(recom, "list")


###################################################
### code chunk number 25: recommenderlab.Rnw:1321-1324
###################################################
recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")


###################################################
### code chunk number 26: recommenderlab.Rnw:1332-1335
###################################################
recom <- predict(r, Jester5k[1001:1002], type="ratings")
recom
as(recom, "matrix")[,1:10]


###################################################
### code chunk number 27: recommenderlab.Rnw:1355-1358
###################################################
e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9, 
    given=15, goodRating=5)
e


###################################################
### code chunk number 28: recommenderlab.Rnw:1364-1369
###################################################
r1 <- Recommender(getData(e, "train"), "UBCF")
r1

r2 <- Recommender(getData(e, "train"), "IBCF")
r2


###################################################
### code chunk number 29: recommenderlab.Rnw:1376-1380
###################################################
p1 <- predict(r1, getData(e, "known"), type="ratings")
p1
p2 <- predict(r2, getData(e, "known"), type="ratings")
p2


###################################################
### code chunk number 30: recommenderlab.Rnw:1386-1392
###################################################
error <- rbind(
  calcPredictionError(p1, getData(e, "unknown")),
  calcPredictionError(p2, getData(e, "unknown"))
)
rownames(error) <- c("UBCF","IBCF")
error


###################################################
### code chunk number 31: recommenderlab.Rnw:1405-1408
###################################################
scheme <- evaluationScheme(Jester5k[1:1000], method="cross", k=4, given=3,
    goodRating=5)
scheme


###################################################
### code chunk number 32: recommenderlab.Rnw:1415-1417
###################################################
results <- evaluate(scheme, method="POPULAR", n=c(1,3,5,10,15,20))
results


###################################################
### code chunk number 33: recommenderlab.Rnw:1428-1429
###################################################
getConfusionMatrix(results)[[1]]


###################################################
### code chunk number 34: recommenderlab.Rnw:1441-1442
###################################################
avg(results)


###################################################
### code chunk number 35: roc1
###################################################
plot(results, annotate=TRUE)


###################################################
### code chunk number 36: precrec1
###################################################
plot(results, "prec/rec", annotate=TRUE)


###################################################
### code chunk number 37: recommenderlab.Rnw:1487-1500
###################################################
scheme <- evaluationScheme(Jester5k[1:1000], method="split", train = .9, 
    k=1, given=20, goodRating=5)
scheme

algorithms <- list(
        "random items" = list(name="RANDOM", param=NULL),
        "popular items" = list(name="POPULAR", param=NULL),
        "user-based CF" = list(name="UBCF", param=list(method="Cosine", 
		nn=50, minRating=5))
        )

## run algorithms
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))


###################################################
### code chunk number 38: recommenderlab.Rnw:1505-1506
###################################################
results


###################################################
### code chunk number 39: recommenderlab.Rnw:1512-1514
###################################################
names(results)
results[["user-based CF"]]


###################################################
### code chunk number 40: roc2
###################################################
plot(results, annotate=c(1,3), legend="topleft")


###################################################
### code chunk number 41: precrec2
###################################################
plot(results, "prec/rec", annotate=3)


###################################################
### code chunk number 42: recommenderlab.Rnw:1555-1566
###################################################
Jester_binary <- binarize(Jester5k, minRating=5)
Jester_binary <- Jester_binary[rowCounts(Jester_binary)>20]
Jester_binary
scheme_binary <- evaluationScheme(Jester_binary[1:1000], method="split", train=.9, k=1, given=20)
scheme_binary

algorithms_binary <- list(
        "random items" = list(name="RANDOM", param=NULL),
        "popular items" = list(name="POPULAR", param=NULL),
        "user-based CF" = list(name="UBCF", param=list(method="Jaccard", nn=50))
        )


###################################################
### code chunk number 43: recommenderlab.Rnw:1569-1570
###################################################
results_binary <- evaluate(scheme_binary, algorithms_binary, n=c(1,3,5,10,15,20))


###################################################
### code chunk number 44: roc3
###################################################
plot(results_binary, annotate=c(1,3), legend="bottomright")


