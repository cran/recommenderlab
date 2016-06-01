### R code from vignette source 'recommenderlab.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: recommenderlab.Rnw:101-104
###################################################
options(scipen=3, digits=4, prompt="R> ", eps=FALSE, width=75)
### for sampling
set.seed(1234)


###################################################
### code chunk number 2: recommenderlab.Rnw:1060-1061
###################################################
library("recommenderlab")


###################################################
### code chunk number 3: recommenderlab.Rnw:1069-1074
###################################################
m <- matrix(sample(c(as.numeric(0:5), NA), 50,
    replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
    dimnames=list(user=paste("u", 1:5, sep=''),
	item=paste("i", 1:10, sep='')))
m


###################################################
### code chunk number 4: recommenderlab.Rnw:1082-1085
###################################################
r <- as(m, "realRatingMatrix")
r
getRatingMatrix(r)


###################################################
### code chunk number 5: recommenderlab.Rnw:1090-1091
###################################################
identical(as(r, "matrix"),m)


###################################################
### code chunk number 6: recommenderlab.Rnw:1098-1100
###################################################
as(r, "list")
head(as(r, "data.frame"))


###################################################
### code chunk number 7: recommenderlab.Rnw:1115-1118
###################################################
r_m <- normalize(r)
r_m
getRatingMatrix(r_m)


###################################################
### code chunk number 8: recommenderlab.Rnw:1122-1123
###################################################
denormalize(r_m)


###################################################
### code chunk number 9: recommenderlab.Rnw:1129-1131 (eval = FALSE)
###################################################
## image(r, main = "Raw Ratings")
## image(r_m, main = "Normalized Ratings")


###################################################
### code chunk number 10: image1
###################################################
print(image(r, main = "Raw Ratings"))


###################################################
### code chunk number 11: image2
###################################################
print(image(r_m, main = "Normalized Ratings"))


###################################################
### code chunk number 12: recommenderlab.Rnw:1164-1167
###################################################
r_b <- binarize(r, minRating=4)
r_b
as(r_b, "matrix")


###################################################
### code chunk number 13: recommenderlab.Rnw:1179-1181
###################################################
data(Jester5k)
Jester5k


###################################################
### code chunk number 14: recommenderlab.Rnw:1190-1193
###################################################
set.seed(1234)
r <- sample(Jester5k, 1000)
r


###################################################
### code chunk number 15: recommenderlab.Rnw:1200-1203
###################################################
rowCounts(r[1,])
as(r[1,], "list")
rowMeans(r[1,])


###################################################
### code chunk number 16: hist1
###################################################
hist(getRatings(r), breaks=100)


###################################################
### code chunk number 17: hist2
###################################################
hist(getRatings(normalize(r)), breaks=100)


###################################################
### code chunk number 18: hist3
###################################################
hist(getRatings(normalize(r, method="Z-score")), breaks=100)


###################################################
### code chunk number 19: hist4
###################################################
hist(rowCounts(r), breaks=50)


###################################################
### code chunk number 20: hist5
###################################################
hist(colMeans(r), breaks=20)


###################################################
### code chunk number 21: recommenderlab.Rnw:1292-1293
###################################################
recommenderRegistry$get_entries(dataType = "realRatingMatrix")


###################################################
### code chunk number 22: recommenderlab.Rnw:1301-1303
###################################################
r <- Recommender(Jester5k[1:1000], method = "POPULAR")
r


###################################################
### code chunk number 23: recommenderlab.Rnw:1307-1309
###################################################
names(getModel(r))
getModel(r)$topN


###################################################
### code chunk number 24: recommenderlab.Rnw:1324-1326
###################################################
recom <- predict(r, Jester5k[1001:1002], n=5)
recom


###################################################
### code chunk number 25: recommenderlab.Rnw:1331-1332
###################################################
as(recom, "list")


###################################################
### code chunk number 26: recommenderlab.Rnw:1338-1341
###################################################
recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")


###################################################
### code chunk number 27: recommenderlab.Rnw:1349-1352
###################################################
recom <- predict(r, Jester5k[1001:1002], type="ratings")
recom
as(recom, "matrix")[,1:10]


###################################################
### code chunk number 28: recommenderlab.Rnw:1363-1366
###################################################
recom <- predict(r, Jester5k[1001:1002], type="ratingMatrix")
recom
as(recom, "matrix")[,1:10]


###################################################
### code chunk number 29: recommenderlab.Rnw:1381-1384
###################################################
e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9,
    given=15, goodRating=5)
e


###################################################
### code chunk number 30: recommenderlab.Rnw:1390-1395
###################################################
r1 <- Recommender(getData(e, "train"), "UBCF")
r1

r2 <- Recommender(getData(e, "train"), "IBCF")
r2


###################################################
### code chunk number 31: recommenderlab.Rnw:1402-1406
###################################################
p1 <- predict(r1, getData(e, "known"), type="ratings")
p1
p2 <- predict(r2, getData(e, "known"), type="ratings")
p2


###################################################
### code chunk number 32: recommenderlab.Rnw:1412-1417
###################################################
error <- rbind(
  UBCF = calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF = calcPredictionAccuracy(p2, getData(e, "unknown"))
)
error


###################################################
### code chunk number 33: recommenderlab.Rnw:1430-1433
###################################################
scheme <- evaluationScheme(Jester5k[1:1000], method="cross", k=4, given=3,
    goodRating=5)
scheme


###################################################
### code chunk number 34: recommenderlab.Rnw:1440-1443
###################################################
results <- evaluate(scheme, method="POPULAR", type = "topNList",
  n=c(1,3,5,10,15,20))
results


###################################################
### code chunk number 35: recommenderlab.Rnw:1454-1455
###################################################
getConfusionMatrix(results)[[1]]


###################################################
### code chunk number 36: recommenderlab.Rnw:1467-1468
###################################################
avg(results)


###################################################
### code chunk number 37: roc1
###################################################
plot(results, annotate=TRUE)


###################################################
### code chunk number 38: precrec1
###################################################
plot(results, "prec/rec", annotate=TRUE)


###################################################
### code chunk number 39: recommenderlab.Rnw:1515-1531
###################################################
set.seed(2016)
scheme <- evaluationScheme(Jester5k[1:1000], method="split", train = .9,
  k=1, given=-5, goodRating=5)
scheme

algorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "user-based CF" = list(name="UBCF", param=list(nn=50)),
  "item-based CF" = list(name="IBCF", param=list(k=50)),
  "SVD approximation" = list(name="SVD", param=list(k = 50))
)

## run algorithms
results <- evaluate(scheme, algorithms, type = "topNList",
  n=c(1, 3, 5, 10, 15, 20))


###################################################
### code chunk number 40: recommenderlab.Rnw:1536-1537
###################################################
results


###################################################
### code chunk number 41: recommenderlab.Rnw:1543-1545
###################################################
names(results)
results[["user-based CF"]]


###################################################
### code chunk number 42: roc2
###################################################
plot(results, annotate=c(1,3), legend="bottomright")


###################################################
### code chunk number 43: precrec2
###################################################
plot(results, "prec/rec", annotate=3, legend="topleft")


###################################################
### code chunk number 44: recommenderlab.Rnw:1588-1590
###################################################
## run algorithms
results <- evaluate(scheme, algorithms, type = "ratings")


###################################################
### code chunk number 45: recommenderlab.Rnw:1595-1596
###################################################
results


###################################################
### code chunk number 46: real
###################################################
plot(results, ylim = c(0,100))


###################################################
### code chunk number 47: recommenderlab.Rnw:1618-1624
###################################################
Jester_binary <- binarize(Jester5k, minRating=5)
Jester_binary <- Jester_binary[rowCounts(Jester_binary)>20]
Jester_binary
scheme_binary <- evaluationScheme(Jester_binary[1:1000],
	method="split", train=.9, k=1, given=3)
scheme_binary


###################################################
### code chunk number 48: recommenderlab.Rnw:1627-1629
###################################################
results_binary <- evaluate(scheme_binary, algorithms,
  type = "topNList", n=c(1,3,5,10,15,20))


###################################################
### code chunk number 49: roc3
###################################################
plot(results_binary, annotate=c(1,3), legend="topright")


