### R code from vignette source 'recommenderlab.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: recommenderlab.Rnw:101-104
###################################################
options(scipen=3, digits=4, prompt="R> ", eps=FALSE, width=75)
### for sampling
set.seed(1234)


###################################################
### code chunk number 2: recommenderlab.Rnw:238-239
###################################################
citation("recommenderlab")


###################################################
### code chunk number 3: recommenderlab.Rnw:1064-1065
###################################################
library("recommenderlab")


###################################################
### code chunk number 4: recommenderlab.Rnw:1073-1078
###################################################
m <- matrix(sample(c(as.numeric(0:5), NA), 50,
    replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
    dimnames=list(user=paste("u", 1:5, sep=''),
	item=paste("i", 1:10, sep='')))
m


###################################################
### code chunk number 5: recommenderlab.Rnw:1086-1089
###################################################
r <- as(m, "realRatingMatrix")
r
getRatingMatrix(r)


###################################################
### code chunk number 6: recommenderlab.Rnw:1094-1095
###################################################
identical(as(r, "matrix"),m)


###################################################
### code chunk number 7: recommenderlab.Rnw:1102-1104
###################################################
as(r, "list")
head(as(r, "data.frame"))


###################################################
### code chunk number 8: recommenderlab.Rnw:1119-1122
###################################################
r_m <- normalize(r)
r_m
getRatingMatrix(r_m)


###################################################
### code chunk number 9: recommenderlab.Rnw:1126-1127
###################################################
denormalize(r_m)


###################################################
### code chunk number 10: recommenderlab.Rnw:1133-1135 (eval = FALSE)
###################################################
## image(r, main = "Raw Ratings")
## image(r_m, main = "Normalized Ratings")


###################################################
### code chunk number 11: image1
###################################################
print(image(r, main = "Raw Ratings"))


###################################################
### code chunk number 12: image2
###################################################
print(image(r_m, main = "Normalized Ratings"))


###################################################
### code chunk number 13: recommenderlab.Rnw:1168-1171
###################################################
r_b <- binarize(r, minRating=4)
r_b
as(r_b, "matrix")


###################################################
### code chunk number 14: recommenderlab.Rnw:1183-1185
###################################################
data(Jester5k)
Jester5k


###################################################
### code chunk number 15: recommenderlab.Rnw:1194-1197
###################################################
set.seed(1234)
r <- sample(Jester5k, 1000)
r


###################################################
### code chunk number 16: recommenderlab.Rnw:1204-1207
###################################################
rowCounts(r[1,])
as(r[1,], "list")
rowMeans(r[1,])


###################################################
### code chunk number 17: hist1
###################################################
hist(getRatings(r), breaks=100)


###################################################
### code chunk number 18: hist2
###################################################
hist(getRatings(normalize(r)), breaks=100)


###################################################
### code chunk number 19: hist3
###################################################
hist(getRatings(normalize(r, method="Z-score")), breaks=100)


###################################################
### code chunk number 20: hist4
###################################################
hist(rowCounts(r), breaks=50)


###################################################
### code chunk number 21: hist5
###################################################
hist(colMeans(r), breaks=20)


###################################################
### code chunk number 22: recommenderlab.Rnw:1296-1297
###################################################
recommenderRegistry$get_entries(dataType = "realRatingMatrix")


###################################################
### code chunk number 23: recommenderlab.Rnw:1305-1307
###################################################
r <- Recommender(Jester5k[1:1000], method = "POPULAR")
r


###################################################
### code chunk number 24: recommenderlab.Rnw:1311-1313
###################################################
names(getModel(r))
getModel(r)$topN


###################################################
### code chunk number 25: recommenderlab.Rnw:1328-1330
###################################################
recom <- predict(r, Jester5k[1001:1002], n=5)
recom


###################################################
### code chunk number 26: recommenderlab.Rnw:1335-1336
###################################################
as(recom, "list")


###################################################
### code chunk number 27: recommenderlab.Rnw:1342-1345
###################################################
recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")


###################################################
### code chunk number 28: recommenderlab.Rnw:1353-1356
###################################################
recom <- predict(r, Jester5k[1001:1002], type="ratings")
recom
as(recom, "matrix")[,1:10]


###################################################
### code chunk number 29: recommenderlab.Rnw:1367-1370
###################################################
recom <- predict(r, Jester5k[1001:1002], type="ratingMatrix")
recom
as(recom, "matrix")[,1:10]


###################################################
### code chunk number 30: recommenderlab.Rnw:1385-1388
###################################################
e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9,
    given=15, goodRating=5)
e


###################################################
### code chunk number 31: recommenderlab.Rnw:1394-1399
###################################################
r1 <- Recommender(getData(e, "train"), "UBCF")
r1

r2 <- Recommender(getData(e, "train"), "IBCF")
r2


###################################################
### code chunk number 32: recommenderlab.Rnw:1406-1410
###################################################
p1 <- predict(r1, getData(e, "known"), type="ratings")
p1
p2 <- predict(r2, getData(e, "known"), type="ratings")
p2


###################################################
### code chunk number 33: recommenderlab.Rnw:1416-1421
###################################################
error <- rbind(
  UBCF = calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF = calcPredictionAccuracy(p2, getData(e, "unknown"))
)
error


###################################################
### code chunk number 34: recommenderlab.Rnw:1434-1437
###################################################
scheme <- evaluationScheme(Jester5k[1:1000], method="cross", k=4, given=3,
    goodRating=5)
scheme


###################################################
### code chunk number 35: recommenderlab.Rnw:1444-1447
###################################################
results <- evaluate(scheme, method="POPULAR", type = "topNList",
  n=c(1,3,5,10,15,20))
results


###################################################
### code chunk number 36: recommenderlab.Rnw:1458-1459
###################################################
getConfusionMatrix(results)[[1]]


###################################################
### code chunk number 37: recommenderlab.Rnw:1471-1472
###################################################
avg(results)


###################################################
### code chunk number 38: roc1
###################################################
plot(results, annotate=TRUE)


###################################################
### code chunk number 39: precrec1
###################################################
plot(results, "prec/rec", annotate=TRUE)


###################################################
### code chunk number 40: recommenderlab.Rnw:1519-1535
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
### code chunk number 41: recommenderlab.Rnw:1540-1541
###################################################
results


###################################################
### code chunk number 42: recommenderlab.Rnw:1547-1549
###################################################
names(results)
results[["user-based CF"]]


###################################################
### code chunk number 43: roc2
###################################################
plot(results, annotate=c(1,3), legend="bottomright")


###################################################
### code chunk number 44: precrec2
###################################################
plot(results, "prec/rec", annotate=3, legend="topleft")


###################################################
### code chunk number 45: recommenderlab.Rnw:1592-1594
###################################################
## run algorithms
results <- evaluate(scheme, algorithms, type = "ratings")


###################################################
### code chunk number 46: recommenderlab.Rnw:1599-1600
###################################################
results


###################################################
### code chunk number 47: real
###################################################
plot(results, ylim = c(0,100))


###################################################
### code chunk number 48: recommenderlab.Rnw:1622-1628
###################################################
Jester_binary <- binarize(Jester5k, minRating=5)
Jester_binary <- Jester_binary[rowCounts(Jester_binary)>20]
Jester_binary
scheme_binary <- evaluationScheme(Jester_binary[1:1000],
	method="split", train=.9, k=1, given=3)
scheme_binary


###################################################
### code chunk number 49: recommenderlab.Rnw:1631-1633
###################################################
results_binary <- evaluate(scheme_binary, algorithms,
  type = "topNList", n=c(1,3,5,10,15,20))


###################################################
### code chunk number 50: roc3
###################################################
plot(results_binary, annotate=c(1,3), legend="topright")


