### R code from vignette source 'recommenderlab.Rnw'

###################################################
### code chunk number 1: recommenderlab.Rnw:88-91
###################################################
options(scipen=3, digits=4, prompt="R> ", eps=FALSE, width=75)
### for sampling
set.seed(1234)


###################################################
### code chunk number 2: recommenderlab.Rnw:1088-1089
###################################################
library("recommenderlab")


###################################################
### code chunk number 3: recommenderlab.Rnw:1097-1102
###################################################
m <- matrix(sample(c(as.numeric(0:5), NA), 50,
    replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
    dimnames=list(user=paste("u", 1:5, sep=''),
	item=paste("i", 1:10, sep='')))
m


###################################################
### code chunk number 4: recommenderlab.Rnw:1110-1113
###################################################
r <- as(m, "realRatingMatrix")
r
getRatingMatrix(r)


###################################################
### code chunk number 5: recommenderlab.Rnw:1118-1119
###################################################
identical(as(r, "matrix"),m)


###################################################
### code chunk number 6: recommenderlab.Rnw:1126-1128
###################################################
as(r, "list")
head(as(r, "data.frame"))


###################################################
### code chunk number 7: recommenderlab.Rnw:1143-1146
###################################################
r_m <- normalize(r)
r_m
getRatingMatrix(r_m)


###################################################
### code chunk number 8: recommenderlab.Rnw:1150-1151
###################################################
denormalize(r_m)


###################################################
### code chunk number 9: recommenderlab.Rnw:1157-1159 (eval = FALSE)
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
### code chunk number 12: recommenderlab.Rnw:1192-1195
###################################################
r_b <- binarize(r, minRating=4)
r_b
as(r_b, "matrix")


###################################################
### code chunk number 13: recommenderlab.Rnw:1207-1209
###################################################
data(Jester5k)
Jester5k


###################################################
### code chunk number 14: recommenderlab.Rnw:1218-1221
###################################################
set.seed(1234)
r <- sample(Jester5k, 1000)
r


###################################################
### code chunk number 15: recommenderlab.Rnw:1228-1231
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
### code chunk number 21: recommenderlab.Rnw:1320-1321
###################################################
recommenderRegistry$get_entries(dataType = "realRatingMatrix")


###################################################
### code chunk number 22: recommenderlab.Rnw:1329-1331
###################################################
r <- Recommender(Jester5k[1:1000], method = "POPULAR")
r


###################################################
### code chunk number 23: recommenderlab.Rnw:1335-1337
###################################################
names(getModel(r))
getModel(r)$topN


###################################################
### code chunk number 24: recommenderlab.Rnw:1352-1354
###################################################
recom <- predict(r, Jester5k[1001:1002], n=5)
recom


###################################################
### code chunk number 25: recommenderlab.Rnw:1359-1360
###################################################
as(recom, "list")


###################################################
### code chunk number 26: recommenderlab.Rnw:1366-1369
###################################################
recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")


###################################################
### code chunk number 27: recommenderlab.Rnw:1377-1380
###################################################
recom <- predict(r, Jester5k[1001:1002], type="ratings")
recom
as(recom, "matrix")[,1:10]


###################################################
### code chunk number 28: recommenderlab.Rnw:1391-1394
###################################################
recom <- predict(r, Jester5k[1001:1002], type="ratingMatrix")
recom
as(recom, "matrix")[,1:10]


###################################################
### code chunk number 29: recommenderlab.Rnw:1409-1412
###################################################
e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9,
    given=15, goodRating=5)
e


###################################################
### code chunk number 30: recommenderlab.Rnw:1418-1423
###################################################
r1 <- Recommender(getData(e, "train"), "UBCF")
r1

r2 <- Recommender(getData(e, "train"), "IBCF")
r2


###################################################
### code chunk number 31: recommenderlab.Rnw:1430-1434
###################################################
p1 <- predict(r1, getData(e, "known"), type="ratings")
p1
p2 <- predict(r2, getData(e, "known"), type="ratings")
p2


###################################################
### code chunk number 32: recommenderlab.Rnw:1440-1445
###################################################
error <- rbind(
  UBCF = calcPredictionAccuracy(p1, getData(e, "unknown")),
  IBCF = calcPredictionAccuracy(p2, getData(e, "unknown"))
)
error


###################################################
### code chunk number 33: recommenderlab.Rnw:1458-1461
###################################################
scheme <- evaluationScheme(Jester5k[1:1000], method="cross", k=4, given=3,
    goodRating=5)
scheme


###################################################
### code chunk number 34: recommenderlab.Rnw:1468-1471
###################################################
results <- evaluate(scheme, method="POPULAR", type = "topNList",
  n=c(1,3,5,10,15,20))
results


###################################################
### code chunk number 35: recommenderlab.Rnw:1482-1483
###################################################
getConfusionMatrix(results)[[1]]


###################################################
### code chunk number 36: recommenderlab.Rnw:1495-1496
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
### code chunk number 39: recommenderlab.Rnw:1543-1559
###################################################
set.seed(2016)
scheme <- evaluationScheme(Jester5k[1:1000], method="split", train = .9,
  given=-5, goodRating=5)
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
### code chunk number 40: recommenderlab.Rnw:1564-1565
###################################################
results


###################################################
### code chunk number 41: recommenderlab.Rnw:1571-1573
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
### code chunk number 44: recommenderlab.Rnw:1615-1617
###################################################
## run algorithms
results <- evaluate(scheme, algorithms, type = "ratings")


###################################################
### code chunk number 45: recommenderlab.Rnw:1622-1623
###################################################
results


###################################################
### code chunk number 46: real
###################################################
plot(results, ylim = c(0,100))


###################################################
### code chunk number 47: recommenderlab.Rnw:1645-1651
###################################################
Jester_binary <- binarize(Jester5k, minRating=5)
Jester_binary <- Jester_binary[rowCounts(Jester_binary)>20]
Jester_binary
scheme_binary <- evaluationScheme(Jester_binary[1:1000],
	method="split", train=.9, k=1, given=3)
scheme_binary


###################################################
### code chunk number 48: recommenderlab.Rnw:1654-1656
###################################################
results_binary <- evaluate(scheme_binary, algorithms,
  type = "topNList", n=c(1,3,5,10,15,20))


###################################################
### code chunk number 49: roc3
###################################################
plot(results_binary, annotate=c(1,3), legend="topright")


