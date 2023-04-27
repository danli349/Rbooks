
## -----------------------------------------------------------------------------
dpois(x = 3, lambda = 5)

## -----------------------------------------------------------------------------
.oldopt = options(digits = 2)
0:12
dpois(x = 0:12, lambda = 5)
barplot(dpois(0:12, 5), names.arg = 0:12, col = "red")
options(.oldopt)

## -----------------------------------------------------------------------------
genotype = c("AA","AO","BB","AO","OO","AO","AA","BO","BO",
             "AO","BB","AO","BO","AB","OO","AB","BB","AO","AO")
table(genotype)

## -----------------------------------------------------------------------------
genotypeF = factor(genotype)
levels(genotypeF)
table(genotypeF)

## -----------------------------------------------------------------------------
rbinom(15, prob = 0.5, size = 1)

## -----------------------------------------------------------------------------
rbinom(12, prob = 2/3, size = 1)

## -----------------------------------------------------------------------------
rbinom(1, prob = 2/3, size = 12)

## -----------------------------------------------------------------------------
set.seed(235569515)
rbinom(1, prob = 0.3, size = 15)

## -----------------------------------------------------------------------------
probabilities = dbinom(0:15, prob = 0.3, size = 15)
round(probabilities, 2)

## -----------------------------------------------------------------------------
barplot(probabilities, names.arg = 0:15, col = "red")

## -----------------------------------------------------------------------------
## plot(dbinom(0:12, prob = 5e-4, size = 1e4),
##      dpois(0:12, lambda = 5), asp = 1)
## abline(a = 0, b = 1, col = "blue")

## -----------------------------------------------------------------------------
5^3 * exp(-5) / factorial(3)

## -----------------------------------------------------------------------------
rbinom(1, prob = 5e-4, size = 10000)
simulations = rbinom(n = 300000, prob = 5e-4, size = 10000)
barplot(table(simulations), col = "lavender")

## -----------------------------------------------------------------------------
`[<-`(rep(0, 100), 22, 1)

## -----------------------------------------------------------------------------
s100 = rpois(100, lambda=0.5)
barplot(s100, ylim = c(0, 7), width = 0.7, xlim = c(-0.5,100.5),
  names.arg = seq(along = s100), col="lavender")

## -----------------------------------------------------------------------------
## set.seed(8969311)
## e100 = rpois(100,lambda = 0.5)
## e100[42] = 7
## save(e100, file = "../data/e100.RData")

## -----------------------------------------------------------------------------
load("../data/e100.RData")
barplot(e100, ylim = c(0, 7), width = 0.7, xlim = c(-0.5, 100.5),
  names.arg = seq(along = e100), col = "darkolivegreen")

## -----------------------------------------------------------------------------
barplot(e100, ylim = c(0, 7), width = 0.7, xlim = c(-0.5, 100.5),
  names.arg = seq(along = e100), col = "darkolivegreen")
text(35, 7, adj = c(-0.05, 0.5), labels = "?", xpd = NA, col = "red",
  cex = 1.25, font = 2)

## -----------------------------------------------------------------------------
1 - ppois(6, 0.5)
ppois(6, 0.5, lower.tail = FALSE)

## -----------------------------------------------------------------------------
maxes = replicate(100000, {
  max(rpois(100, 0.5))
})
table(maxes)

## -----------------------------------------------------------------------------
mean( maxes >= 7 )

## -----------------------------------------------------------------------------
dmultinom(c(4, 2, 0, 0), prob = rep(1/4, 4))

## -----------------------------------------------------------------------------
pvec = rep(1/4, 4)
t(rmultinom(1, prob = pvec, size = 8))

## -----------------------------------------------------------------------------
obsunder0 = rmultinom(1000, prob = pvec, size = 20)
dim(obsunder0)
obsunder0[, 1:11]

## -----------------------------------------------------------------------------
thep = unique(pvec); stopifnot(length(thep)==1, thep == 0.25)

## -----------------------------------------------------------------------------
expected0 = pvec * 20
sum((obsunder0[, 1] - expected0)^2 / expected0)
sum((obsunder0[, 2] - expected0)^2 / expected0)
sum((obsunder0[, 3] - expected0)^2 / expected0)

## -----------------------------------------------------------------------------
stat = function(obsvd, exptd = 20 * pvec) {
  sum((obsvd - exptd)^2 / exptd)
}
stat(obsunder0[, 1])

## -----------------------------------------------------------------------------
S0 = apply(obsunder0, 2, stat)
summary(S0)
hist(S0, breaks = 25, col = "lavender", main = "")

## -----------------------------------------------------------------------------
q95 = quantile(S0, probs = 0.95)
q95

## -----------------------------------------------------------------------------
## ## This was done to save this object for its reuse in Chapter 2.
## save(S0, file = "../data/S0.RData")

## -----------------------------------------------------------------------------
pvecA = c(3/8, 1/4, 1/4, 1/8)
observed = rmultinom(1000, prob = pvecA, size = 20)
dim(observed)
observed[, 1:7]
apply(observed, 1, mean)
expectedA = pvecA * 20
expectedA

## -----------------------------------------------------------------------------
stat(observed[, 1])
S1 = apply(observed, 2, stat)
q95
sum(S1 > q95)
power = mean(S1 > q95)
power

## -----------------------------------------------------------------------------
stopifnot(stat(observed[, 1]) < q95)

## -----------------------------------------------------------------------------
dbinom(2, size = 10, prob = 0.3)
pbinom(2, size = 10, prob = 0.3)
sum(dbinom(0:2, size = 10, prob = 0.3)) 

## -----------------------------------------------------------------------------
poismax = function(lambda, n, m) {
  epsilon = 1 - ppois(m - 1, lambda)
  1 - exp( -n * epsilon)
}
poismax(lambda = 0.5, n = 100, m = 7)
poismax(lambda = mean(e100), n = 100, m = 7)

## -----------------------------------------------------------------------------
poismax = function(lambda, n = 100, m = 7) {
  1 - exp( -n * (1 - ppois(m - 1, lambda)))
}
poismax(0.5)
poismax(0.5, m = 9)

## -----------------------------------------------------------------------------
## if (!requireNamespace("BiocManager", quietly = TRUE))
##     install.packages("BiocManager")
## BiocManager::install(c("Biostrings", "BSgenome.Celegans.UCSC.ce2"))

## -----------------------------------------------------------------------------
library("BSgenome.Celegans.UCSC.ce2")
Celegans
seqnames(Celegans)
Celegans$chrM
class(Celegans$chrM)
length(Celegans$chrM)

## -----------------------------------------------------------------------------
library("Biostrings")
lfM = letterFrequency(Celegans$chrM, letters=c("A", "C", "G", "T"))
lfM
sum(lfM)
lfM / sum(lfM)

## -----------------------------------------------------------------------------
t(rmultinom(1, length(Celegans$chrM), p = rep(1/4, 4)))

## -----------------------------------------------------------------------------
length(Celegans$chrM) / 4

## -----------------------------------------------------------------------------
oestat = function(o, e) {
  sum((o-e)^2 / e)
}
oe = oestat(o = lfM, e = length(Celegans$chrM) / 4)
oe

## -----------------------------------------------------------------------------
B = 10000
n = length(Celegans$chrM)
expected = rep(n / 4, 4)
oenull = replicate(B,
  oestat(e = expected, o = rmultinom(1, n, p = rep(1/4, 4))))

## -----------------------------------------------------------------------------
## hist(oenull, breaks = 100, col = "skyblue", main = "")

## -----------------------------------------------------------------------------
stopifnot( oe/10 > max(oenull) )
