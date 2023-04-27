
## -----------------------------------------------------------------------------
load("../data/e100.RData")
e99 = e100[-which.max(e100)]

## -----------------------------------------------------------------------------
barplot(table(e99), space = 0.8, col = "chartreuse4")

## -----------------------------------------------------------------------------
library("vcd")
gf1 = goodfit( e99, "poisson")
rootogram(gf1, xlab = "", rect_gp = gpar(fill = "chartreuse4"))

## -----------------------------------------------------------------------------
simp = rpois(100, lambda = 0.05)
gf2 = goodfit(simp, "poisson")
rootogram(gf2, xlab = "")

## -----------------------------------------------------------------------------
table(e100)

## -----------------------------------------------------------------------------
table(rpois(100, 3))

## -----------------------------------------------------------------------------
counts  =  table(e100)
stopifnot(identical(names(counts), c("0", "1", "2", "7")), all(counts==c(58, 34, 7, 1)))

## -----------------------------------------------------------------------------
prod(dpois(c(0, 1, 2, 7), lambda = 3) ^ (c(58, 34, 7, 1)))

## -----------------------------------------------------------------------------
prod(dpois(c(0, 1, 2, 7), lambda = 0.4) ^ (c(58, 34, 7, 1)))

## -----------------------------------------------------------------------------
loglikelihood  =  function(lambda, data = e100) {
  sum(log(dpois(data, lambda)))
}

## -----------------------------------------------------------------------------
lambdas = seq(0.05, 0.95, length = 100)
loglik = vapply(lambdas, loglikelihood, numeric(1))
plot(lambdas, loglik, type = "l", col = "red", ylab = "", lwd = 2,
     xlab = expression(lambda))
m0 = mean(e100)
abline(v = m0, col = "blue", lwd = 2)
abline(h = loglikelihood(m0), col = "purple", lwd = 2)
m0

## -----------------------------------------------------------------------------
gf  =  goodfit(e100, "poisson")
names(gf)
gf$par

## -----------------------------------------------------------------------------
cb  =  c(rep(0, 110), rep(1, 10))

## -----------------------------------------------------------------------------
table(cb)

## -----------------------------------------------------------------------------
mean(cb)

## -----------------------------------------------------------------------------
probs  =  seq(0, 0.3, by = 0.005)
likelihood = dbinom(sum(cb), prob = probs, size = length(cb))
plot(probs, likelihood, pch = 16, xlab = "probability of success",
       ylab = "likelihood", cex=0.6)
probs[which.max(likelihood)]

## -----------------------------------------------------------------------------
stopifnot(abs(probs[which.max(likelihood)]-1/12) < diff(probs[1:2]))

## -----------------------------------------------------------------------------
loglikelihood = function(p, n = 300, y = 40) {
  log(choose(n, y)) + y * log(p) + (n - y) * log(1 - p)
}

## -----------------------------------------------------------------------------
p_seq = seq(0, 1, by = 0.001)
plot(p_seq, loglikelihood(p_seq), xlab = "p", ylab = "log f(p|y)", type = "l")

## -----------------------------------------------------------------------------
library("Biostrings")
staph = readDNAStringSet("../data/staphsequence.ffn.txt", "fasta")

## -----------------------------------------------------------------------------
staph[1]
letterFrequency(staph[[1]], letters = "ACGT", OR = 0)

## -----------------------------------------------------------------------------
letterFrq = vapply(staph, letterFrequency, FUN.VALUE = numeric(4),
         letters = "ACGT", OR = 0)
colnames(letterFrq) = paste0("gene", seq(along = staph))
tab10 = letterFrq[, 1:10]
computeProportions = function(x) { x/sum(x) }
prop10 = apply(tab10, 2, computeProportions)
round(prop10, digits = 2)
p0 = rowMeans(prop10)
p0

## -----------------------------------------------------------------------------
cs = colSums(tab10)
cs
expectedtab10 = outer(p0, cs, FUN = "*")
round(expectedtab10)

## -----------------------------------------------------------------------------
randomtab10 = sapply(cs, function(s) { rmultinom(1, s, p0) } )
all(colSums(randomtab10) == cs)

## -----------------------------------------------------------------------------
stopifnot(all(colSums(randomtab10) == cs))

## -----------------------------------------------------------------------------
stat = function(obsvd, exptd) {
   sum((obsvd - exptd)^2 / exptd)
}
B = 1000
simulstat = replicate(B, {
  randomtab10 = sapply(cs, function(s) { rmultinom(1, s, p0) })
  stat(randomtab10, expectedtab10)
})
S1 = stat(tab10, expectedtab10)
sum(simulstat >= S1)

hist(simulstat, col = "lavender", breaks = seq(0, 75, length.out=50))
abline(v = S1, col = "red")
abline(v = quantile(simulstat, probs = c(0.95, 0.99)),
       col = c("darkgreen", "blue"), lty = 2)

## -----------------------------------------------------------------------------
stopifnot(max(simulstat)<75, S1<75)

## -----------------------------------------------------------------------------
qs = ppoints(100)
quantile(simulstat, qs)
quantile(qchisq(qs, df = 30), qs)

## -----------------------------------------------------------------------------
qqplot(qchisq(ppoints(B), df = 30), simulstat, main = "",
  xlab = expression(chi[nu==30]^2), asp = 1, cex = 0.5, pch = 16)
abline(a = 0, b = 1, col = "red")

## -----------------------------------------------------------------------------
1 - pchisq(S1, df = 30)

## -----------------------------------------------------------------------------
load("../data/ChargaffTable.RData")
ChargaffTable

## -----------------------------------------------------------------------------
stopifnot(nrow(ChargaffTable) == 8)
mycolors = c("chocolate", "aquamarine4", "cadetblue4", "coral3",
            "chartreuse4","darkgoldenrod4","darkcyan","brown4")
par(mfrow=c(2, 4), mai = c(0, 0.7, 0.7, 0))
for (i in 1:8) {
  cbp = barplot(ChargaffTable[i, ], horiz = TRUE, axes = FALSE, axisnames = FALSE, col = mycolors[i])
  ax = axis(3, las = 2, labels = FALSE, col = mycolors[i], cex = 0.5, at = c(0, 10, 20))
  mtext(side = 3, at = ax,  text = paste(ax), col = mycolors[i], line = 0, las = 1, cex = 0.9)
  mtext(side = 2, at = cbp, text = colnames(ChargaffTable), col = mycolors[i], line = 0, las = 2, cex = 1)
  title(paste(rownames(ChargaffTable)[i]), col = mycolors[i], cex = 1.1)
}

## -----------------------------------------------------------------------------
statChf = function(x){
  sum((x[, "C"] - x[, "G"])^2 + (x[, "A"] - x[, "T"])^2)
}
chfstat = statChf(ChargaffTable)
permstat = replicate(100000, {
     permuted = t(apply(ChargaffTable, 1, sample))
     colnames(permuted) = colnames(ChargaffTable)
     statChf(permuted)
})
pChf = mean(permstat <= chfstat)
pChf
hist(permstat, breaks = 100, main = "", col = "lavender")
abline(v = chfstat, lwd = 2, col = "red")

## -----------------------------------------------------------------------------
HairEyeColor[,, "Female"]

## -----------------------------------------------------------------------------
str(HairEyeColor)
?HairEyeColor

## -----------------------------------------------------------------------------
load("../data/Deuteranopia.RData")
Deuteranopia

## -----------------------------------------------------------------------------
chisq.test(Deuteranopia)

## -----------------------------------------------------------------------------
library("HardyWeinberg")
data("Mourant")
Mourant[214:216,]
nMM = Mourant$MM[216]
nMN = Mourant$MN[216]
nNN = Mourant$NN[216]
loglik = function(p, q = 1 - p) {
  2 * nMM * log(p) + nMN * log(2*p*q) + 2 * nNN * log(q)
}
xv = seq(0.01, 0.99, by = 0.01)
yv = loglik(xv)
plot(x = xv, y = yv, type = "l", lwd = 2,
     xlab = "p", ylab = "log-likelihood")
imax = which.max(yv)
abline(v = xv[imax], h = yv[imax], lwd = 1.5, col = "blue")
abline(h = yv[imax], lwd = 1.5, col = "purple")

## -----------------------------------------------------------------------------
phat  =  af(c(nMM, nMN, nNN))
phat
pMM   =  phat^2
qhat  =  1 - phat

## -----------------------------------------------------------------------------
pHW = c(MM = phat^2, MN = 2*phat*qhat, NN = qhat^2)
sum(c(nMM, nMN, nNN)) * pHW

## -----------------------------------------------------------------------------
par(mai = rep(0.1, 4))
pops = c(1, 69, 128, 148, 192)
genotypeFrequencies = as.matrix(Mourant[, c("MM", "MN", "NN")])
HWTernaryPlot(genotypeFrequencies[pops, ],
        markerlab = Mourant$Country[pops],
        alpha = 0.0001, curvecols = c("red", rep("purple", 4)),
        mcex = 0.75, vertex.cex = 1)

## -----------------------------------------------------------------------------
HWTernaryPlot(genotypeFrequencies[pops, ],
              markerlab = Mourant$Country[pops],
              curvecols = c("red", rep("purple", 4)),
              alpha = 0.0001, mcex = 0.75, vertex.cex = 1)
HWTernaryPlot(genotypeFrequencies[-pops, ], 
              newframe = FALSE, alpha = 0.0001, cex = 0.5)

## -----------------------------------------------------------------------------
newgf = round(genotypeFrequencies / 50)
HWTernaryPlot(newgf[pops, ],
              markerlab = Mourant$Country[pops],
              curvecols = c("red", rep("purple", 4)),
              alpha = 0.0001, mcex = 0.75, vertex.cex = 1)

## -----------------------------------------------------------------------------
library("seqLogo")
load("../data/kozak.RData")
kozak
pwm = makePWM(kozak)
seqLogo(pwm, ic.scale = FALSE)

## -----------------------------------------------------------------------------
library("markovchain")
library("igraph")
sequence = toupper(c("a", "c", "a", "c", "g", "t", "t", "t", "t", "c", "c",
                     "a", "c", "g", "t", "a", "c","c","c","a","a","a","t","a",
                     "c","g","g","c","a","t","g","t","g","t","g","a","g","c","t","g"))
mcFit   =  markovchainFit(data = sequence)
MCgraph =  markovchain:::.getNet(mcFit$estimate, round = TRUE)
edgelab =  round(E(MCgraph)$weight / 100, 2)

## -----------------------------------------------------------------------------
par(mai=c(0,0,0,0))
plot.igraph(MCgraph, edge.label = edgelab,
       vertex.size = 40, xlim = c(-1, 1.25))

## -----------------------------------------------------------------------------
haplo6 = read.table("../data/haplotype6.txt", header = TRUE)
haplo6

## -----------------------------------------------------------------------------
with(haplo6, stopifnot(Individual[1] == "H1", DYS19[1] == 14, DXYS156Y[1] == 12))

## -----------------------------------------------------------------------------
dfbetas = data.frame(
  p = rep(p_seq, 3),
  dbeta = c(dbeta(p_seq,  10,  30),
            dbeta(p_seq,  20,  60), 
            dbeta(p_seq,  50, 150)),
  pars = rep(c("Beta(10,30)", "Beta(20,60)", "Beta(50,150)"), each = length(p_seq)))
library("ggplot2")
ggplot(dfbetas) +
  geom_line(aes(x = p, y = dbeta, colour = pars)) +
  theme(legend.title = element_blank()) +
  geom_vline(aes(xintercept = 0.25), colour = "#990000", linetype = "dashed")

## -----------------------------------------------------------------------------
rp = rbeta(100000, 50, 350)
y = vapply(rp, 
           function(x) rbinom(1, prob = x, size = 300), 
           integer(1))
hist(y, breaks = 50, col = "orange", main = "", xlab = "")

## -----------------------------------------------------------------------------
set.seed(0xbebe)
y1 = vapply(rp, 
            function(x) rbinom(1, prob = x, size = 300), 
            integer(1))
set.seed(0xbebe)
y2 = rbinom(length(rp), rp, size = 300)
stopifnot(identical(y1, y2))

## -----------------------------------------------------------------------------
pPostEmp = rp[ y == 40 ]
hist(pPostEmp, breaks = 40, col = "chartreuse4", main = "",
  probability = TRUE, xlab = "posterior p")

p_seq = seq(0, 1, by = 0.001)
densPostTheory = dbeta(p_seq, 50 + 40, 350 + 260)
lines(p_seq, densPostTheory, type = "l", lwd = 3)

## -----------------------------------------------------------------------------
mean(pPostEmp)
dp = p_seq[2] - p_seq[1]
sum(p_seq * densPostTheory * dp)

## -----------------------------------------------------------------------------
stopifnot(abs(mean(pPostEmp) - sum(p_seq * densPostTheory * dp)) < 1e-3)

## -----------------------------------------------------------------------------
pPostMC = rbeta(n = 100000, 90, 610)
mean(pPostMC)

## -----------------------------------------------------------------------------
qqplot(pPostMC, pPostEmp, type = "l", asp = 1)
abline(a = 0, b = 1, col = "blue")

## -----------------------------------------------------------------------------
densPost2 = dbeta(p_seq, 115, 735)
mcPost2   = rbeta(1e6, 115, 735)
sum(p_seq * densPost2 * dp)   # mean, by numeric integration
mean(mcPost2)                 # mean by MC
p_seq[which.max(densPost2)]   # MAP estimate

## -----------------------------------------------------------------------------
quantile(mcPost2, c(0.025, 0.975))

## -----------------------------------------------------------------------------
library("Biostrings")

## -----------------------------------------------------------------------------
## GENETIC_CODE
## IUPAC_CODE_MAP
## vignette(package = "Biostrings")
## vignette("BiostringsQuickOverview", package = "Biostrings")

## -----------------------------------------------------------------------------
GENETIC_CODE
IUPAC_CODE_MAP

## -----------------------------------------------------------------------------
library("BSgenome")
ag = available.genomes()
length(ag)
ag[1:2]

## -----------------------------------------------------------------------------
library("BSgenome.Ecoli.NCBI.20080805")
Ecoli
shineDalgarno = "AGGAGGT"
ecoli = Ecoli$NC_010473

## -----------------------------------------------------------------------------
window = 50000
starts = seq(1, length(ecoli) - window, by = window)
ends   = starts + window - 1
numMatches = vapply(seq_along(starts), function(i) {
  countPattern(shineDalgarno, ecoli[starts[i]:ends[i]],
               max.mismatch = 0)
  }, numeric(1))
table(numMatches)

## -----------------------------------------------------------------------------
library("vcd")
gf = goodfit(numMatches, "poisson")
summary(gf)
distplot(numMatches, type = "poisson")

## -----------------------------------------------------------------------------
sdMatches = matchPattern(shineDalgarno, ecoli, max.mismatch = 0)

## -----------------------------------------------------------------------------
betweenmotifs = gaps(sdMatches)

## -----------------------------------------------------------------------------
library("Renext")
expplot(width(betweenmotifs), rate = 1/mean(width(betweenmotifs)),
        labels = "fit")

## -----------------------------------------------------------------------------
## gofExp.test(width(betweenmotifs))

## -----------------------------------------------------------------------------
library("BSgenome.Hsapiens.UCSC.hg19")
chr8  =  Hsapiens$chr8
CpGtab = read.table("../data/model-based-cpg-islands-hg19.txt",
                    header = TRUE)
nrow(CpGtab)
head(CpGtab)
irCpG = with(dplyr::filter(CpGtab, chr == "chr8"),
         IRanges(start = start, end = end))

## -----------------------------------------------------------------------------
grCpG = GRanges(ranges = irCpG, seqnames = "chr8", strand = "+")
genome(grCpG) = "hg19"

## -----------------------------------------------------------------------------
library("Gviz")
ideo = IdeogramTrack(genome = "hg19", chromosome = "chr8")
plotTracks(
  list(GenomeAxisTrack(),
    AnnotationTrack(grCpG, name = "CpG"), ideo),
    from = 2200000, to = 5800000,
    shape = "box", fill = "#006400", stacking = "dense")

## -----------------------------------------------------------------------------
CGIview    = Views(unmasked(Hsapiens$chr8), irCpG)
NonCGIview = Views(unmasked(Hsapiens$chr8), gaps(irCpG))

## -----------------------------------------------------------------------------
seqCGI      = as(CGIview, "DNAStringSet")
seqNonCGI   = as(NonCGIview, "DNAStringSet")
dinucCpG    = sapply(seqCGI, dinucleotideFrequency)
dinucNonCpG = sapply(seqNonCGI, dinucleotideFrequency)
dinucNonCpG[, 1]
NonICounts = rowSums(dinucNonCpG)
IslCounts  = rowSums(dinucCpG)

## -----------------------------------------------------------------------------
TI  = matrix( IslCounts, ncol = 4, byrow = TRUE)
TnI = matrix(NonICounts, ncol = 4, byrow = TRUE)
dimnames(TI) = dimnames(TnI) =
  list(c("A", "C", "G", "T"), c("A", "C", "G", "T"))

## -----------------------------------------------------------------------------
MI = TI /rowSums(TI)
MI
MN = TnI / rowSums(TnI)
MN

## -----------------------------------------------------------------------------
freqIsl = alphabetFrequency(seqCGI, baseOnly = TRUE, collapse = TRUE)[1:4]
freqIsl / sum(freqIsl)
freqNon = alphabetFrequency(seqNonCGI, baseOnly = TRUE, collapse = TRUE)[1:4]
freqNon / sum(freqNon)

## -----------------------------------------------------------------------------
alpha = log((freqIsl/sum(freqIsl)) / (freqNon/sum(freqNon)))
beta  = log(MI / MN)

## -----------------------------------------------------------------------------
x = "ACGTTATACTACG"
scorefun = function(x) {
  s = unlist(strsplit(x, ""))
  score = alpha[s[1]]
  if (length(s) >= 2)
    for (j in 2:length(s))
      score = score + beta[s[j-1], s[j]]
  score
}
scorefun(x)

## -----------------------------------------------------------------------------
generateRandomScores = function(s, len = 100, B = 1000) {
  alphFreq = alphabetFrequency(s)
  isGoodSeq = rowSums(alphFreq[, 5:ncol(alphFreq)]) == 0
  s = s[isGoodSeq]
  slen = sapply(s, length)
  prob = pmax(slen - len, 0)
  prob = prob / sum(prob)
  idx  = sample(length(s), B, replace = TRUE, prob = prob)
  ssmp = s[idx]
  start = sapply(ssmp, function(x) sample(length(x) - len, 1))
  scores = sapply(seq_len(B), function(i)
    scorefun(as.character(ssmp[[i]][start[i]+(1:len)]))
  )
  scores / len
}
scoresCGI    = generateRandomScores(seqCGI)
scoresNonCGI = generateRandomScores(seqNonCGI)

## -----------------------------------------------------------------------------
rgs = range(c(scoresCGI, scoresNonCGI))
br = seq(rgs[1], rgs[2], length.out = 50)
h1 = hist(scoresCGI,    breaks = br, plot = FALSE)
h2 = hist(scoresNonCGI, breaks = br, plot = FALSE)
plot(h1, col = rgb(0, 0, 1, 1/4), xlim = c(-0.5, 0.5), ylim=c(0,120))
plot(h2, col = rgb(1, 0, 0, 1/4), add = TRUE)

## -----------------------------------------------------------------------------
## ###This is for provenance reasons, keep track of how the data
## ###were generated for the EM exercise in Chapter 4.
## Mdata=c(scoresCGI,scoresNonCGI)
## MM1=sample(Mdata[1:1000],800)
## MM2=sample(Mdata[1001:2000],1000)
## Myst=c(MM1,MM2);names(Myst)=NULL
## saveRDS(c(MM1,MM2),"../data/Myst.rds")
## ###True value of m1,m2,s1 and s2
## ###

## -----------------------------------------------------------------------------
stopifnot(max(h1$counts) < 120, max(h2$counts) < 120,
          h1$breaks[1] >= br[1], h1$breaks[length(h1$breaks)] <= br[length(br)],
          h2$breaks[1] >= br[1], h2$breaks[length(h2$breaks)] <= br[length(br)])

## -----------------------------------------------------------------------------
mtb = read.table("../data/M_tuberculosis.txt", header = TRUE)
head(mtb, n = 4)

## -----------------------------------------------------------------------------
pro  =  mtb[ mtb$AmAcid == "Pro", "Number"]
pro/sum(pro)

## -----------------------------------------------------------------------------
staph = readDNAStringSet("../data/staphsequence.ffn.txt", "fasta")

## -----------------------------------------------------------------------------
staph[1:3, ]
staph

## -----------------------------------------------------------------------------
letterFrequency(staph[[1]], letters = "ACGT", OR = 0)
GCstaph = data.frame(
  ID = names(staph),
  GC = rowSums(alphabetFrequency(staph)[, 2:3] / width(staph)) * 100
)

## -----------------------------------------------------------------------------
window = 100
gc = rowSums( letterFrequencyInSlidingView(staph[[364]], window,
      c("G","C")))/window
plot(x = seq(along = gc), y = gc, type = "l")

## -----------------------------------------------------------------------------
plot(x = seq(along = gc), y = gc, type = "l")
lines(lowess(x = seq(along = gc), y = gc, f = 0.2), col = 2)

## -----------------------------------------------------------------------------
dfbetas = data.frame(
  p = rep(p_seq, 5),
  dbeta = c(dbeta(p_seq, 0.5, 0.5), 
            dbeta(p_seq,   1,   1), 
            dbeta(p_seq,  10,  30),
            dbeta(p_seq,  20,  60), 
            dbeta(p_seq,  50, 150)),
  pars = rep(c("Beta(0.5,0.5)", "U(0,1)=Beta(1,1)", 
               "Beta(10,30)", "Beta(20,60)", 
               "Beta(50,150)"), each = length(p_seq)))
ggplot(dfbetas) +
  geom_line(aes(x = p, y = dbeta, colour = pars)) +
  theme(legend.title = element_blank()) +
  geom_vline(aes(xintercept = 0.25), colour = "#990000", linetype = "dashed")
