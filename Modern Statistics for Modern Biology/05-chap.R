
    'FourDistances_a.png', 'FourDistances_b.png',
    'FourDistances_c.png', 'FourDistances_d.png')))

## -----------------------------------------------------------------------------
library("MASS")
library("RColorBrewer")
set.seed(101)
n <- 60000
S1=matrix(c(1,.72,.72,1), ncol=2)
S2=matrix(c(1.5,-0.6,-0.6,1.5),ncol=2)
mu1=c(.5,2.5)
mu2=c(6.5,4)
X1 = mvrnorm(n, mu=c(.5,2.5), Sigma=matrix(c(1,.72,.72,1), ncol=2))
X2 = mvrnorm(n,mu=c(6.5,4), Sigma=matrix(c(1.5,-0.6,-0.6,1.5),ncol=2))
# A color palette from blue to yellow to red
k = 11
my.cols <- rev(brewer.pal(k, "RdYlBu"))
plot(X1, xlim=c(-4,12),ylim=c(-2,9), xlab="Orange", ylab="Red", pch='.', cex=1)
points(X2, pch='.', cex=1)
# Draw the colored contour lines
# compute 2D kernel density, see MASS book, pp. 130-131
z1 = kde2d(X1[,1], X1[,2], n=50)
z2 = kde2d(X2[,1], X2[,2], n=50)
contour(z1, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE, lwd=2)
contour(z2, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE, lwd=2)
points(3.2,2,pch=20,cex=2.2,col="red")
lines(c(3.2,6.5),c(2,4),col="red",lwd=3)
lines(c(3.2,.5),c(2,2.5),col="red",lwd=3)

## -----------------------------------------------------------------------------
mx  = c(0, 0, 0, 1, 1, 1)
my  = c(1, 0, 1, 1, 0, 1)
mz  = c(1, 1, 1, 0, 1, 1)
mat = rbind(mx, my, mz)
dist(mat)
dist(mat, method = "binary")

## -----------------------------------------------------------------------------
load("../data/Morder.RData")
sqrt(sum((Morder[1, ] - Morder[2, ])^2))
as.matrix(dist(Morder))[2, 1]

## -----------------------------------------------------------------------------
mut = read.csv("../data/HIVmutations.csv")
mut[1:3, 10:16]

## -----------------------------------------------------------------------------
.o = options(digits = 3)
library("vegan")
mutJ = vegdist(mut, "jaccard")
mutC = sqrt(2 * (1 - cor(t(mut))))
mutJ
as.dist(mutC)
options(.o)

## -----------------------------------------------------------------------------
#
# This code is from Wolfgang. Should it be moved elsewhere, e.g. convert into an exercise?
# The code could be simplied using a loop
#
set.seed(248811)
Xmat = matrix(runif(100), ncol = 2)
nk = 3
cents = Xmat[sample(nrow(Xmat), nk, replace = FALSE), ]
# default distance: Euclidean
dist1 = function(vec){dist(rbind(vec, cents[1,]))}
dist2 = function(vec){dist(rbind(vec, cents[2,]))}
dist3 = function(vec){dist(rbind(vec, cents[3,]))}
dists123 = cbind(apply(Xmat, 1, dist1),
                 apply(Xmat, 1, dist2),
                 apply(Xmat, 1, dist3))
clust0 = apply(dists123, 1, which.min)
out1 = kmeans(Xmat, cents, iter.max=1)
out2 = kmeans(Xmat, cents, iter.max=3)
data0 = data.frame(x = Xmat[,1],
                   y = Xmat[,2],
                   cluster = as.factor(clust0))
data1 = data.frame(x = Xmat[,1],
                   y = Xmat[,2],
                   cluster = as.factor(out1$cluster))
data2 = data.frame(x = Xmat[,1],
                   y = Xmat[,2],
                   cluster = as.factor(out2$cluster))
library("ggplot2")
.mp = function(v, cdg) {
  ggplot(data = v, aes(x = x, y = y)) +
  geom_point(aes(col = cluster, shape = cluster), size = 5) + 
  geom_point(data = cdg, fill = "black", size = 7, shape = 1) +
  scale_shape_discrete(solid = TRUE, guide = "none") + 
  xlab("") + ylab("") + guides(col = "none") + coord_fixed()
}

# centers of clusters:
cdg = data.frame(x = cents[,1],y = cents[,2])
.mp(data0, cdg)

cents = out1$centers
cdg1 = data.frame(x=cents[,1],y=cents[,2])
.mp(data1, cdg1)

cents = out2$centers
cdg2 = data.frame(x=cents[,1],y=cents[,2])
.mp(data2, cdg2)

## -----------------------------------------------------------------------------
.oldMar = par("mar")
par(mar = c(1.1, 6, 4.1, 1.1))
library("clusterExperiment")
fluidigm = scRNAseq::ReprocessedFluidigmData()
se = fluidigm[, fluidigm$Coverage_Type == "High"]
assays(se) = list(normalized_counts = 
   round(limma::normalizeQuantiles(assay(se))))
ce = clusterMany(se, clusterFunction = "pam", ks = 5:10, run = TRUE,
  isCount = TRUE, reduceMethod = "var", nFilterDims = c(60, 100, 150))
clusterLabels(ce) = sub("FilterDims", "", clusterLabels(ce))
plotClusters(ce, whichClusters = "workflow", axisLine = -1)
par(mar = .oldMar)

## -----------------------------------------------------------------------------
stopifnot(length(clusterLabels(ce)) == 18)

## -----------------------------------------------------------------------------
library("flowCore")
library("flowViz")
fcsB = read.FCS("../data/Bendall_2011.fcs", truncate_max_range = FALSE)
slotNames(fcsB)

## -----------------------------------------------------------------------------
markersB = readr::read_csv("../data/Bendall_2011_markers.csv")
mt = match(markersB$isotope, colnames(fcsB))
stopifnot(!any(is.na(mt)))
colnames(fcsB)[mt] = markersB$marker

## -----------------------------------------------------------------------------
flowPlot(fcsB, plotParameters = colnames(fcsB)[2:3], logy = TRUE)

## -----------------------------------------------------------------------------
v1 = seq(0, 1, length.out = 100)
plot(log(v1), asinh(v1), type = 'l')
plot(v1, asinh(v1), type = 'l')
v3 = seq(30, 3000, length = 100)
plot(log(v3), asinh(v3), type= 'l')

## -----------------------------------------------------------------------------
asinhtrsf = arcsinhTransform(a = 0.1, b = 1)
fcsBT = transform(fcsB, transformList(colnames(fcsB)[-c(1, 2, 41)], asinhtrsf))
densityplot(~`CD3all`, fcsB)
densityplot(~`CD3all`, fcsBT)

## -----------------------------------------------------------------------------
kf = kmeansFilter("CD3all" = c("Pop1","Pop2"), filterId="myKmFilter")
fres = flowCore::filter(fcsBT, kf)
summary(fres)
fcsBT1 = flowCore::split(fcsBT, fres, population = "Pop1")
fcsBT2 = flowCore::split(fcsBT, fres, population = "Pop2")

## -----------------------------------------------------------------------------
library("flowPeaks")
fp = flowPeaks(Biobase::exprs(fcsBT)[, c("CD3all", "CD56")])
plot(fp)

## -----------------------------------------------------------------------------
flowPlot(fcsBT, plotParameters = c("CD3all", "CD56"), logy = FALSE)
contour(fcsBT[, c(40, 19)], add = TRUE)

## -----------------------------------------------------------------------------
library("ggcyto")
library("labeling")

p1 = ggcyto(fcsB, aes(x = CD4)) + geom_histogram(bins = 60)
p2 = ggcyto(fcsB, aes(x = CD8)) + geom_histogram(bins = 60)
p3 = ggcyto(fcsB, aes(x = CD4, y = CD8)) + geom_density2d(colour = "black")

fcsBT = transform(fcsB, transformList(colnames(fcsB)[-c(1, 2, 41)], 
                                      arcsinhTransform(a = 0, b = 1)))
                                      
p1t = ggcyto(fcsBT, aes(x = CD4))            + geom_histogram(bins = 90)
p2t = ggcyto(fcsBT, aes(x = CD4,y = CD8))    + geom_density2d(colour = "black")
p3t = ggcyto(fcsBT, aes(x = CD45RA,y = CD20))+ geom_density2d(colour = "black")

## -----------------------------------------------------------------------------
library("dbscan")
mc5 = Biobase::exprs(fcsBT)[, c(15,16,19,40,33)]
res5 = dbscan::dbscan(mc5, eps = 0.65, minPts = 30)
mc5df = data.frame(mc5, cluster = as.factor(res5$cluster))
table(mc5df$cluster)

## -----------------------------------------------------------------------------
ggplot(mc5df, aes(x=CD4,    y=CD8,  col=cluster))+geom_density2d()
ggplot(mc5df, aes(x=CD3all, y=CD20, col=cluster))+geom_density2d()

## -----------------------------------------------------------------------------
mc6 = Biobase::exprs(fcsBT)[, c(15, 16, 19, 33, 25, 40)]
res = dbscan::dbscan(mc6, eps = 0.65, minPts = 20)
mc6df = data.frame(mc6, cluster = as.factor(res$cluster))
table(mc6df$cluster)

## -----------------------------------------------------------------------------
mc7 = Biobase::exprs(fcsBT)[, c(11, 15, 16, 19, 25, 33, 40)]
res = dbscan::dbscan(mc7, eps = 0.95, minPts = 20)
mc7df = data.frame(mc7, cluster = as.factor(res$cluster))
table(mc7df$cluster)

## -----------------------------------------------------------------------------
## ####This is for documentation purposes, had to paste
## ####trees together par(mfrow)) not working for pheatmap
## library("pheatmap")
## load("../data/d14.RData")
## pheatmap(d14,clustering_distance_rows=d14,treeheight_col =200,
## cellwidth=20,cellheight=10,lwd=5,treeheight_row=0,clustering_method = "single",
## labels_col=1:11,main="single")
## pheatmap(d14,clustering_distance_rows=d14,treeheight_col =200,cellwidth=20,
## cellheight=10,lwd=5,treeheight_row=0,clustering_method = "average",
## labels_col=1:11,main="average")
## pheatmap(d14,clustering_distance_rows=d14,treeheight_col =200,cellwidth=20,
## cellheight=10,lwd=5,treeheight_row=0,clustering_method = "complete",labels_col=1:11,
## main="complete")

## -----------------------------------------------------------------------------
## ####### For the eecord: this is what was done to the data
## ####### Melanoma/Tcell Data: Peter Lee, Susan Holmes, PNAS.
## load("../data/Msig3transp.RData")
## celltypes=factor(substr(rownames(Msig3transp),7,9))
## status=factor(substr(rownames(Msig3transp),1,3))
## Msig2=as.matrix(Msig3transp)
## rownames(Msig2)=substr(rownames(Msig2),1,9)
## hm1=heatmap(as.matrix(dist(Msig2)))
## Morder=Msig2[hm1$rowInd,]
## save(Morder,file="../data/Morder.RData")
## write.table(Morder,"../data/Morder.txt")

## -----------------------------------------------------------------------------
## library("gplots")
## library("pheatmap")
## library("RColorBrewer")
## load("../data/Morder.RData")
## celltypes=factor(substr(rownames(Morder),7,9))
## status=factor(substr(rownames(Morder),1,3))
## ##Just the Euclidean distance
## pheatmap(as.matrix(dist(Morder)),cluster_rows=FALSE,
##         cluster_cols=FALSE,cellwidth=10,cellheight=10)
## ###Manhattan
## pheatmap(as.matrix(dist(Morder,"manhattan")),cluster_rows=FALSE,
##         cluster_cols=FALSE,cellwidth=10,cellheight=10)

## -----------------------------------------------------------------------------
## pheatmap(corT,clustering_distance_rows=distcor,
##        annotation_row=samplesdata[,c("celltypes","status")],
##        show_rownames = FALSE, show_colnames = FALSE)
## pheatmap(corT,clustering_distance_rows=distcor,treeheight_row =150,
##        annotation_row=samplesdata[,c("celltypes","status")],
##        show_rownames = FALSE, show_colnames = FALSE)
## pheatmap(corT,clustering_distance_rows=distcor,treeheight_row =150,
##        annotation_row=samplesdata[,c("celltypes","status")],
##        treeheight_col =150,
##        show_rownames = FALSE, show_colnames = FALSE)
## pheatmap(corT,clustering_distance_rows=distcor,treeheight_row =150,
##           annotation_col=samplesdata[,c("celltypes","status")],
##        annotation_row=samplesdata[,c("celltypes","status")],
##        treeheight_col =150,
##        show_rownames = FALSE, show_colnames = FALSE)

    'single14heatmap.png', 'average14heatmap.png', 'complete14heatmap.png')))

## -----------------------------------------------------------------------------
library("dplyr")
simdat = lapply(c(0, 8), function(mx) {
  lapply(c(0,8), function(my) {
    tibble(x = rnorm(100, mean = mx, sd = 2),
           y = rnorm(100, mean = my, sd = 2),
           class = paste(mx, my, sep = ":"))
   }) %>% bind_rows
}) %>% bind_rows
simdat
simdatxy = simdat[, c("x", "y")] # without class label

## -----------------------------------------------------------------------------
ggplot(simdat, aes(x = x, y = y, col = class)) + geom_point() +
  coord_fixed()

## -----------------------------------------------------------------------------
wss = tibble(k = 1:8, value = NA_real_)
wss$value[1] = sum(scale(simdatxy, scale = FALSE)^2)
for (i in 2:nrow(wss)) {
  km  = kmeans(simdatxy, centers = wss$k[i])
  wss$value[i] = sum(km$withinss)
}
ggplot(wss, aes(x = k, y = value)) + geom_col()

## -----------------------------------------------------------------------------
library("fpc")
library("cluster")
CH = tibble(
  k = 2:8,
  value = sapply(k, function(i) {
    p = pam(simdatxy, i)
    calinhara(simdatxy, p$cluster)
  })
)
ggplot(CH, aes(x = k, y = value)) + geom_line() + geom_point() +
  ylab("CH index")

## -----------------------------------------------------------------------------
library("cluster")
library("ggplot2")
pamfun = function(x, k)
  list(cluster = pam(x, k, cluster.only = TRUE))

gss = clusGap(simdatxy, FUN = pamfun, K.max = 8, B = 50,
              verbose = FALSE)
plot_gap = function(x) {
  gstab = data.frame(x$Tab, k = seq_len(nrow(x$Tab)))
  ggplot(gstab, aes(k, gap)) + geom_line() +
    geom_errorbar(aes(ymax = gap + SE.sim,
                      ymin = gap - SE.sim), width=0.1) +
    geom_point(size = 3, col=  "red")
}
plot_gap(gss)

## -----------------------------------------------------------------------------
library("Hiiragi2013")
data("x")

## -----------------------------------------------------------------------------
selFeats = order(rowVars(Biobase::exprs(x)), decreasing = TRUE)[1:50]
embmat = t(Biobase::exprs(x)[selFeats, ])
embgap = clusGap(embmat, FUN = pamfun, K.max = 24, verbose = FALSE)
k1 = maxSE(embgap$Tab[, "gap"], embgap$Tab[, "SE.sim"])
k2 = maxSE(embgap$Tab[, "gap"], embgap$Tab[, "SE.sim"],
           method = "Tibs2001SEmax")
c(k1, k2)

## -----------------------------------------------------------------------------
stopifnot("firstSEmax" == eval(formals(maxSE)$method)[1])

## -----------------------------------------------------------------------------
plot(embgap, main = "")
cl = pamfun(embmat, k = k1)$cluster
table(pData(x)[names(cl), "sampleGroup"], cl)

    'BootstrapClusterNew.png', 'BootstrapCluster2New.png')))

## -----------------------------------------------------------------------------
clusterResampling = function(x, ngenes = 50, k = 2, B = 250,
                             prob = 0.67) {
  mat = Biobase::exprs(x)
  ce = cl_ensemble(list = lapply(seq_len(B), function(b) {
    selSamps = sample(ncol(mat), size = round(prob * ncol(mat)),
                      replace = FALSE)
    submat = mat[, selSamps, drop = FALSE]
    sel = order(rowVars(submat), decreasing = TRUE)[seq_len(ngenes)]
    submat = submat[sel,, drop = FALSE]
    pamres = pam(t(submat), k = k)
    pred = cl_predict(pamres, t(mat[sel, ]), "memberships")
    as.cl_partition(pred)
  }))
  cons = cl_consensus(ce)
  ag = sapply(ce, cl_agreement, y = cons)
  list(agreements = ag, consensus = cons)
}

## -----------------------------------------------------------------------------
iswt = (x$genotype == "WT")
cr1 = clusterResampling(x[, x$Embryonic.day == "E3.25" & iswt])
cr2 = clusterResampling(x[, x$Embryonic.day == "E3.5"  & iswt])

## -----------------------------------------------------------------------------
ag1 = tibble(agreements = cr1$agreements, day = "E3.25")
ag2 = tibble(agreements = cr2$agreements, day = "E3.5")
p1 <- ggplot(bind_rows(ag1, ag2), aes(x = day, y = agreements)) +
  geom_boxplot() +
  ggbeeswarm::geom_beeswarm(cex = 1.5, col = "#0000ff40")
mem1 = tibble(y = sort(cl_membership(cr1$consensus)[, 1]),
              x = seq(along = y), day = "E3.25")
mem2 = tibble(y = sort(cl_membership(cr2$consensus)[, 1]),
              x = seq(along = y), day = "E3.5")
p2 <- ggplot(bind_rows(mem1, mem2), aes(x = x, y = y, col = day)) +
  geom_point() + facet_grid(~ day, scales = "free_x")
gridExtra::grid.arrange(p1, p2, widths = c(2.4,4.0))

## -----------------------------------------------------------------------------
library("mixtools")
seq1 = rmvnorm(n = 1e3, mu = -c(1, 1), sigma = 0.5 * diag(c(1, 1)))
seq2 = rmvnorm(n = 1e5, mu =  c(1, 1), sigma = 0.5 * diag(c(1, 1)))
twogr = data.frame(
  rbind(seq1, seq2),
  seq = factor(c(rep(1, nrow(seq1)),
                 rep(2, nrow(seq2))))
)
colnames(twogr)[1:2] = c("x", "y")
library("ggplot2")
ggplot(twogr, aes(x = x, y = y, colour = seq,fill = seq)) +
  geom_hex(alpha = 0.5, bins = 50) + coord_fixed()

## -----------------------------------------------------------------------------
n    = 2000
len  = 200
perr = 0.001
seqs = matrix(runif(n * len) >= perr, nrow = n, ncol = len)

## -----------------------------------------------------------------------------
dists = as.matrix(dist(seqs, method = "manhattan"))

## -----------------------------------------------------------------------------
library("tibble")
dfseqs = tibble(
  k = 10 ^ seq(log10(2), log10(n), length.out = 20),
  diameter = vapply(k, function(i) {
    s = sample(n, i)
    max(dists[s, s])
    }, numeric(1)))
ggplot(dfseqs, aes(x = k, y = diameter)) + geom_point()+geom_smooth()

## -----------------------------------------------------------------------------
simseq10K = replicate(1e5, sum(rpois(200, 0.0005)))
mean(simseq10K)
vcd::distplot(simseq10K, "poisson")

## -----------------------------------------------------------------------------
derepFs = readRDS(file="../data/derepFs.rds")
derepRs = readRDS(file="../data/derepRs.rds")
library("dada2")
ddF = dada(derepFs, err = NULL, selfConsist = TRUE)
ddR = dada(derepRs, err = NULL, selfConsist = TRUE)

## -----------------------------------------------------------------------------
plotErrors(ddF)

## -----------------------------------------------------------------------------
dadaFs = dada(derepFs, err=ddF[[1]]$err_out, pool = TRUE)
dadaRs = dada(derepRs, err=ddR[[1]]$err_out, pool = TRUE)

## -----------------------------------------------------------------------------
mergers = mergePairs(dadaFs, derepFs, dadaRs, derepRs)

## -----------------------------------------------------------------------------
seqtab.all = makeSequenceTable(mergers[!grepl("Mock",names(mergers))])

## -----------------------------------------------------------------------------
dadada = unique(vapply(dadaRs, class, character(1)))
stopifnot(is.list(dadaRs), identical("dada", dadada))

## -----------------------------------------------------------------------------
length(dadaRs)
length(dadaFs)
class(dadaRs)
names(dadaRs)
mergers = mergePairs(dadaFs, derepFs, dadaRs, derepRs)
class(mergers)
length(mergers)

## -----------------------------------------------------------------------------
seqtab = removeBimeraDenovo(seqtab.all)

## -----------------------------------------------------------------------------
library("cluster")
pam4 = pam(simdatxy, 4)
sil = silhouette(pam4, 4)
plot(sil, col=c("red","green","blue","purple"), main="Silhouette")

## -----------------------------------------------------------------------------
## jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
## paletteSize <- 256
## jBuPuPalette <- jBuPuFun(paletteSize)
## dd=as.matrix(dist.dune)
## prdune <- data.frame(sample = colnames(dd),
##                         probe = rownames(dd),
##                         dist = dd)
## ggplot(prdune, aes(x = probe, y = sample, fill = dist)) +
##   geom_tile() +
##   scale_fill_gradient2(low = jBuPuPalette[1],
##                        mid = jBuPuPalette[paletteSize/2],
##                        high = jBuPuPalette[paletteSize],
##                        midpoint = (max(prdune$dist) + min(prdune$dist)) / 2,
##                        name = "Distance")

## -----------------------------------------------------------------------------
## ## To Do: use pheatmap
## library("graphics")
## library("gplots")
## rc=heat.colors(21, alpha = 1)
## dr=round(as.matrix(dist.dune),1)
## heatmap.2(1-as.matrix(dist.dune),symm = TRUE, margins = c(3,3),Rowv = NA, Colv = NA,col=rc,
## distfun=function(c) as.dist(c), cellnote=dr,key=FALSE)

## -----------------------------------------------------------------------------
library("kernlab")
data("spirals")
clusts = kmeans(spirals,2)$cluster
plot(spirals, col = c("blue", "red")[clusts])
data("spirals", package = "kernlab")
res.dbscan = dbscan::dbscan(spirals, eps = 0.16, minPts = 3)
plot(spirals,col=c("blue","red","forestgreen")[res.dbscan$cluster])

## -----------------------------------------------------------------------------
stopifnot(identical(range(res.dbscan$cluster), c(1L, 3L)))

## -----------------------------------------------------------------------------
## sc = specc(spirals, centers=2)
## plot(spirals, col=sc)

## -----------------------------------------------------------------------------
base_dir = "../data"
miseq_path = file.path(base_dir, "MiSeq_SOP")
filt_path = file.path(miseq_path, "filtered")
fnFs = sort(list.files(miseq_path, pattern="_R1_001.fastq"))
fnRs = sort(list.files(miseq_path, pattern="_R2_001.fastq"))
sampleNames = sapply(strsplit(fnFs, "_"), `[`, 1)
if (!file_test("-d", filt_path)) dir.create(filt_path)
filtFs = file.path(filt_path, paste0(sampleNames, "_F_filt.fastq.gz"))
filtRs = file.path(filt_path, paste0(sampleNames, "_R_filt.fastq.gz"))
fnFs = file.path(miseq_path, fnFs)
fnRs = file.path(miseq_path, fnRs)
print(length(fnFs))

## -----------------------------------------------------------------------------
plotQualityProfile(fnFs[1:2]) + ggtitle("Forward")
plotQualityProfile(fnRs[1:2]) + ggtitle("Reverse")

## -----------------------------------------------------------------------------
ii = sample(length(fnFs), 4)
plotQualityProfile(fnFs[ii]) + ggtitle("Forward")
plotQualityProfile(fnRs[ii]) + ggtitle("Reverse")

## -----------------------------------------------------------------------------
out = filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=c(240,160),
        maxN=0, maxEE=2, truncQ=2, rm.phix=TRUE,  trimLeft=10,
        compress=TRUE, multithread=TRUE) # On Windows set multithread=FALSE
head(out)

## -----------------------------------------------------------------------------
derepFs = derepFastq(filtFs, verbose = FALSE)
derepRs = derepFastq(filtRs, verbose = FALSE)
names(derepFs) = sampleNames
names(derepRs) = sampleNames
