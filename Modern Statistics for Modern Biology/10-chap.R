
## -----------------------------------------------------------------------------
dats = read.table("../data/small_chemokine.txt", header = TRUE)
library("ggtree")
library("igraph")
library("tibble")
library("ggplot2")
library("reshape")
library("ggnetwork")
library("ggrepel")
library("dplyr")
gr = graph_from_data_frame(dats[,c("node1", "node2")], directed = FALSE)
E(gr)$weight = 1
V(gr)$size = centralization.degree(gr)$res
ggd = ggnetwork(gr, layout = layout_with_fr(gr), cell.jitter = 0)
ggplot(ggd, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black", curvature = 0.1, linewidth = 0.85, alpha = 1/2) +
  geom_nodes(aes(x = x, y = y), size = 4, alpha = 1/2, color = "orange") +
  geom_nodelabel_repel(aes(label = name), size = 4, color = "black") +
  theme_blank() + theme(legend.position = "none")

## -----------------------------------------------------------------------------
library("igraph")
edges = matrix(c(1,3, 2,3, 3,4, 4,5, 4,6), byrow = TRUE, ncol = 2)
g1 = graph_from_edgelist(edges, directed = FALSE)
vertex_attr(g1, name = "name") = 1:6
plot(g1, vertex.size = 25, edge.width = 5, vertex.color = "coral")

## -----------------------------------------------------------------------------
ggplotadjacency = function(a) {
  n = nrow(a)
  p = ncol(a)
  fromto  = reshape2::melt(a)
  stopifnot(identical(nrow(fromto), n*p))
  fromto$value = as.factor(fromto$value)
  cols = c("white", "darkblue")
  ggplot(data = fromto, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(colour = "black") +
    coord_fixed(ratio = 1, ylim = c(0.5, n + 0.5), xlim = c(0.5, p + 0.5)) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(name = "" , breaks = 1:p, labels = paste(1:p)) +
    scale_y_reverse(  name = "" , breaks = n:1, labels = paste(n:1)) + 
    theme_bw() +
    theme(axis.text = element_text(size = 14),
      legend.key = element_rect(fill = "white"),
      legend.background = element_rect(fill = "white"),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "white"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank() 
    )
}

## -----------------------------------------------------------------------------
ggplotadjacency(as_adj(g1, sparse = FALSE))

## -----------------------------------------------------------------------------
edges = "1,3\n2,3\n3,4\n4,6\n4,5"
df = read.csv(textConnection(edges), header = FALSE)
sg = graph_from_data_frame(df, directed = FALSE)
sg

## -----------------------------------------------------------------------------
finch = readr::read_csv("../data/finch.csv", 
                        comment = "#", col_types = "cc")
finch
library("network")
finch.nw  = as.network(finch, bipartite = TRUE, directed = FALSE)
is.island = nchar(network.vertex.names(finch.nw)) == 1
plot(finch.nw, vertex.cex = 2.5, displaylabels = TRUE, 
     vertex.col = ifelse(is.island, "forestgreen", "gold3"))
finch.nw |> as.matrix() |> t() |> (\(x) x[, order(colnames(x))])()

## -----------------------------------------------------------------------------
library("ggnetwork")
g1df = ggnetwork(g1)
ggplot(g1df, aes(x = x, y = y, xend = xend, yend = yend)) +
 geom_edges() + geom_nodes(size = 6,color = "#8856a7") +
 geom_nodetext(aes(label = name), size = 4, color = "white") +
 theme_blank() + theme(legend.position = "none")

## -----------------------------------------------------------------------------
library("markovchain")
statesNames = c("A", "C", "G","T")
T1MC = new("markovchain", states = statesNames, transitionMatrix =
  matrix(c(0.2,0.1,0.4,0.3,0,1,0,0,0.1,0.2,0.2,0.5,0.1,0.1,0.8,0.0),
         nrow = 4,byrow = TRUE, dimnames = list(statesNames, statesNames)))
plot(T1MC, edge.arrow.size = 0.4, vertex.color = "purple",
     edge.arrow.width = 2.2, edge.width = 5, edge.color = "blue",
     edge.curved = TRUE, edge.label.cex = 2.5, vertex.size= 32,
     vertex.label.cex = 3.5, edge.loop.angle = 3,
     vertex.label.family = "sans", vertex.label.color = "white")

## -----------------------------------------------------------------------------
library("ggnetwork")
oldpar = par(mar = c(5.1,4.7,4.1,2.6))
datf = read.table("../data/string_graph.txt", header = TRUE)
grs = graph_from_data_frame(datf[, c("node1", "node2")], directed = FALSE)
E(grs)$weight = 1
V(grs)$size = centralization.degree(grs)$res
ggdf = ggnetwork(grs, layout = layout_with_fr(grs), cell.jitter = 0)
ggplot(ggdf, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black", curvature = 0.1, linewidth = 0.95, alpha = 0.8)+
  geom_nodes(aes(x = x, y = y), size = 3, alpha = 0.5, color = "orange") +
  geom_nodelabel_repel(aes(label = name), size = 2.5, color = "#8856a7") +
  theme_blank() + theme(legend.position = "none")
par(oldpar)

## -----------------------------------------------------------------------------
## library("GSEABase")
## ## This requires a login to the website.
## fl   =  "/path/to/msigdb_v5.1.xml"
## gss  =  getBroadSets(fl)
## organism(gss[[1]])
## table(sapply(gss, organism))

## -----------------------------------------------------------------------------
d1 <- data.frame(Yellow = c(25, 500), Blue = c(25, 100), Red = c(25, 400))
row.names(d1) <- c('Significant', 'Universe')
knitr::kable(d1)

## -----------------------------------------------------------------------------
universe = c(rep("Yellow", 500), rep("Blue", 100), rep("Red", 400))
countblue = replicate(20000, {
  pick75 = sample(universe, 75, replace = FALSE)
  sum(pick75 == "Blue")
})
summary(countblue)

## -----------------------------------------------------------------------------
ggplot(data.frame(countblue), aes(x = countblue)) +
  geom_histogram(binwidth = 1, colour = "white", fill = "purple", center = 0.5, linewidth = 1)

## -----------------------------------------------------------------------------
stopifnot(all(countblue<22))

## -----------------------------------------------------------------------------
library("GOplot")
data("EC")
circ  =  circle_dat(EC$david, EC$genelist)
chord =  chord_dat(circ, EC$genes, EC$process)
GOChord(chord, limit = c(0, 5))

## -----------------------------------------------------------------------------
library("BioNet")
library("DLBCL")
data("dataLym")
data("interactome")
interactome
pval = dataLym$t.pval
names(pval)  =  dataLym$label
subnet = subNetwork(dataLym$label, interactome)
subnet = rmSelfLoops(subnet)
subnet

## -----------------------------------------------------------------------------
## Function to qqplot the output from fitBumModel
ggplotqqbu = function(x) {
  n = length(x$pvalues)
  probs = (rank(sort(x$pvalues)) - 0.5)/n
  quantiles = unlist(sapply(probs, uniroot, f = BioNet:::.pbum.solve,
        interval = c(0, 1), lambda = x$lambda, a = x$a)[1, ])
  df = data.frame(fitted = quantiles, observed = sort(x$pvalues))
  ggplot(df, aes(x = fitted, y = observed)) +
      xlab("Theoretical quantiles") + ylab("Observed quantiles") +
      geom_point(size = 0.3, alpha = 0.3, color = "red")+
      geom_segment(aes(x = 0, y = 0, xend = 1, yend= 1 ), color = "blue") +
      coord_fixed(ratio = 1)
}

hist1.bum = function(x, breaks = 50){
  n = length(x$pvalues)
  bumdata = seq(from = 0.006, to = 1, length=n)
  ys = x$lambda + (1 - x$lambda) * x$a * bumdata^(x$a -1)
  dat = data.frame(pvalues = x$pvalues, xxs = bumdata, y = ys)
  y0 = BioNet:::piUpper(x)
  ggplot(dat, aes(x = pvalues)) +
    geom_histogram(aes( y= after_stat(density)), binwidth = .02, fill = "orange", alpha = 0.75)+
    geom_hline(yintercept = y0, color = "blue3", alpha = 0.5, size = 1.2)+
    geom_line(aes(x = xxs, y = y), col = "red3", size = 1.3, alpha = 0.5)+
    xlab("p-values") +
    annotate("text", x = -0.03, y = y0 + 0.5, label = "pi[0]", parse = TRUE, size = 8)
}

## -----------------------------------------------------------------------------
## ## Original analysis as done by the authors using both p-values.
## library(BioNet)
## library(DLBCL)
## data("dataLym")
## data("interactome")
## interactome
## pvals  =  cbind(t = dataLym$t.pval, s = dataLym$s.pval)
## rownames(pvals)  =  dataLym$label
## pval  =  aggrPvals(pvals, order = 2, plot = FALSE)
## subnet  =  subNetwork(dataLym$label, interactome)
## subnet  =  rmSelfLoops(subnet)
## subnet

## -----------------------------------------------------------------------------
fb = fitBumModel(pval, plot = FALSE)
fb
scores=scoreNodes(subnet, fb, fdr = 0.001)

## -----------------------------------------------------------------------------
ggp=ggplotqqbu(fb)
print(ggp)

## -----------------------------------------------------------------------------
ggh=hist1.bum(fb)
print(ggh)

## -----------------------------------------------------------------------------
hotSub  =  runFastHeinz(subnet, scores)
hotSub
logFC=dataLym$diff
names(logFC)=dataLym$label

## -----------------------------------------------------------------------------
plotModule(hotSub, layout = layout.davidson.harel, scores = scores,
                  diff.expr = logFC)

## -----------------------------------------------------------------------------
knitr::kable(col.names = c("",""), align = "lr", data.frame(
  "$Q = \\begin{array}{lcccc} & A & T & C & G \\\\ 
  A & -3\\alpha & \\alpha & \\alpha & \\alpha \\\\ 
  T & \\alpha & -3\\alpha & \\alpha & \\alpha \\\\ 
  C & \\alpha & \\alpha & -3\\alpha & \\alpha \\\\ 
  G & \\alpha & \\alpha & \\alpha & -3\\alpha \\\\ \\end{array}$", 
  "$Q = \\begin{array}{lcccc} & A & T & C & G \\\\
  A & -\\alpha-2 \\beta & \\beta & \\beta & \\alpha \\\\ 
  T & \\beta & -\\alpha-2 \\beta & \\alpha & \\beta \\\\
  C & \\beta & \\alpha & -\\alpha-2 \\beta & \\beta \\\\
  G & \\alpha & \\beta & \\beta & -\\alpha-2 \\beta \\\\ \\end{array}$"))

## -----------------------------------------------------------------------------
library("phangorn")
library("ggtree")
load("../data/tree1.RData")

## -----------------------------------------------------------------------------
ggtree(tree1, lwd = 2, color = "darkgreen", alpha = 0.8, right = TRUE) +
  geom_tiplab(size = 7, angle = 90, offset = 0.05) +
  geom_point(aes(shape = isTip, color = isTip), size = 5, alpha = 0.6)

## -----------------------------------------------------------------------------
seqs6 = simSeq(tree1, l = 60, type = "DNA", bf = c(1, 1, 3, 3)/8, rate = 0.1)
seqs6
mat6df = data.frame(as.character(seqs6))
p = ggtree(tree1, lwd = 1.2) + geom_tiplab(aes(x = branch), size = 5, vjust = 2)
gheatmap(p, mat6df[, 1:60], offset = 0.01, colnames = FALSE)

## -----------------------------------------------------------------------------
plot(g1, vertex.size=20, edge.width=5, vertex.color="coral",
     vertex.shape=c("circle","square")[c(1,1,2,2,1,1)])

## -----------------------------------------------------------------------------
tree.nj = nj(dist.ml(seqs6, "JC69"))
ggtree(tree.nj) + geom_tiplab(size = 7) 

## -----------------------------------------------------------------------------
fit = pml(tree1, seqs6, k = 4)

## -----------------------------------------------------------------------------
library("dada2")
seqtab = readRDS("../data/seqtab.rds")
seqs = getSequences(seqtab)
names(seqs) = seqs

## -----------------------------------------------------------------------------
## fastaRef = "../tmp/rdp_train_set_16.fa.gz"
## taxtab = assignTaxonomy(seqtab, refFasta = fastaRef)

## -----------------------------------------------------------------------------
taxtab = readRDS(file= "../data/taxtab16.rds")
dim(taxtab)

## -----------------------------------------------------------------------------
head(taxtab) |> `rownames<-`(NULL)

## -----------------------------------------------------------------------------
readLines("../data/mal2.dna.txt") |> head(12) |> cat(sep="\n")

## -----------------------------------------------------------------------------
library("DECIPHER")
alignment = AlignSeqs(DNAStringSet(seqs), anchor = NA, verbose = FALSE)

## -----------------------------------------------------------------------------
phangAlign = phangorn::phyDat(as(alignment, "matrix"), type = "DNA")
dm = phangorn::dist.ml(phangAlign)
treeNJ = phangorn::NJ(dm)   # Note: tip order != sequence order
fit = phangorn::pml(treeNJ, data = phangAlign)
fitGTR = update(fit, k = 4, inv = 0.2)
fitGTR = phangorn::optim.pml(fitGTR, model = "GTR", optInv = TRUE,
         optGamma = TRUE,  rearrangement = "stochastic",
         control = phangorn::pml.control(trace = 0))

## -----------------------------------------------------------------------------
samples = read.csv(file.path("..", "data", "MIMARKS_Data_combined.csv"), header = TRUE)
samples$SampleID = paste0(gsub("00", "", samples$host_subject_id), 
                          "D", samples$age-21) 
samples = samples[!duplicated(samples$SampleID), ] 
stopifnot(all(rownames(seqtab) %in% samples$SampleID))
rownames(samples) = samples$SampleID 
keepCols = c("collection_date", "biome", "target_gene", "target_subfragment", 
  "host_common_name", "host_subject_id", "age", "sex", "body_product", "tot_mass",
  "diet", "family_relationship", "genotype", "SampleID") 
samples = samples[rownames(seqtab), keepCols] 

## -----------------------------------------------------------------------------
# For 'sex', samples_2 has oddly the values 'FALSE' 
samples_2 = read.csv(file.path("..", "data", "MIMARKS_Data_clean.csv"), header = TRUE)
for (k in setdiff(keepCols, "sex")) stopifnot(identical(samples[[k]], samples_2[[k]]))

## -----------------------------------------------------------------------------
library("phyloseq")
pso = phyloseq(tax_table(taxtab), 
               sample_data(samples),
               otu_table(seqtab, taxa_are_rows = FALSE), 
               phy_tree(fitGTR$tree))

## -----------------------------------------------------------------------------
prune_samples(rowSums(otu_table(pso)) > 5000, pso)

## -----------------------------------------------------------------------------
prevalence = apply(X = otu_table(pso),
                   MARGIN = ifelse(taxa_are_rows(pso), yes = 1, no = 2),
                   FUN = function(x) {sum(x > 0)})
prevdf = data.frame(Prevalence = prevalence,
                    TotalAbundance = taxa_sums(pso),
                    tax_table(pso))
tab = table(prevdf$Phylum)
keepPhyla = names(tab)[tab>5]
prevdf1   = subset(prevdf,   Phylum %in% keepPhyla)
ps2v      = subset_taxa(pso, Phylum %in% keepPhyla)

## -----------------------------------------------------------------------------
# warning: !expr c("DESeqDataSet.se, design = design, ignoreRank.: some variables in design formula are characters, converting to factors")
library("DESeq2")
ps1 = readRDS("../data/ps1.rds")
ps_dds = phyloseq_to_deseq2(ps1, design = ~ ageBin + family_relationship)
geometricmean = function(x)
   if (all(x == 0)) { 0 } else { exp(mean(log(x[x != 0]))) }
geoMeans = apply(counts(ps_dds), 1, geometricmean)
ps_dds = estimateSizeFactors(ps_dds, geoMeans = geoMeans)
ps_dds = estimateDispersions(ps_dds)
abund = getVarianceStabilizedData(ps_dds)

## -----------------------------------------------------------------------------
rownames(abund) = substr(rownames(abund), 1, 5) |> make.names(unique = TRUE)

## -----------------------------------------------------------------------------
library("structSSI")
el = phy_tree(ps1)$edge
el0 = el
el0 = el0[rev(seq_len(nrow(el))), ]
el_names = c(rownames(abund), seq_len(phy_tree(ps1)$Nnode))
el[, 1] = el_names[el0[, 1]]
el[, 2] = el_names[el0[, 2]]
unadj_p = treePValues(el, abund, sample_data(ps1)$ageBin)

## -----------------------------------------------------------------------------
hfdr_res = hFDR.adjust(unadj_p, el, 0.75)
summary(hfdr_res)
#plot(hfdr_res, height = 5000) # not run: opens in a browser

## -----------------------------------------------------------------------------
library("dplyr")
options(digits = 3)
tax = tax_table(ps1)[, c("Family", "Genus")] |> data.frame()
tax$seq = rownames(abund)
hfdr_res@p.vals$seq = rownames(hfdr_res@p.vals)
left_join(tax, hfdr_res@p.vals[,-3]) |>
  arrange(adjp) |> head(9) |> dplyr::select(1,2,4,5)

## -----------------------------------------------------------------------------
library("igraph")
library("ggnetwork")
library("ggplot2")
pts = structure(c(0, 0, 1, 1, 1.5, 2, 0, 1, 1, 0, 0.5, 0.5),
                .Dim = c(6L, 2L))
matxy = pts
distxy = stats::dist(matxy)
g = graph.adjacency(as.matrix(distxy), weighted = TRUE)
mst1 = igraph::mst(g)

## -----------------------------------------------------------------------------
gred = igraph::make_ring(6) - igraph::edges(6)
ggred = ggnetwork(gred, arrow.gap = 0, layout = matxy)
ggplot(ggred, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(size = 1.5, color = "red", alpha = 0.8) +
  geom_nodes(size = 6) + theme_blank()
ggmst1 = ggnetwork(mst1, arrow.gap = 0, layout = matxy)
ggplot(ggmst1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(size = 1.5, color = "steelblue", alpha = 0.8) +
  geom_nodes(size = 6) + theme_blank()

## -----------------------------------------------------------------------------
load("../data/dist2009c.RData")
country09 = attr(dist2009c, "Label")
mstree2009 = ape::mst(dist2009c)
gr09 = graph.adjacency(mstree2009, mode = "undirected")
gg = ggnetwork(gr09, arrow.gap = 0, layout = layout_with_fr(gr09))
ggplot(gg, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black", alpha = 0.5, curvature = 0.1) +
  geom_nodes(aes(color = name), size = 2) + theme_blank() +
  geom_nodetext(aes(label = name), color = "black", size = 2.5) +
  guides(color = guide_legend(keyheight = 0.09, keywidth = 0.09,
    title = "Countries")) + 
  theme(legend.text = element_text(size = 7))

## -----------------------------------------------------------------------------
## # these options were previously (<=2022-01-07) also there but did not seem to make it better
## theme(
##     legend.position = c(0, 0.14),
##     legend.background = element_blank(),
##     plot.margin = unit(c(0, 1, 1, 6), "cm"))

## -----------------------------------------------------------------------------
ggplot(gg, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "black", alpha = 0.5, curvature = 0.1) +
  geom_nodes(aes(color = name), size = 2) + theme_blank() +
  geom_nodetext_repel(aes(label = name), color = "black", 
                      size = 2, max.overlaps = 18) +
  theme_blank() +
  guides(color = guide_legend(keyheight = 0.09, keywidth = 0.09,
         title = "Countries")) + 
  theme(legend.text = element_text(size = 7))

## -----------------------------------------------------------------------------
library("rworldmap")
mat = match(country09, countriesLow$NAME)
coords2009 = data.frame(
  lat = countriesLow$LAT[mat],
  lon = countriesLow$LON[mat],
  country = country09)
layoutCoordinates = cbind(
  x = jitter(coords2009$lon, amount = 15),
  y = jitter(coords2009$lat, amount = 8))
labc = names(table(country09)[which(table(country09) > 1)])
matc = match(labc, countriesLow$NAME)
dfc = data.frame(
  latc = countriesLow$LAT[matc],
  lonc = countriesLow$LON[matc],
  labc)
dfctrans = dfc
dfctrans[, 1] = (dfc[,1] + 31) / 93
dfctrans[, 2] = (dfc[,2] + 105) / 238
ggeo09 = ggnetwork(gr09, arrow.gap = 0, layout = layoutCoordinates)
ggplot(ggeo09, aes(x = x, y = y)) +
  geom_nodes(aes(color = name), size = 2) +
  geom_edges(aes(xend = xend, yend = yend), color = "black", alpha = 0.5, curvature = 0.1) +
  geom_label(data = dfctrans, aes(x = lonc, y = latc, 
       label = labc, fill = labc), colour = "white", alpha = 0.5, size = 3) +
   theme(legend.position = "none") + theme_blank()

## -----------------------------------------------------------------------------
dfbr=data.frame(measure=c(rnorm(15,0.9),rnorm(15,1.8)),
  group=as.factor(c(rep("men",15),rep("women",15))))
ggplot(dfbr,aes(x=measure,group=group,y=0)) + ylim(-0.25,+0.25) +
  geom_point(aes(col=group,x=measure,y=0,shape=group),size=5,alpha=0.6)+
  scale_color_manual(values=c("blue","red"))+
  theme_bw() + geom_hline(yintercept = 0) +
  theme(panel.border = element_blank(),
  axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major.y = element_blank() ,
        panel.grid.minor.y = element_blank() )+ coord_fixed()

## -----------------------------------------------------------------------------
ps1  = readRDS("../data/ps1.rds")
sampledata = data.frame( sample_data(ps1))
d1 = as.matrix(phyloseq::distance(ps1, method="jaccard"))
gr = graph.adjacency(d1,  mode = "undirected", weighted = TRUE)
net = igraph::mst(gr)
V(net)$id = sampledata[names(V(net)), "host_subject_id"]
V(net)$litter = sampledata[names(V(net)), "family_relationship"]

## -----------------------------------------------------------------------------
gnet=ggnetwork(net)
ggplot(gnet, aes(x = x, y = y, xend = xend, yend = yend))+
  geom_edges(color = "darkgray") +
  geom_nodes(aes(color = id, shape = litter)) + theme_blank()+
  theme(legend.position="bottom")

## -----------------------------------------------------------------------------
library("phyloseqGraphTest")
gt = graph_perm_test(ps1, "host_subject_id", distance="jaccard",
                     type="mst",  nperm=1000)
gt$pval

## -----------------------------------------------------------------------------
plot_permutations(gt)

## -----------------------------------------------------------------------------
net = make_network(ps1, max.dist = 0.35)
sampledata = data.frame(sample_data(ps1))
V(net)$id = sampledata[names(V(net)), "host_subject_id"]
V(net)$litter = sampledata[names(V(net)), "family_relationship"]
netg = ggnetwork(net)

## -----------------------------------------------------------------------------
ggplot(netg, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "darkgray") +
  geom_nodes(aes(color = id, shape = litter)) + theme_blank()+
    theme(plot.margin = unit(c(0, 5, 2, 0), "cm"))+
    theme(legend.position = c(1.4, 0.3),legend.background = element_blank(),
          legend.margin=margin(0, 3, 0, 0, "cm"))+
         guides(color=guide_legend(ncol=2))

## -----------------------------------------------------------------------------
gt = graph_perm_test(ps1, "family_relationship",
        grouping = "host_subject_id",
        distance = "jaccard", type = "mst", nperm= 1000)
gt$pval

## -----------------------------------------------------------------------------
plot_permutations(gt)

## -----------------------------------------------------------------------------
gtnn1 = graph_perm_test(ps1, "family_relationship",
                      grouping = "host_subject_id",
                      distance = "jaccard", type = "knn", knn = 1)
gtnn1$pval

## -----------------------------------------------------------------------------
plot_test_network(gtnn1)

## -----------------------------------------------------------------------------
## NA

## -----------------------------------------------------------------------------
## NA

## -----------------------------------------------------------------------------
## library("markovchain")
## # Make Markov chain object
## mcPreg  =  new("markovchain", states = CSTs,
##               transitionMatrix = trans, name="PregCST")
## mcPreg
## # Set up igraph of the markov chain
## netMC  =  markovchain:::.getNet(mcPreg, round = TRUE)

## -----------------------------------------------------------------------------
## wts  =  E(netMC)$weight/100
## edgel  =  get.edgelist(netMC)
## elcat  =  paste(edgel[,1], edgel[,2])
## elrev  =  paste(edgel[,2], edgel[,1])
## edge.curved  =  sapply(elcat, function(x) x %in% elrev)
## samples_def  =  data.frame(sample_data(ps))
## samples_def  =  samples_def[samples$Preterm | samples$Term,] # Only those definitely assigned, i.e. not marginal
## premat  =  table(samples_def$CST, samples_def$Preterm)
## rownames(premat)  =  markovchain::states(mcPreg)
## colnames(premat)  =  c("Term", "Preterm")
## premat
## premat  =  premat/rowSums(premat)
## vert.CSTclrs  =  CSTColors

## -----------------------------------------------------------------------------
## default.par  =  par(no.readonly = TRUE)
## # Define color scale
## # Plotting function for markov chain
## plotMC  =  function(object, ...) {
##     netMC  =  markovchain:::.getNet(object, round = TRUE)
##     plot.igraph(x = netMC, ...)
## }
## # Color bar for the markov chain visualization, gradient in strength of preterm association
## color.bar  =  function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title=NULL) {
##     scale = (length(lut)-1)/(max-min)
##     cur.par = par(no.readonly = TRUE)
##     par(mar = c(0, 4, 1, 4) + 0.1, oma = c(0, 0, 0, 0) + 0.1)
##     par(ps = 10, cex = 0.8)
##     par(tcl=-0.2, cex.axis=0.8, cex.lab = 0.8)
##     plot(c(min,max), c(0,10), type='n', bty='n', xaxt='n', xlab=", yaxt='n', ylab=", main=title)
##     axis(1, c(0, 0.5, 1))
##     for (i in 1:(length(lut)-1)) {
##       x = (i-1)/scale + min
##       rect(x,0,x+1/scale,10, col=lut[i], border=NA)
##     }
## }
## 
## pal  =  colorRampPalette(c("grey50", "maroon", "magenta2"))(101)
## vert.clrs  =  sapply(states(mcPreg), function(x) pal[1+round(100*premat[x,"Preterm"])])
## vert.sz  =  4 + 2*sapply(states(mcPreg),
##               function(x) nrow(unique(sample_data(ps)[sample_data(ps)$CST==x,"SubjectID"])))
## vert.sz  =  vert.sz * 0.85
## vert.font.clrs  =  c("white", "white", "white", "white", "white")
## 
## # E(netMC) to see edge list, have to define loop angles individually by the # in edge list, not vertex
## edge.loop.angle = c(0, 0, 0, 0, 3.14, 3.14, 0, 0, 0, 0, 3.14, 0, 0, 0, 0, 0)-0.45
## layout  =  matrix(c(0.6,0.95, 0.43,1, 0.3,0.66, 0.55,0.3, 0.75,0.65), nrow = 5, ncol = 2, byrow = TRUE)
## 
## # Color by association with preterm birth
## layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), heights=c(1,10))
## color.bar(pal, min=0, max=1, nticks=6, title="Fraction preterm")
## par(mar=c(0,1,1,1)+0.1)
## edge.arrow.size=0.8
## edge.arrow.width=1.4
## edge.width = (15*wts + 0.1)*0.6
## edge.labels  =  as.character(E(netMC)$weight/100)
## edge.labels[edge.labels<0.4]  =  NA  # labels only for self-loops
## plotMC(mcPreg, edge.arrow.size=edge.arrow.size, edge.arrow.width = edge.arrow.width,
##        edge.width=edge.width, edge.curved=edge.curved,
##        vertex.color=vert.clrs, vertex.size=(vert.sz),
##        vertex.label.font = 2, vertex.label.cex = 1,
##        vertex.label.color = vert.font.clrs, vertex.frame.color = NA,
##        layout=layout, edge.loop.angle = edge.loop.angle)
## par(default.par)

## -----------------------------------------------------------------------------
dat = read.table("../data/ccnb1datsmall.txt", header = TRUE, comment.char = "", stringsAsFactors = TRUE)
v = levels(unlist(dat[,1:2]))        # vertex names
n = length(v)                        # number of vertices
e = matrix(match(as.character(unlist(dat[,1:2])), v),ncol=2) # edge list
w = dat$coexpression                 # edge weights

## -----------------------------------------------------------------------------
M = matrix(0, n, n)
M[e] = w
M = M + t(M)
dimnames(M) = list(v, v)
A = 1*(M > 0)

## -----------------------------------------------------------------------------
## library(igraph)
## net = network(e, directed=FALSE)
## par(mar=rep(0,4))
## plot(net, label=v)

## -----------------------------------------------------------------------------
breaks  =  c(0, seq(0.9, 1, length=11))
cols  =  grey(1-c(0,seq(0.5,1,length=10)))
ccnb1ind  =  which(v == "CCNB1")
vcols  =  rep("white",n)
vcols[ccnb1ind]  =  "blue"
vcols[which(M[,ccnb1ind]>0 | M[ccnb1ind,])]  =  "red"
par(mar = rep(0, 4))
heatmap(M, symm = TRUE, ColSideColors = vcols, RowSideColors = vcols,
        col = cols, breaks = breaks,  frame = TRUE)
legend("topleft", c("Neighbors(CCNB1)", "CCNB1"),
       fill = c("red","blue"),
       bty = "n", inset = 0, xpd = TRUE,  border = FALSE)

## -----------------------------------------------------------------------------
## library("ape")
## library("phangorn")
## GAG=read.dna("../data/DNA_GAG_20.txt")

## -----------------------------------------------------------------------------
gt = graph_perm_test(ps1, "family_relationship", distance = "bray", 
                     grouping = "host_subject_id", type = "knn", knn = 2)
gt$pval

## -----------------------------------------------------------------------------
plot_test_network(gt)
permdf = data.frame(perm=gt$perm)
obs = gt$observed
ymax = max(gt$perm)
ggplot(permdf, aes(x = perm)) + geom_histogram(bins = 20) +
  geom_segment(aes(x = obs, y = 0, xend = obs, yend = ymax/10), color = "red") +
  geom_point(aes(x = obs, y = ymax/10), color = "red") + xlab("Number of pure edges")
