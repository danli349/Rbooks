## ----initialize, echo = FALSE, message = FALSE, error = FALSE-----------------
source("../chapter-setup.R"); chaptersetup("/__w/CUBook/CUBook/Chap14-MultipleTest/multtest.Rnw", "14")
knitr::opts_chunk$set(dev = 'png', dpi = 100, fig.margin = TRUE, fig.show = 'hold', fig.keep = 'none')


## ---- xkcdmulttest-newspapertitle, eval = TRUE, echo = FALSE, fig.show = 'hold', fig.keep = 'high'----
knitr::include_graphics(c('images/xkcdmulttest-newspapertitle.png'), dpi = NA)


## ---- active-substance-discovery-robot-screening-robot, eval = TRUE, echo = FALSE, fig.show = 'hold', fig.keep = 'high', fig.cap = "High-throughput data in modern biology are screened for associations with millions of hypothesis tests. ([Source: Bayer](https://www.research.bayer.com/en/automated-search-for-active-ingredients-with-robots.aspx))"----
knitr::include_graphics(c('images/active-substance-discovery-robot-screening-robot.jpg'), dpi = NA)


## ----FDRsetup, message = FALSE, echo = FALSE----------------------------------
library("tibble")
library("dplyr")
library("ggplot2")
library("gganimate")
library("magick")
require("transformr")


## ----FDRcomputestuff, echo = FALSE--------------------------------------------
makedata = function(px, f1, f2, pi0, xcut, what) {
  stopifnot(length(px)==length(f1), 
            length(px)==length(f2), 
            length(pi0)==1, 
            length(xcut)==1,
            length(what)==1,
            pi0>=0, pi0<=1)
  f1 = f1 * pi0
  f2 = f2 * (1-pi0)
  i1 = which(px >= xcut)
  i2 = seq(1, i1[1], by = 1L)
  maxf1 = max(f1)
  maxf2 = max(f2)
  bind_rows(  
  tibble(
    x = px[c(i1, rev(i1))],
    y = c(f1[i1], rep(0, length(i1))),
    outcome = "True Negative"),
  tibble(
    x = px[c(i2, rev(i2))],
    y = c(f1[i2], rep(0, length(i2))),
    outcome = "False Positive"),
  tibble(
    x = px[c(i1, rev(i1))],
    y = c(f2[i1], rep(0, length(i1))) + maxf1,
    outcome = "False Negative"),
  tibble(
    x = px[c(i2, rev(i2))],
    y = c(f2[i2], rep(0, length(i2))) + maxf1,
    outcome = "True Positive"),
  tibble(
    x = rep(xcut, 3L),
    y = c(0, maxf1+maxf2, 0),
    outcome = ""
  )) |>
  bind_cols(tibble(xcut = xcut, what = what))
} 

findclosest = function(x, x0) {x[which.min(abs(x-x0))]}

pi0 = 2/3
t_df = 4
pxa = seq(-4, 4, length.out = 500)
pxb = pt(pxa, df = t_df)
xcuta = findclosest(pxa, qt(0.05, df = t_df))
xcutb = findclosest(pxb,    0.05)
f1a = dt(pxa, df = t_df) 
f2a = dgamma(pxa + 4, shape = 2, rate = 0.8) 

chainrulefac = (diff(pxa)/diff(pxb)) |> {\(x) c(x, last(x))}()
f1b = f1a * chainrulefac |> {\(x) x/sum(x)}()
f2b = f2a * chainrulefac |> {\(x) x/sum(x)}()
f1b = f1b/sum(f1b)
f2b = f2b/sum(f2b)

df = bind_rows(
  makedata(pxa, f1a, f2a, pi0, xcuta, "x"),
  makedata(pxb, f1b, f2b, pi0, xcutb, "p-value")
) 

make_static_plot = function(df) {
  stopifnot(nrow(df)>=3)
  colpal = setNames(
      c(RColorBrewer::brewer.pal(12, "Paired")[c(6,5,1,2)], "black"),
      c("True Positive", "False Negative",
        "False Positive", "True Negative", ""))
  ggplot(df, aes(x = x, y = y, fill = outcome, col = outcome)) + 
    geom_polygon() +
    scale_fill_manual(values = colpal) +
    scale_colour_manual(values = colpal) +
    xlab("value") +
    theme(legend.position = "bottom",
          legend.title = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.y  = element_blank(),
          axis.ticks.y = element_blank())
}


## ----mt-FDRvspstatic1, fig.keep = 'high', fig.cap = "Making a binary (yes/no) decision. Here, we call the two possible decisions \"positive\" and \"negative\" based on some continuous-valued score $x$, shown along the $x$-axis. The curve shaded in blue shows the distribution density of $x$ for one of the classes (the negatives), the curve shaded in red, for the other class (the positives). The distributions are distinctive (the red values are generally lower), but have some overlap. The vertical black bar marks some choice of a decision boundary, which results in four possible outcomes highlighted by the color key.", fig.width = 5.6, fig.height = 3, echo = FALSE----
make_static_plot(dplyr::filter(df, what == "x"))


## ----mt-FDRvspstatic2, fig.keep = 'high', fig.cap = "Analogous to Figure \\@ref(fig:mt-FDRvspstatic1), but now we have transformed $x$ from its original range to the range $[0,1]$ using a non-linear, strictly increasing transformation function $p=f(x)$, which we chose such that the resulting blue distribution is uniform. Such a function always exists: it is the cumulative distribution function of $x$ (we have seen it in Section \\@ref(rgraphics:sub:ecdf)). We call the result a **p-value**. The definition of the FDR in Equation \\@ref(eq:mt-simplefdr) applies equally well in Figure \\@ref(fig:mt-FDRvspstatic1) and here.", fig.width = 5.6, fig.height = 3, echo = FALSE----
make_static_plot(dplyr::filter(df, what == "p-value"))


## ----mt-FDRvspanim, fig.keep = 'high', fig.cap = "The animation highlights the analogies between using a generic score $x$ (as in Fig. \\@ref(fig:mt-FDRvspstatic1)) and a p-value from a formal hypothesis test (as in Fig. \\@ref(fig:mt-FDRvspstatic2)) for decision making. We will come back to these concepts in terms of the two-group model in Section \\@ref(sec:mt:localfdr) and Figure \\@ref(fig:mt-lfdr).", fig.width = 5.6, fig.height = 3, echo = FALSE, fig.show = "asis"----
p1 <- make_static_plot(df) + 
  labs(title = "{closest_state}") +
  transition_states(what,
                    transition_length = 3,
                    state_length = 1) + 
  view_follow() +
  ease_aes("cubic-in-out")
animate(p1, renderer = magick_renderer(), width = 5.6, height = 3, units = "in", res = 150, device = "png")


## ----whatprob1----------------------------------------------------------------
set.seed(0xdada)
numFlips = 100
probHead = 0.6
coinFlips = sample(c("H", "T"), size = numFlips,
  replace = TRUE, prob = c(probHead, 1 - probHead))
head(coinFlips)


## ----tableCoinFlips-----------------------------------------------------------
table(coinFlips)


## ----binomDens----------------------------------------------------------------
library("dplyr")
k = 0:numFlips
numHeads = sum(coinFlips == "H")
binomDensity = tibble(k = k,
     p = dbinom(k, size = numFlips, prob = 0.5))


## ----mt-dbinom, fig.keep = 'high', fig.cap = "The binomial distribution for the parameters $n=(ref:mt-dbinom-1)$ and $p=0.5$, according to Equation \\@ref(eq:mt-dbinom).", fig.width = 3.5, fig.height = 3----
library("ggplot2")
ggplot(binomDensity) +
  geom_bar(aes(x = k, y = p), stat = "identity") +
  geom_vline(xintercept = numHeads, col = "blue")


## ----rbinom, fig.keep = 'high', fig.cap = "An approximation of the binomial distribution from $(ref:rbinom-1)$ simulations (same parameters as Figure \\@ref(fig:mt-dbinom)).", fig.dim = c(3.5, 3)----
numSimulations = 10000
outcome = replicate(numSimulations, {
  coinFlips = sample(c("H", "T"), size = numFlips,
                     replace = TRUE, prob = c(0.5, 0.5))
  sum(coinFlips == "H")
})
ggplot(tibble(outcome)) + xlim(-0.5, 100.5) +
  geom_histogram(aes(x = outcome), binwidth = 1, center = 50) +
  geom_vline(xintercept = numHeads, col = "blue")


## ----mt-findrej, fig.keep = 'high', fig.cap = "As Figure \\@ref(fig:mt-dbinom), with rejection region (red) that has been chosen such that it contains the maximum number of bins whose total area is at most $\\alpha=(ref:mt-findrej-1)$.", fig.width=3.5, fig.height=3----
library("dplyr")
alpha = 0.05
binomDensity = arrange(binomDensity, p) |>
        mutate(reject = (cumsum(p) <= alpha))

ggplot(binomDensity) +
  geom_bar(aes(x = k, y = p, col = reject), stat = "identity") +
  scale_colour_manual(
    values = c(`TRUE` = "red", `FALSE` = "darkgrey")) +
  geom_vline(xintercept = numHeads, col = "blue") +
  theme(legend.position = "none")


## ----assertion, echo=FALSE----------------------------------------------------
stopifnot( .tmp1 + .tmp2 != numFlips )


## ----binom.test---------------------------------------------------------------
binom.test(x = numHeads, n = numFlips, p = 0.5)


## ---- typesoferror, echo = FALSE----------------------------------------------
dat <- data.frame(c('**Reject null hypothesis**', '**Do not reject**'),
                          c('Type I error (false positive)', 'True negative'),
                          c('True positive', 'Type II error (false negative)'))
            knitr::kable(dat, col.names = c('Test vs reality', 'Null hypothesis is true', '$...$ is false'), caption = 'Types of error in a statistical test.')


## ----checkbyexperimentalmaths, echo=FALSE, results="hide"---------------------
.myttest = function(x, y) {
  mx  = mean(x)
  my  = mean(y)
  s12 = sqrt((sum((x-mx)^2)+sum((y-my)^2)) / (length(x)+length(y)-2))
  (mx - my) / s12 * sqrt(length(x)*length(y)/(length(x)+length(y)))
}
replicate(100, {
  x = rnorm(ceiling(30 * runif(1)))
  y = rnorm(ceiling(30 * runif(1)))
  stopifnot(abs(.myttest(x, y) - t.test(x, y, var.equal=TRUE)$statistic) < 1e-9)
})


## ----mt-plantgrowth, fig.keep = 'high', fig.cap = "The `PlantGrowth` data.\\label{mt-plantgrowth}", fig.width = 3, fig.height = 2.75----
library("ggbeeswarm")
data("PlantGrowth")
ggplot(PlantGrowth, aes(y = weight, x = group, col = group)) +
  geom_beeswarm() + theme(legend.position = "none")
tt = with(PlantGrowth,
          t.test(weight[group =="ctrl"],
                 weight[group =="trt2"],
                 var.equal = TRUE))
tt


## ----mt-ttestperm, fig.keep = 'high', fig.cap = "The null distribution of the (absolute) $t$-statistic determined by simulations -- namely, by random permutations of the group labels.\\label{mt-ttestperm}", fig.width = 3, fig.height = 2.75----
abs_t_null = with(
  dplyr::filter(PlantGrowth, group %in% c("ctrl", "trt2")),
    replicate(10000,
      abs(t.test(weight ~ sample(group))$statistic)))

ggplot(tibble(`|t|` = abs_t_null), aes(x = `|t|`)) +
  geom_histogram(binwidth = 0.1, boundary = 0) +
  geom_vline(xintercept = abs(tt$statistic), col = "red")

mean(abs(tt$statistic) <= abs_t_null)


## ----tttestpermcheck, echo = FALSE--------------------------------------------
stopifnot(abs(mean(abs(tt$statistic) <= abs_t_null) -  tt$p.value) < 0.0025)


## ----ttdup--------------------------------------------------------------------
with(rbind(PlantGrowth, PlantGrowth),
       t.test(weight[group == "ctrl"],
              weight[group == "trt2"],
              var.equal = TRUE))


## ----prohHead_assertion, echo=FALSE-------------------------------------------
stopifnot(probHead!=0.5)


## ---- mterrors, echo = FALSE--------------------------------------------------
dat <- data.frame(c('**Rejected**', '**Not rejected**', '**Total**'),
                              c('$V$', '$U$', '$m_0$'),
                              c('$S$', '$T$','$m-m_0$'),
                              c('$R$', '$m-R$', '$m$'))
            knitr::kable(dat, col.names = c('Test vs reality', 'Null hypothesis is true', '$...$ is false', 'Total'), caption = 'Types of error in multiple testing. The letters designate the number of
    times each type of error occurs.')


## ----typeerror3---------------------------------------------------------------
1 - (1 - 1/1e6)^8e5


## ----mt-bonferroni, fig.keep = 'high', fig.cap = "Bonferroni method. The plot shows the graph of \\@ref(eq:mt-bonferroni) for $m=(ref:mt-bonferroni-1)$ as a function of $\\alpha$.", fig.width = 3, fig.height = 2.75----
m = 10000
ggplot(tibble(
  alpha = seq(0, 7e-6, length.out = 100),
  p     = 1 - (1 - alpha)^m),
  aes(x = alpha, y = p)) +  geom_line() +
  xlab(expression(alpha)) +
  ylab("Prob( no false rejection )") +
  geom_hline(yintercept = 0.05, col = "red")


## ----mtdeseq2airway, message=FALSE--------------------------------------------
library("DESeq2")
library("airway")
data("airway")
aw   = DESeqDataSet(se = airway, design = ~ cell + dex)
aw   = DESeq(aw)
awde = as.data.frame(results(aw)) |> dplyr::filter(!is.na(pvalue))


## ----mt-awpvhist, fig.keep = 'high', fig.cap = "p-value histogram of for the `airway` data.", fig.width = 3, fig.height = 2.75----
ggplot(awde, aes(x = pvalue)) +
  geom_histogram(binwidth = 0.025, boundary = 0)


## ----mt-awpvvisfdr, fig.keep = 'high', fig.cap = "Visual estimation of the FDR with the p-value histogram.", fig.width = 3, fig.height = 2.75----
alpha = binw = 0.025
pi0 = 2 * mean(awde$pvalue > 0.5)
ggplot(awde,
  aes(x = pvalue)) + geom_histogram(binwidth = binw, boundary = 0) +
  geom_hline(yintercept = pi0 * binw * nrow(awde), col = "blue") +
  geom_vline(xintercept = alpha, col = "red")


## ----fdrvis-------------------------------------------------------------------
pi0 * alpha / mean(awde$pvalue <= alpha)


## ----mt-BH, fig.keep = 'high', fig.cap = "Visualization of the Benjamini-Hochberg procedure. Shown is a zoom-in to the 7000 lowest p-values.\\label{mt-BH}", fig.width = 3, fig.height = 2.75----
phi  = 0.10
awde = mutate(awde, rank = rank(pvalue))
m    = nrow(awde)

ggplot(dplyr::filter(awde, rank <= 7000), aes(x = rank, y = pvalue)) +
  geom_line() + geom_abline(slope = phi / m, col = "red")


## ----kmax---------------------------------------------------------------------
kmax = with(arrange(awde, rank),
         last(which(pvalue <= phi * rank / m)))
kmax


## ----mt-SchwederSpjotvoll, fig.keep = 'high', fig.cap = "Schweder and Spj\\o{}tvoll plot, as described in the answer to Question \\@ref(ques:Testing-ques-SchwederSpjotvoll).", fig.dim = c(4.5, 4.5)----
awdef = awde %>%
  dplyr::filter(baseMean >=1) %>% 
  arrange(pvalue) %>%
  mutate(oneminusp = 1 - pvalue,
         N = n() - row_number())
jj = round(nrow(awdef) * c(1, 0.5))
slope = with(awdef, diff(N[jj]) / diff(oneminusp[jj]))
ggplot(awdef) +
  geom_point(aes(x = oneminusp, y = N), size = 0.15) + 
  xlab(expression(1-p[i])) +
  ylab(expression(N(p[i]))) +
  geom_abline(intercept = 0, slope = slope, col = "red3") +
  geom_hline(yintercept = slope, linetype = "dotted") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  geom_text(x = 0, y = slope, label = paste(round(slope)), 
            hjust = -0.1, vjust = -0.25) 


## ---- mt-sunexplode, fig.margin = FALSE, eval = TRUE, echo = FALSE, fig.show = 'hold', fig.keep = 'high', fig.cap = "From [http://xkcd.com/1132](http://xkcd.com/1132) -- While the frequentist only has the currently available data, the Bayesian can draw on her understanding of the world or on previous experience. As a Bayesian, she would know enough about physics to understand that our sun\'s mass is too small to become a nova. Even if she does not know physics, she might be an **empirical Bayesian** and draw her prior from a myriad previous days where the sun did not go nova."----
knitr::include_graphics(c('images/xkcd1132.png'), dpi = NA)


## ----mt-lfdr, fig.keep = 'high', fig.cap = "Local false discovery rate and the two-group model, with some choice of $f_{\\text{alt}}(p)$, and $\\pi_0=(ref:mt-lfdr-1)$. Top: densities. Bottom: distribution functions.", fig.width = 3, fig.height = 6, echo = FALSE----
pi0 = 0.6
f1 = function(t, shape2 = 7) {
   rv = dbeta(t, 1, shape2)
   rv / sum(rv) * (length(rv)-1) * (1-pi0)
}

t = seq(0, 1, length.out = 101)
t0 = 0.1

f0  = rep(pi0, length(t))
f   = f0 + f1(t)
F0  = cumsum(f0) / (length(t)-1)
F   = cumsum(f)  / (length(t)-1)
stopifnot(abs(F[length(F)] - 1) < 1e-2)

myplot = function(y, y0, ylim, yat, havepi0, colo = RColorBrewer::brewer.pal(12, "Paired")) {
  plot(x = t, y = y, type = "l", xlim = c(0, 1), ylim = ylim,
    xaxs = "i", yaxs = "i", ylab = "", yaxt = "n", xaxt = "n", xlab = "", main = deparse(substitute(y)))
  axis(side = 1, at = c(0, 1))
  axis(side = 2, at = yat)
  xa  =  t[t<=t0]
  xb  =  t[t>=t0]
  y0a = y0[t<=t0]
  y0b = y0[t>=t0]
  ya  =  y[t<=t0]
  polygon(x = c(xa, rev(xa)), y = c(y[t<=t0], rev(y0a)), col = colo[2])
  polygon(x = c(xb, rev(xb)), y = c(y[t>=t0], rev(y0b)), col = colo[1])
  polygon(x = c(xa, rev(xa)), y = c(rep(0, length(xa)), rev(y0a)), col = "#c0c0c0")
  polygon(x = c(xb, rev(xb)), y = c(rep(0, length(xb)), rev(y0b)), col = "#f0f0f0")
  segments(x0 = rep(t0, 2), x1 = rep(t0, 2), y0 = c(0, last(y0a)), y1 = c(last(y0a), last(ya)),
           col = colo[5:6], lwd = 3)
  text(t0, 0, adj = c(0, 1.8), labels = expression(p), cex = 1, xpd = NA)
  if (havepi0)
      text(0, pi0, adj = c(1.5, 0.5), labels = expression(pi[0]), cex = 1, xpd = NA)
}

par(mai = c(1, 0.6, 0.4, 0.3), mfcol = c(2,1))
myplot(f, f0, ylim = c(0, f[1]), yat = c(0:3),       havepi0 = TRUE)
myplot(F, F0, ylim = c(0, 1),    yat = c(0, 0.5, 1), havepi0 = FALSE)


## ----fdrtool, fig.width = 4, fig.height = 8, message = FALSE, results = "hide"----
library("fdrtool")
ft = fdrtool(awde$pvalue, statistic = "pvalue")


## ----qvalue31-----------------------------------------------------------------
ft$param[,"eta0"]


## ----awde_basemean_counts-----------------------------------------------------
awde$baseMean[1]
cts = counts(aw, normalized = TRUE)[1, ]
cts
mean(cts)


## ----makesure, echo=FALSE-----------------------------------------------------
stopifnot(abs(mean(cts)-awde$baseMean[1])<1e-9)


## ----mt-basemean-hist, fig.keep = 'high', fig.cap = "Histogram of `baseMean`. We see that it covers a large dynamic range, from close to 0 to around (ref:mt-basemean-hist-1). \\label{mt-basemean-hist}", fig.width = 3, fig.height = 2.4----
ggplot(awde, aes(x = asinh(baseMean))) +
  geom_histogram(bins = 60)


## ----mt-basemean-scp, fig.keep = 'high', fig.cap = "Scatterplot of the rank of `baseMean` versus the negative logarithm of the p-value. For small values of `baseMean`, no small p-values occur. Only for genes whose read counts across all observations have a certain size, the test for differential expression has power to come out with a small p-value. \\label{mt-basemean-scp}", fig.width = 3, fig.height = 2.4----
ggplot(awde, aes(x = rank(baseMean), y = -log10(pvalue))) +
  geom_hex(bins = 60) +
  theme(legend.position = "none")


## ----awde_stratify------------------------------------------------------------
awde = mutate(awde, stratum = cut(baseMean, include.lowest = TRUE,
  breaks = signif(quantile(baseMean,probs=seq(0,1,length.out=7)),2)))


## ----mt-awde-stratified-hist, fig.keep = 'high', fig.cap = "p-value histograms of the airway data, stratified into equally sized groups defined by increasing value of `baseMean`. \\label{mt-awde-stratified-hist}", fig.width = 2.8, fig.height = 4----
ggplot(awde, aes(x = pvalue)) + facet_wrap( ~ stratum, nrow = 4) +
  geom_histogram(binwidth = 0.025, boundary = 0)


## ----mt-awde-stratified-ecdf, fig.keep = 'high', fig.cap = "Same data as in Figure \\@ref(fig:mt-awde-stratified-hist), shown with ECDFs.\\label{mt-awde-stratified-ecdf}", fig.width = 4, fig.height = 3.4----
ggplot(awde, aes(x = pvalue, col = stratum)) +
  stat_ecdf(geom = "step") + theme(legend.position = "bottom")


## ----ihw_do-------------------------------------------------------------------
library("IHW")
ihw_res = ihw(awde$pvalue, awde$baseMean, alpha = 0.1)
rejections(ihw_res)


## ----ihwcompare---------------------------------------------------------------
padj_BH = p.adjust(awde$pvalue, method = "BH")
sum(padj_BH < 0.1)


## ----mt-ihwplot, fig.keep = 'high', fig.cap = "Hypothesis weights determined by the `ihw` function. Here the function\'s default settings chose (ref:mt-ihwplot-1) strata, while in our manual exploration above (Figures \\@ref(fig:mt-awde-stratified-hist), \\@ref(fig:mt-awde-stratified-ecdf)) we had used (ref:mt-ihwplot-2); in practice, this is a minor detail.", fig.width = 3.5, fig.height = 2----
plot(ihw_res)








