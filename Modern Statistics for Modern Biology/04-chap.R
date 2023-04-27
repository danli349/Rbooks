
## -----------------------------------------------------------------------------
coinflips = (runif(10000) > 0.5)
table(coinflips)
oneFlip = function(fl, mean1 = 1, mean2 = 3, sd1 = 0.5, sd2 = 0.5) {
  if (fl) {
   rnorm(1, mean1, sd1)
  } else {
   rnorm(1, mean2, sd2)
  }
}
fairmix = vapply(coinflips, oneFlip, numeric(1))
library("ggplot2")
library("dplyr")
ggplot(tibble(value = fairmix), aes(x = value)) +
     geom_histogram(fill = "purple", binwidth = 0.1)

## -----------------------------------------------------------------------------
means = c(1, 3)
sds   = c(0.5, 0.5)
values = rnorm(length(coinflips),
               mean = ifelse(coinflips, means[1], means[2]),
               sd   = ifelse(coinflips, sds[1],   sds[2]))

## -----------------------------------------------------------------------------
fair = tibble(
  coinflips = (runif(1e6) > 0.5),
  values = rnorm(length(coinflips),
                 mean = ifelse(coinflips, means[1], means[2]),
                 sd   = ifelse(coinflips, sds[1],   sds[2])))
ggplot(fair, aes(x = values)) +
     geom_histogram(fill = "purple", bins = 200)

## -----------------------------------------------------------------------------
ggplot(dplyr::filter(fair, coinflips), aes(x = values)) +
  geom_histogram(aes(y = after_stat(density)), fill = "purple", binwidth = 0.01) +
  stat_function(fun = dnorm, color = "red",
                args = list(mean = means[1], sd = sds[1]))

## -----------------------------------------------------------------------------
fairtheory = tibble(
  x = seq(-1, 5, length.out = 1000),
  f = 0.5 * dnorm(x, mean = means[1], sd = sds[1]) +
      0.5 * dnorm(x, mean = means[2], sd = sds[2]))
ggplot(fairtheory, aes(x = x, y = f)) +
  geom_line(color = "red", linewidth = 1.5) + ylab("mixture density")

## -----------------------------------------------------------------------------
mystery = tibble(
  coinflips = (runif(1e3) > 0.5),
  values = rnorm(length(coinflips),
                 mean = ifelse(coinflips, 1, 2),
                 sd   = ifelse(coinflips, sqrt(.5), sqrt(.5))))
br2 = with(mystery, seq(min(values), max(values), length.out = 30))
ggplot(mystery, aes(x = values)) +
geom_histogram(fill = "purple", breaks = br2)

## -----------------------------------------------------------------------------
head(mystery, 3)
br = with(mystery, seq(min(values), max(values), length.out = 30))
ggplot(mystery, aes(x = values)) +
  geom_histogram(data = dplyr::filter(mystery, coinflips),
     fill = "red", alpha = 0.2, breaks = br) +
  geom_histogram(data = dplyr::filter(mystery, !coinflips),
     fill = "darkblue", alpha = 0.2, breaks = br) 

## -----------------------------------------------------------------------------
stopifnot(identical(br2, br))

## -----------------------------------------------------------------------------
ggplot(mystery, aes(x = values, fill = coinflips)) +
  geom_histogram(data = dplyr::filter(mystery, coinflips),
     fill = "red", alpha = 0.2, breaks = br) +
  geom_histogram(data = dplyr::filter(mystery, !coinflips),
     fill = "darkblue", alpha = 0.2, breaks = br) +
  geom_histogram(fill = "purple", breaks = br, alpha = 0.2)

## -----------------------------------------------------------------------------
mus = c(-0.5, 1.5)
lambda = 0.5
u = sample(2, size = 100, replace = TRUE, prob = c(lambda, 1-lambda))
x = rnorm(length(u), mean = mus[u])
dux = tibble(u, x)
head(dux)

## -----------------------------------------------------------------------------
group_by(dux, u) |> summarize(mu = mean(x), sigma = sd(x))
table(dux$u) / nrow(dux)

## -----------------------------------------------------------------------------
.o = options(digits = 3)
library("mixtools")
y = c(rnorm(100, mean = -0.2, sd = 0.5),
      rnorm( 50, mean =  0.5, sd =   1))
gm = normalmixEM(y, k = 2, 
                    lambda = c(0.5, 0.5),
                    mu = c(-0.01, 0.01), 
                    sigma = c(3, 3))
with(gm, c(lambda, mu, sigma, loglik))
options(.o)

## -----------------------------------------------------------------------------
## PROVENANCE: here's a record of how the data were created
library("mosaics")
library("mosaicsExample")
for(f in c("wgEncodeSydhTfbsGm12878Stat1StdAlnRep1_chr22_sorted.bam",
           "wgEncodeSydhTfbsGm12878InputStdAlnRep1_chr22_sorted.bam"))
  constructBins(infile = system.file(file.path("extdata", f), package="mosaicsExample"),
    fileFormat = "bam", outfileLoc = "../data/",
    byChr = FALSE, useChrfile = FALSE, chrfile = NULL, excludeChr = NULL,
    PET = FALSE, fragLen = 200, binSize = 200, capping = 0)

datafiles = c("../data/wgEncodeSydhTfbsGm12878Stat1StdAlnRep1_chr22_sorted.bam_fragL200_bin200.txt",
              "../data/wgEncodeSydhTfbsGm12878InputStdAlnRep1_chr22_sorted.bam_fragL200_bin200.txt")
binTFBS = readBins(type = c("chip", "input"), fileName = datafiles)

## -----------------------------------------------------------------------------
binTFBS

## -----------------------------------------------------------------------------
bincts = print(binTFBS)
ggplot(bincts, aes(x = tagCount)) +
  geom_histogram(binwidth = 1, fill = "forestgreen")

## -----------------------------------------------------------------------------
ggplot(bincts, aes(x = tagCount)) + scale_y_log10() +
   geom_histogram(binwidth = 1, fill = "forestgreen")

## -----------------------------------------------------------------------------
masses = c(A =  331, C =  307, G =  347, T =  322)
probs  = c(A = 0.12, C = 0.38, G = 0.36, T = 0.14)
N  = 7000
sd = 3
nuclt   = sample(length(probs), N, replace = TRUE, prob = probs)
quadwts = rnorm(length(nuclt),
                mean = masses[nuclt],
                sd   = sd)
ggplot(tibble(quadwts = quadwts), aes(x = quadwts)) +
  geom_histogram(bins = 100, fill = "purple")

## -----------------------------------------------------------------------------
library("HistData")
ZeaMays$diff
ggplot(ZeaMays, aes(x = diff, ymax = 1/15, ymin = 0)) +
  geom_linerange(linewidth = 1, col = "forestgreen") + ylim(0, 0.1)

## -----------------------------------------------------------------------------
stopifnot(nrow(ZeaMays) == 15)

## -----------------------------------------------------------------------------
B = 1000
meds = replicate(B, {
  i = sample(15, 15, replace = TRUE)
  median(ZeaMays$diff[i])
})
ggplot(tibble(medians = meds), aes(x = medians)) +
  geom_histogram(bins = 30, fill = "purple")

## -----------------------------------------------------------------------------
library("bootstrap")
bootstrap(ZeaMays$diff, B, mean)
bootstrap(ZeaMays$diff, B, median)

## -----------------------------------------------------------------------------
c(N3 = choose(5, 3), N15 = choose(29, 15))

## -----------------------------------------------------------------------------
w = rexp(10000, rate = 1)

## -----------------------------------------------------------------------------
mu  = 0.3
lps = rnorm(length(w), mean = mu, sd = sqrt(w))
ggplot(data.frame(lps), aes(x = lps)) +
  geom_histogram(fill = "purple", binwidth = 0.1)

## -----------------------------------------------------------------------------
mu = 0.3; sigma = 0.4; theta = -1
w  = rexp(10000, 1)
alps = rnorm(length(w), theta + mu * w, sigma * sqrt(w))
ggplot(tibble(alps), aes(x = alps)) +
  geom_histogram(fill = "purple", binwidth = 0.1)

    c('LaplaceMixturePromoterLengths.png', 'tcellhist.png')))

## -----------------------------------------------------------------------------
ggplot(tibble(x = rgamma(10000, shape = 2, rate = 1/3)),
   aes(x = x)) + geom_histogram(bins = 100, fill= "purple")
ggplot(tibble(x = rgamma(10000, shape = 10, rate = 3/2)),
   aes(x = x)) + geom_histogram(bins = 100, fill= "purple")

## -----------------------------------------------------------------------------
lambda = rgamma(10000, shape = 10, rate = 3/2)
gp = rpois(length(lambda), lambda = lambda)
ggplot(tibble(x = gp), aes(x = x)) +
  geom_histogram(bins = 100, fill= "purple")

## -----------------------------------------------------------------------------
library("vcd")
ofit = goodfit(gp, "nbinomial")
plot(ofit, xlab = "")
ofit$par

## -----------------------------------------------------------------------------
x    = 0:95
mu   = 50
vtot = 80
v1   = vtot - mu
scale = v1/mu    # 0.6
shape = mu^2/v1  # 83.3
p1   = dgamma(x = x, scale = 0.6, shape = 80)
p2   = dpois(x = x, lambda = mu*1.2)
p3   = dnbinom(x = x, mu = mu, size = mu^2/vtot)

## -----------------------------------------------------------------------------
library("RColorBrewer")
cols = brewer.pal(8, "Paired")
par(mfrow=c(3,1), mai=c(0.5, 0.5, 0.01, 0.01))
xlim = x[c(1, length(x))]
plot(NA, NA, xlim=xlim, ylim=c(0,0.07), type="n", ylab="", xlab="")
polygon(x, p1, col=cols[1])
abline(v=mu, col="black", lwd=3)
abline(v=mu*1.2, col=cols[2], lty=2, lwd=3)
plot(x, p2, col=cols[3], xlim=xlim, ylab="", xlab="", type="h", lwd=2)
abline(v=mu*1.2, col=cols[2], lwd=2)
abline(v=mu*1.1, col=cols[4], lty=2, lwd=3)
plot(x, p3, col=cols[4], xlim=xlim, type="h", lwd=2, ylab="", xlab="")

## -----------------------------------------------------------------------------
lambdas = seq(100, 900, by = 100)
simdat = lapply(lambdas, function(l)
    tibble(y = rpois(n = 40, lambda=l), lambda = l)
  ) %>% bind_rows
library("ggbeeswarm")
ggplot(simdat, aes(x = lambda, y = y)) +
  geom_beeswarm(alpha = 0.6, color = "purple")
ggplot(simdat, aes(x = lambda, y = sqrt(y))) +
  geom_beeswarm(alpha = 0.6, color = "purple")

## -----------------------------------------------------------------------------
.o = options(digits = 3)
summarise(group_by(simdat, lambda), sd(y), sd(2*sqrt(y)))
options(.o)

## -----------------------------------------------------------------------------
muvalues = 2^seq(0, 10, by = 1)
simgp = lapply(muvalues, function(mu) {
  u = rnbinom(n = 1e4, mu = mu, size = 4)
  tibble(mean = mean(u), sd = sd(u),
         lower = quantile(u, 0.025),
         upper = quantile(u, 0.975),
         mu = mu)
  } ) %>% bind_rows
head(as.data.frame(simgp), 2)
ggplot(simgp, aes(x = mu, y = mean, ymin = lower, ymax = upper)) +
  geom_point() + geom_errorbar()

## -----------------------------------------------------------------------------
simgp = mutate(simgp,
  slopes = 1 / sd,
  trsf   = cumsum(slopes * mean))
ggplot(simgp, aes(x = mean, y = trsf)) +
  geom_point() + geom_line() + xlab("")

## -----------------------------------------------------------------------------
f = function(x, a) 
  ifelse (a==0, 
    sqrt(x), 
    log(2*sqrt(a) * sqrt(x*(a*x+1)) + 2*a*x+1) / (2*sqrt(a)))
x  = seq(0, 24, by = 0.1)
df = lapply(c(0, 0.05*2^(0:5)), function(a) 
  tibble(x = x, a = a, y = f(x, a))) %>% bind_rows()
ggplot(df, aes(x = x, y = y, col = factor(a))) + 
  geom_line() + labs(col = expression(alpha))

## -----------------------------------------------------------------------------
f2 = function(x, a) ifelse (a==0, sqrt(x), acosh(2*a*x + 1) / (2*sqrt(a)))  
with(df, max(abs(f2(x,a) - y)))
stopifnot(with(df, max(abs(f2(x,a) - y))) < 1e-10)

## -----------------------------------------------------------------------------
  a = c(0.2, 0.5, 1)
  f(1e6, a) 
  1/(2*sqrt(a)) * (log(1e6) + log(4*a))

## -----------------------------------------------------------------------------
mx = readRDS("../data/Myst.rds")$yvar
str(mx)
ggplot(tibble(mx), aes(x = mx)) + geom_histogram(binwidth = 0.025)

## -----------------------------------------------------------------------------
wA = runif(length(mx))
wB = 1 - wA

## -----------------------------------------------------------------------------
iter      = 0
loglik    = -Inf
delta     = +Inf
tolerance = 1e-3
miniter   = 50
maxiter   = 1000

## -----------------------------------------------------------------------------
while((delta > tolerance) && (iter <= maxiter) || (iter < miniter)) {
  lambda = mean(wA)
  muA = weighted.mean(mx, wA)
  muB = weighted.mean(mx, wB)
  sdA = sqrt(weighted.mean((mx - muA)^2, wA))
  sdB = sqrt(weighted.mean((mx - muB)^2, wB))

  pA   =    lambda    * dnorm(mx, mean = muA, sd = sdA)
  pB   = (1 - lambda) * dnorm(mx, mean = muB, sd = sdB)
  ptot = pA + pB
  wA   = pA / ptot
  wB   = pB / ptot

  loglikOld = loglik
  loglik = sum(log(pA)) + sum(log(pB))
  delta = abs(loglikOld - loglik)
  iter = iter + 1
}
iter

## -----------------------------------------------------------------------------
.o = options(digits = 3)
c(lambda, muA, muB, sdA, sdB)
options(.o)

## -----------------------------------------------------------------------------
.o = options(digits = 3)
gm = mixtools::normalmixEM(mx, k = 2)
with(gm, c(lambda[1], mu, sigma))
options(.o)

## -----------------------------------------------------------------------------
library("flexmix")
data("NPreg")

## -----------------------------------------------------------------------------
m1 = flexmix(yn ~ x + I(x^2), data = NPreg, k = 2)

## -----------------------------------------------------------------------------
ggplot(NPreg, aes(x = x, y = yn)) + geom_point()

## -----------------------------------------------------------------------------
modeltools::parameters(m1, component = 1)
modeltools::parameters(m1, component = 2)

## -----------------------------------------------------------------------------
table(NPreg$class, modeltools::clusters(m1))

## -----------------------------------------------------------------------------
summary(m1)

## -----------------------------------------------------------------------------
NPreg = mutate(NPreg, gr = factor(class))
ggplot(NPreg, aes(x = x, y = yn, group = gr)) +
   geom_point(aes(colour = gr, shape = gr)) +
   scale_colour_hue(l = 40, c = 180)
