
## -----------------------------------------------------------------------------
library("tibble")
library("ggplot2")
ov = tibble(
  x = seq(0, 30, by = 1),
  y = 2 + 0.01 * x^2 + 0.1 * x + 2 * rnorm(length(x)))
ggplot(ov, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(span = 0.2, col = "dodgerblue3", se = FALSE) +
  geom_smooth(span = 0.8, col = "darkorange1", se = FALSE)

## -----------------------------------------------------------------------------
data("diabetes", package = "rrcov")
head(diabetes)

## -----------------------------------------------------------------------------
library("reshape2")
ggplot(melt(diabetes, id.vars = "group"), aes(x = value, col = group)) +
 geom_density() + facet_wrap( ~variable, ncol = 1, scales = "free") +
 theme(legend.position = "bottom")

## -----------------------------------------------------------------------------
ggdb = ggplot(mapping = aes(x = sspg, y = glucose)) +
  geom_point(aes(colour = group), data = diabetes)
ggdb

## -----------------------------------------------------------------------------
library("MASS")
diabetes_lda = lda(group ~ sspg + glucose, data = diabetes)
diabetes_lda
ghat = predict(diabetes_lda)$class
table(ghat, diabetes$group)
mean(ghat != diabetes$group)

## -----------------------------------------------------------------------------
make1Dgrid = function(x) {
  rg = grDevices::extendrange(x)
  seq(from = rg[1], to = rg[2], length.out = 100)
}

## -----------------------------------------------------------------------------
diabetes_grid = with(diabetes,
  expand.grid(sspg = make1Dgrid(sspg),
              glucose = make1Dgrid(glucose)))

## -----------------------------------------------------------------------------
diabetes_grid$ghat =
  predict(diabetes_lda, newdata = diabetes_grid)$class

## -----------------------------------------------------------------------------
centers = diabetes_lda$means

## -----------------------------------------------------------------------------
unitcircle = exp(1i * seq(0, 2*pi, length.out = 360)) |>
          (\(z) cbind(Re(z), Im(z)))() 
ellipse = unitcircle %*% solve(diabetes_lda$scaling) |> as_tibble()

## -----------------------------------------------------------------------------
library("dplyr")
ellipses = lapply(rownames(centers), function(gr) {
  mutate(ellipse,
     sspg    = sspg    + centers[gr, "sspg"],
     glucose = glucose + centers[gr, "glucose"],
     group   = gr)
}) |> bind_rows()

## -----------------------------------------------------------------------------
ggdb + geom_raster(aes(fill = ghat),
            data = diabetes_grid, alpha = 0.25, interpolate = TRUE) +
    geom_point(data = as_tibble(centers), pch = "+", size = 8) +
    geom_path(aes(colour = group), data = ellipses) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

## -----------------------------------------------------------------------------
diabetes_up = lda(group ~ sspg + glucose, data = diabetes,
  prior = (\(n) rep(1/n, n)) (nlevels(diabetes$group)))

diabetes_grid$ghat_up =
  predict(diabetes_up, newdata = diabetes_grid)$class

stopifnot(all.equal(diabetes_up$means, diabetes_lda$means))

ellipse_up  = unitcircle %*% solve(diabetes_up$scaling) |> as_tibble()
ellipses_up = lapply(rownames(centers), function(gr) {
  mutate(ellipse_up,
     sspg    = sspg    + centers[gr, "sspg"],
     glucose = glucose + centers[gr, "glucose"],
     group   = gr)
}) |> bind_rows()

ggdb + geom_raster(aes(fill = ghat_up),
            data = diabetes_grid, alpha = 0.4, interpolate = TRUE) +
    geom_point(data = data.frame(centers), pch = "+", size = 8) +
    geom_path(aes(colour = group), data = ellipses_up) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

## -----------------------------------------------------------------------------
diabetes_lda5 = lda(group ~ rw + fpg + glucose + sspg + insulin, data = diabetes)
diabetes_lda5
ghat5 = predict(diabetes_lda5)$class
table(ghat5, diabetes$group)
mean(ghat5 != diabetes$group)

## -----------------------------------------------------------------------------
library("Hiiragi2013")
data("x")
probes = c("1426642_at", "1418765_at", "1418864_at", "1416564_at")
embryoCells = t(Biobase::exprs(x)[probes, ]) |> as_tibble() |>
  mutate(Embryonic.day = x$Embryonic.day) |>
  dplyr::filter(x$genotype == "WT")

## -----------------------------------------------------------------------------
annotation(x)
library("mouse4302.db")
anno = AnnotationDbi::select(mouse4302.db, keys = probes,
                             columns = c("SYMBOL", "GENENAME"))
anno
mt = match(anno$PROBEID, colnames(embryoCells))
colnames(embryoCells)[mt] = anno$SYMBOL

## -----------------------------------------------------------------------------
stopifnot(!any(is.na(mt)))

## -----------------------------------------------------------------------------
library("GGally")
ggpairs(embryoCells, mapping = aes(col = Embryonic.day),
  columns = anno$SYMBOL, upper = list(continuous = "points"))

## -----------------------------------------------------------------------------
ec_lda = lda(Embryonic.day ~ Fn1 + Timd2 + Gata4 + Sox7,
             data = embryoCells)
round(ec_lda$scaling, 1)

## -----------------------------------------------------------------------------
ec_rot = predict(ec_lda)$x |> as_tibble() |>
           mutate(ed = embryoCells$Embryonic.day)
ec_lda2 = lda(ec_rot[, 1:2], predict(ec_lda)$class)
ec_grid = with(ec_rot, expand.grid(
  LD1 = make1Dgrid(LD1),
  LD2 = make1Dgrid(LD2)))
ec_grid$edhat = predict(ec_lda2, newdata = ec_grid)$class
ggplot() +
  geom_point(aes(x = LD1, y = LD2, colour = ed), data = ec_rot) +
  geom_raster(aes(x = LD1, y = LD2, fill = edhat),
            data = ec_grid, alpha = 0.4, interpolate = TRUE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed()

## -----------------------------------------------------------------------------
library("gridExtra")

ec_qda = qda(Embryonic.day ~ Fn1 + Timd2 + Gata4 + Sox7,
             data = embryoCells)

variables = colnames(ec_qda$means)
pairs = combn(variables, 2)
lapply(seq_len(ncol(pairs)), function(i) {
  grid = with(embryoCells,
    expand.grid(x = make1Dgrid(get(pairs[1, i])),
                y = make1Dgrid(get(pairs[2, i])))) |>
    `colnames<-`(pairs[, i])

  for (v in setdiff(variables, pairs[, i]))
    grid[[v]] = median(embryoCells[[v]])

  grid$edhat = predict(ec_qda, newdata = grid)$class

  x <- pairs[1,i]
  y <- pairs[2,i]
  ggplot() + 
    geom_point(
      data = embryoCells,
      aes(x = .data[[x]], y = .data[[y]], colour = Embryonic.day)
    ) +
    geom_raster(
      aes(x = .data[[x]], y = .data[[y]], fill = edhat),
      data = grid, alpha = 0.4, interpolate = TRUE
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_fixed() +
    if (i != ncol(pairs)) theme(legend.position = "none")
}) |> (\(g) grid.arrange(grobs = g, ncol = 2))()

## -----------------------------------------------------------------------------
lda(t(Biobase::exprs(x))[, 1:1000], x$Embryonic.day)
warnings()
qda(t(Biobase::exprs(x))[, 1:1000], x$Embryonic.day)

## -----------------------------------------------------------------------------
p = 2:21
n = 20

mcl = lapply(p, function(pp) {
  replicate(100, {
    xmat = matrix(rnorm(n * pp), nrow = n)
    resp = sample(c("apple", "orange"), n, replace = TRUE)
    fit  = lda(xmat, resp)
    pred = predict(fit)$class
    mean(pred != resp)
  }) |> mean() |> (\(x) tibble(mcl = x, p = pp))()
}) |> bind_rows()

ggplot(mcl, aes(x = p, y = mcl)) + 
  geom_line() + geom_point() +
  ylab("Misclassification rate")

## -----------------------------------------------------------------------------
estimate_mcl_loocv = function(x, resp) {
  vapply(seq_len(nrow(x)), function(i) {
    fit  = lda(x[-i, ], resp[-i])
    ptrn = predict(fit, newdata = x[-i,, drop = FALSE])$class
    ptst = predict(fit, newdata = x[ i,, drop = FALSE])$class
    c(train = mean(ptrn != resp[-i]), test = (ptst != resp[i]))
  }, FUN.VALUE = numeric(2)) |> rowMeans() |> t() |> as_tibble()
}

xmat = matrix(rnorm(n * last(p)), nrow = n)
resp = sample(c("apple", "orange"), n, replace = TRUE)

mcl = lapply(p, function(k) {
  estimate_mcl_loocv(xmat[, 1:k], resp)
}) |> bind_rows() |> data.frame(p) |> melt(id.var = "p")

ggplot(mcl, aes(x = p, y = value, col = variable)) + geom_line() +
  geom_point() + ylab("Misclassification rate")

## -----------------------------------------------------------------------------
p   = 2:20
mcl = replicate(100, {
  xmat = matrix(rnorm(n * last(p)), nrow = n)
  resp = sample(c("apple", "orange"), n, replace = TRUE)
  xmat[, 1:6] = xmat[, 1:6] + as.integer(factor(resp))

  lapply(p, function(k) {
    estimate_mcl_loocv(xmat[, 1:k], resp)
  }) |> bind_rows() |> cbind(p = p) |> melt(id.var = "p")
}, simplify = FALSE) |> bind_rows()

mcl = group_by(mcl, p, variable) |> summarise(value = mean(value))

ggplot(mcl, aes(x = p, y = value, col = variable)) + geom_line() +
   geom_point() + ylab("Misclassification rate")

## -----------------------------------------------------------------------------
sideLength = function(p, pointDensity = 1e6, pointsNeeded = 10)
  (pointsNeeded / pointDensity) ^ (1 / p)
ggplot(tibble(p = 1:400, sideLength = sideLength(p)),
       aes(x = p, y = sideLength)) + geom_line(col = "red") +
  geom_hline(aes(yintercept = 1), linetype = 2)

## -----------------------------------------------------------------------------
tibble(
  p = 1:400,
  volOuterCube = 1 ^ p,
  volInnerCube = 0.98 ^ p,  # 0.98 = 1 - 2 * 0.01
  `V(shell)` = volOuterCube - volInnerCube) |>
ggplot(aes(x = p, y =`V(shell)`)) + geom_line(col = "blue")

## -----------------------------------------------------------------------------
n = 1000
df = tibble(
  p = round(10 ^ seq(0, 4, by = 0.25)),
  cv = vapply(p, function(k) {
    x1 = matrix(runif(k * n), nrow = n)
    x2 = matrix(runif(k * n), nrow = n)
    d = sqrt(rowSums((x1 - x2)^2))
    sd(d) / mean(d)
  }, FUN.VALUE = numeric(1)))
ggplot(df, aes(x = log10(p), y = cv)) + geom_line(col = "orange") +
  geom_point()

## -----------------------------------------------------------------------------
## table(truth, response)

    IMGS, c('TargetBias.png','TargetVariance.png')))

## -----------------------------------------------------------------------------
library("ExperimentHub")
eh = ExperimentHub()
zeller = eh[["EH361"]]

## -----------------------------------------------------------------------------
table(zeller$disease)

## -----------------------------------------------------------------------------
zellerNC = zeller[, zeller$disease %in% c("n", "cancer")]

## -----------------------------------------------------------------------------
stopifnot(is.numeric(Biobase::exprs(zellerNC)), !any(is.na(Biobase::exprs(zellerNC))))

## -----------------------------------------------------------------------------
pData(zellerNC)[ sample(ncol(zellerNC), 3), ]

## -----------------------------------------------------------------------------
formatfn = function(x)
   gsub("|", "| ", x, fixed = TRUE) |> lapply(strwrap)

rownames(zellerNC)[1:4]
rownames(zellerNC)[nrow(zellerNC) + (-2:0)] |> formatfn()

## -----------------------------------------------------------------------------
ggplot(melt(Biobase::exprs(zellerNC)[c(510, 527), ]), aes(x = value)) +
    geom_histogram(bins = 25) +
    facet_wrap( ~ Var1, ncol = 1, scales = "free")

## -----------------------------------------------------------------------------
library("glmnet")
glmfit = glmnet(x = t(Biobase::exprs(zellerNC)),
                y = factor(zellerNC$disease),
                family = "binomial")

## -----------------------------------------------------------------------------
predTrsf = predict(glmfit, newx = t(Biobase::exprs(zellerNC)),
                   type = "class", s = 0.04)
table(predTrsf, zellerNC$disease)

## -----------------------------------------------------------------------------
par(mai = c(0.5, 0.5, 0.575, 0.05))
plot(glmfit, col = brewer.pal(8, "Dark2"), lwd = sqrt(3), ylab = "")

## -----------------------------------------------------------------------------
cvglmfit = cv.glmnet(x = t(Biobase::exprs(zellerNC)),
                     y = factor(zellerNC$disease),
                     family = "binomial")
plot(cvglmfit)

## -----------------------------------------------------------------------------
cvglmfit$lambda.min

## -----------------------------------------------------------------------------
cvglmfit$lambda.1se

## -----------------------------------------------------------------------------
s0 = cvglmfit$lambda.1se
predict(glmfit, newx = t(Biobase::exprs(zellerNC)),type = "class", s = s0) |>
    table(zellerNC$disease)

## -----------------------------------------------------------------------------
coefs = coef(glmfit)[, which.min(abs(glmfit$lambda - s0))]
topthree = order(abs(coefs), decreasing = TRUE)[1:3]
as.vector(coefs[topthree])
formatfn(names(coefs)[topthree])

## -----------------------------------------------------------------------------
cv.glmnet(x = t(asinh(Biobase::exprs(zellerNC))),
          y = factor(zellerNC$disease),
          family = "binomial") |> plot()

## -----------------------------------------------------------------------------
sx = x[, x$Embryonic.day == "E3.25"]
embryoCellsClassifier = cv.glmnet(t(Biobase::exprs(sx)), sx$genotype,
                family = "binomial", type.measure = "class")
plot(embryoCellsClassifier)

## -----------------------------------------------------------------------------
stopifnot(sum((diff(embryoCellsClassifier$cvm) * diff(embryoCellsClassifier$lambda)) < 0) <= 2)

## -----------------------------------------------------------------------------
mouse_de = rowttests(sx, "genotype")
ggplot(mouse_de, aes(x = p.value)) +
  geom_histogram(boundary = 0, breaks = seq(0, 1, by = 0.01))

## -----------------------------------------------------------------------------
dists = as.matrix(dist(scale(t(Biobase::exprs(x)))))
diag(dists) = +Inf

## -----------------------------------------------------------------------------
nn = sapply(seq_len(ncol(dists)), function(i) which.min(dists[, i]))
table(x$sampleGroup, x$sampleGroup[nn]) |> `colnames<-`(NULL)

## -----------------------------------------------------------------------------
library("caret")
caretMethods = names(getModelInfo())
head(caretMethods, 8)
length(caretMethods)

## -----------------------------------------------------------------------------
getModelInfo("nnet", regex = FALSE)[[1]]$parameter

## -----------------------------------------------------------------------------
trnCtrl = trainControl(
  method = "repeatedcv",
  repeats = 3,
  classProbs = TRUE)
tuneGrid = expand.grid(
  size = c(2, 4, 8),
  decay = c(0, 1e-2, 1e-1))
nnfit = train(
  Embryonic.day ~ Fn1 + Timd2 + Gata4 + Sox7,
  data = embryoCells,
  method = "nnet",
  tuneGrid  = tuneGrid,
  trControl = trnCtrl,
  metric = "Accuracy")

## -----------------------------------------------------------------------------
nnfit
plot(nnfit)
predict(nnfit) |> head(10)

## -----------------------------------------------------------------------------
## library("kernlab")
## kfunction= function(linear =0, quadratic=0)
## {  k = function (v,w){ linear*sum((v)*(w)) + quadratic*sum((v^2)*(w^2))}
##   class(k) = "kernel"
##   return(k) }
## subx=subx[,2:3]
## svp = ksvm(subx,dftxy$tg,type="C-svc",C = 100, kernel=kfunction(1,0),scaled=c())
## plot(c(min(subx[,1]), max(subx[,1])),c(min(subx[,2]), max(subx[,2])),
##             type='n',xlab='x1',ylab='x2')
## ymat = ymatrix(svp)
## points(subx[-SVindex(svp),1], subx[-SVindex(svp),2],
##          pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
## points(subx[SVindex(svp),1], subx[SVindex(svp),2],
##          pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))
## 
## # Extract w and b from the model
## w = colSums(coef(svp)[[1]] * subx[SVindex(svp),])
## b = b(svp)
## # Draw the lines
## abline(b/w[2],-w[1]/w[2])
## abline((b+1)/w[2],-w[1]/w[2],lty=2)
## abline((b-1)/w[2],-w[1]/w[2],lty=2)
