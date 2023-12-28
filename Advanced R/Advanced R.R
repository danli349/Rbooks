library(lobstr)
library(h2o)
library(rlang)
library(tibble)
library(dplyr)
library(jpeg)
library(magrittr)
library(purrr)
library(dada2)
library(ggplot2)
library(scales)
library(memoise)
library(sloop)
packageVersion("dada2")
detach(package:rlang)

.libPaths()

sessionInfo()
sessionInfo
Sys.info()
R.Version()
R.version
R.version.string
version

Sys.time()

options(warnPartialMatchDollar = TRUE)
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
codetools::findGlobals(g11)

typeof(sample)
class(sample)
args(sample)
body(sample)
str(sample)
attributes(sample)
environment(sample)


lobstr::obj_addr(mean)
lobstr::obj_addr(base::mean)
lobstr::obj_addr(get("mean"))
lobstr::obj_addr(evalq(mean))
lobstr::obj_addr(match.fun("mean"))

Sys.Date()
Sys.time()
Sys.timezone(location = TRUE)
Sys.timezone()
str(OlsonNames())
OlsonNames()

?as.POSIXct
structure(now_ct, tzone = "Asia/Tokyo")
#> [1] "2018-08-02 07:00:00 JST"
structure(Sys.time(), tzone = "Asia/Shanghai")
one_week_1 <- as.difftime(1, units = "weeks")
one_week_1
typeof(one_week_1)
attributes(one_week_1)
one_week_2 <- as.difftime(7, units = "days")
one_week_2
typeof(one_week_2)
attributes(one_week_2)
class(one_week_2)
str(one_week_2)
?class


rlang::env_print(e1)
rlang::env_names(e1)
names(e1)
ls(e1, all.names = TRUE)
rlang::current_env()
rlang::global_env()
identical(rlang::global_env(), rlang::current_env())
base::globalenv()
base::environment()
base::parent.env(e1)
rlang::env_parent(e1)
rlang::empty_env()
rlang::caller_env()
base::parent.frame()
e1 <- rlang::env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3,
)
e1$a
e1[["a"]]
rlang::env_get(e1, "a")
rlang::env_bind(e1, e = 10, f = 20)
rlang::env_names(e1)
rlang::env_has(e1, "a")
rlang::env_unbind(e1, "a")
rlang::env_has(e1, "a")
#>     a 
#> FALSE
rlang::search_envs()
base::search()
y <- 1
f <- function(x) x + y
rlang::fn_env(f)
base::environment(f)
sd



rlang::abort("This is an error!")
stop("error!")
warning("warning:")
message("message:")



triple <- function(x) x * 3
purrr::map(1:3, triple)
lapply(1:3, triple)

x <- purrr::map(1:3, ~ runif(2))
str(x)


map(mtcars, c(unique, length)) # correct
lapply(mtcars, c(unique, length)) #wrong

lapply(
  mtcars,
  function(x)
  {
  y <- unique(x)
  z <- length(y)
  z
}
)

map_dbl(mtcars, c(unique, length))

split(mtcars, mtcars$cyl)


#Output same type as input with modify()
df <- data.frame(
  x = 1:3,
  y = 6:4
)
df
map(df, ~ .x * 2)
modify(df, ~ .x * 2)
modify_if(df, is.numeric, ~ .x * 2) #This allows you to only double numeric columns of a data frame
#Iterate over two inputs with map2().
#Iterate with an index using imap()
#Return nothing with walk().
#Iterate over any number of inputs with pmap()
map2_dbl(xs, ws, weighted.mean) #map2(), which is vectorised over two arguments, Instead of iterating over one vector, we iterate over two in parallel:
map2_dbl(xs, ws, weighted.mean, na.rm = TRUE) #Additional arguments still go afterwards
map(xs, weighted.mean, ws[[2]])
walk(names, welcome)  #that ignore the return values of the .f and instead return .x invisibly.
imap_chr(iris, ~ paste0("The first value of ", .y, " is ", .x[[1]]))
#imap() is like map2() in the sense that your .f gets called with two arguments, but here both are derived from the vector. imap(x, f) is equivalent to map2(x, names(x), f) if x has names, and map2(x, seq_along(x), f) if it does not.
names(iris)
iris[[1]]
#pmap(): you supply it a single list, which contains any number of arguments. In most cases, that will be a list of equal-length vectors, i.e. something very similar to a data frame.
list(xs, ws)
pmap_dbl(list(xs, ws), weighted.mean)
#> [1]    NA 0.451 0.603 0.452 0.563 0.510 0.342 0.464
pmap_dbl(list(xs, ws), weighted.mean, na.rm = TRUE)
#> [1] 0.504 0.451 0.603 0.452 0.563 0.510 0.342 0.464
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
x
pmap_dbl(list(trim = trims), mean, x = x)
#> [1] -6.6740  0.0210  0.0235  0.0151
?rcauchy

l <- map(1:4, ~ sample(1:10, 15, replace = T))
reduce(l, intersect)
# is equivalent to 
out <- l[[1]]
out <- intersect(out, l[[2]])
out <- intersect(out, l[[3]])
out <- intersect(out, l[[4]])
#> [1] 8 4
reduce(l, union)
#>  [1]  7  1  8  3  2  4 10  5  9  6

integrate() #finds the area under the curve defined by f()
uniroot() #finds where f() hits zero
optimise() #finds the location of the lowest (or highest) value of f()

integrate(sin, 0, pi)
#> 2 with absolute error < 2.2e-14
uniroot(sin, pi * c(1 / 2, 3 / 2))
#> List of 5
#>  $ root      : num 3.14
#>  $ f.root    : num 1.22e-16
#>  $ iter      : int 2
#>  $ init.it   : int NA
#>  $ estim.prec: num 6.1e-05
optimise(sin, c(0, 2 * pi))
#> List of 2
#>  $ minimum  : num 4.71
#>  $ objective: num -1
optimise(sin, c(0, pi), maximum = TRUE)
#> List of 2
#>  $ maximum  : num 1.57
#>  $ objective: num 1
#Function factories
power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}
square <- power1(2) #exp in its enclosing environment. x in the execution environment 
cube <- power1(3)
power1(3)(2)
rlang::env_print(square)
rlang::env_print(cube)
rlang::fn_env(square)
rlang::fn_env(square)$exp
#> [1] 2
rlang::fn_env(cube)$exp

y <- c(12345, 123456, 1234567)
scales::comma_format()(y)
#> [1] "12,345"    "123,456"   "1,234,567"
scales::comma_format(y)
scales::number_format(scale = 1e-3, suffix = " K")(y)
#> [1] "12 K"    "123 K"   "1 235 K"
scales::number_format(scale = 1e3, suffix = " K")(y)
scales::scientific_format()(y)

names <- list(
  square = 2, 
  cube = 3, 
  root = 1/2, 
  cuberoot = 1/3, 
  reciprocal = -1
)
funs <- purrr::map(names, power1)
rlang::env_bind(globalenv(), !!!funs) #you could copy the functions to the global environment with env_bind().
root(100)
#> [1] 10
rlang::env_unbind(globalenv(), names(funs)) #You can later unbind those same names, 

x <- list(
  c(0.512, 0.165, 0.717),
  c(0.064, 0.781, 0.427),
  c(0.890, 0.785, 0.495),
  "oops"
)
purrr::safely(sum)(x[[1]]) #safely() takes a function and returns a wrapped function which we can call as usual
purrr::safely(sum)(x[[4]]) #a function transformed by safely() always returns a list with two elements, result and error. If the function runs successfully, error is NULL and result contains the result; if the function fails, result is NULL and error contains the error.
map(x, safely(sum))
purrr::transpose(map(x, safely(sum))) #transpose() make the output easier to use by turning it “inside-out”,  so that we get a list of results and a list of errors:

x[map_lgl(transpose(map(x, safely(sum)))$error, is.null)] #Now we can easily find the results that worked
x[!map_lgl(transpose(map(x, safely(sum)))$error, is.null)] #, or the inputs that failed:
transpose(map(x, safely(sum)))$result
transpose(map(x, safely(sum)))$error
fit_model <- function(df) {
  glm(y ~ x1 + x2 * x3, data = df)
}

models <- transpose(map(datasets, safely(fit_model)))
ok <- map_lgl(models$error, is.null)

# which data failed to converge?
datasets[!ok]

# which models were successful?
models[ok]

memoise::memoise() #It memoises a function, meaning that the function will remember previous inputs and return cached results.
fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}   #computing the Fibonacci series
fib(23)
system.time(fib(23))
#>    user  system elapsed 
#>   0.042   0.000   0.042
system.time(fib(24))
#>    user  system elapsed 
#>   0.066   0.000   0.067
fib2 <- memoise::memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})
system.time(fib2(23))
#>    user  system elapsed 
#>   0.026   0.000   0.026
fib(24)
fib2(24)
system.time(fib2(24))
tempdir()

sloop::otype(1:10)
#> [1] "base"
sloop::otype(mtcars)
#> [1] "S3"
mle_obj <- stats4::mle(function(x = 1) (x - 2) ^ 2)
otype(mle_obj)
#> [1] "S4"
# A base object:
is.object(1:10) # the difference between base and OO objects is that OO objects have a “class” attribute:
#> [1] FALSE
sloop::otype(1:10)
#> [1] "base"
# An OO object
is.object(mtcars)
#> [1] TRUE
sloop::otype(mtcars)
#> [1] "S3"
attr(1:10, "class")
#> NULL
attr(mtcars, "class")
#> [1] "data.frame"
x <- matrix(1:4, nrow = 2)
class(x)
#> [1] "matrix"
class(mtcars)
#> [1] "data.frame"
sloop::s3_class(x)
#> [1] "matrix"  "integer" "numeric"
typeof(1:10) # While only OO objects have a class attribute, every object has a base type
#> [1] "integer"

typeof(mtcars)
#> [1] "list"
typeof(mle_obj)

#Vectors, Chapter 3, include types NULL (NILSXP), logical (LGLSXP), integer (INTSXP), double (REALSXP), complex (CPLXSXP), character (STRSXP), list (VECSXP), and raw (RAWSXP).
typeof(NULL)
#> [1] "NULL"
typeof(1L)
#> [1] "integer"
typeof(1i)
#> [1] "complex"
#Functions, Chapter 6, include types closure (regular R functions, CLOSXP), special (internal functions, SPECIALSXP), and builtin (primitive functions, BUILTINSXP).
typeof(mean)
#> [1] "closure"
typeof(`[`)
#> [1] "special"
typeof(sum)    
#> [1] "builtin"
#Environments, Chapter 7, have type environment (ENVSXP).
typeof(globalenv())
#> [1] "environment"
#The S4 type (S4SXP), Chapter 15, is used for S4 classes that don’t inherit from an existing base type.
mle_obj <- stats4::mle(function(x = 1) (x - 2) ^ 2)
typeof(mle_obj)
#> [1] "S4"
#Language components, Chapter 18, include symbol (aka name, SYMSXP), language (usually called calls, LANGSXP), and pairlist (used for function arguments, LISTSXP) types.
quote(a)
typeof(quote(a))
#> [1] "symbol"
typeof(quote(a + 1))
#> [1] "language"
typeof(formals(mean))
#> [1] "pairlist"
#The remaining types are esoteric and rarely seen in R. They are important primarily for C code: externalptr (EXTPTRSXP), weakref (WEAKREFSXP), bytecode (BCODESXP), promise (PROMSXP), ... (DOTSXP), and any (ANYSXP).

#An S3 object is a base type with at least a class attribute
#S3 objects: factor, Date, difftime, POSIXct, and POSIXlt classes, all of them have attributes.
f <- factor(c("a", "b", "c"))  #S3 object
g <- c("a", "b", "c")  #base type
typeof(f)
#> [1] "integer"
typeof(g)
class(f)
#> [1] "factor"
class(g)
attributes(f) #An S3 object is a base type with at least a class attribute
#> $levels
#> [1] "a" "b" "c"
#> 
#> $class
#> [1] "factor"
attributes(g)
unclass(f) #You can get the underlying base type by unclass()ing it, which strips the class attribute, causing it to lose its special behaviour:
#> [1] 1 2 3
#> attr(,"levels")
#> [1] "a" "b" "c"


# The easiest way to tell if a function is a generic is to use sloop::ftype() and look for “generic” in the output:
ftype(print)
#> [1] "S3"      "generic"
class(print)
sloop::ftype(class)
sloop::ftype(str)
#> [1] "S3"      "generic"
sloop::ftype(unclass); sloop::ftype(class)
#> [1] "primitive" [1] "primitive"

sloop::ftype(print)
#> [1] "S3"      "generic"
sloop::ftype(str)
#> [1] "S3"      "generic"
sloop::ftype(unclass)
#> [1] "primitive"
f <- factor(c("a", "b", "c"))
f
sloop::s3_dispatch(print(f))
#> => print.factor
#>  * print.default

sloop::s3_get_method(weighted.mean.Date)
#> function (x, w, ...) 
#> structure(weighted.mean(unclass(x), w, ...), class = "Date")
#> <bytecode: 0x5be6b48>
#> <environment: namespace:stats>

# Create and assign class in one step
x <- structure(list(), class = "my_class")

# or Create, then set class
x <- list()
x
class(x) <- "my_class"
class(x)
#You can determine the class of an S3 object with class(x), and see if an object is an instance of a class using inherits(x, "classname")
class(x)
#> [1] "my_class"
inherits(x, "my_class")
#> [1] TRUE


#The generic is a middleman: its job is to define the interface (i.e. the arguments) then find the right implementation for the job. The implementation for a specific class is called a method, and the generic finds that method by performing method dispatch.
#You can use sloop::s3_dispatch() to see the process of method dispatch
s3_dispatch(print(f))
#> => print.factor
#>  * print.default
s3_dispatch(print(g))
#sloop::s3_get_method() to see the source code for most S3 methods

#to make an object an instance of a class, you simply set the class attribute.
# Create and assign class in one step
x <- structure(list(), class = "my_class")
x
# Create, then set class
x <- list()
x
class(x) <- "my_class"

#You can determine the class of an S3 object with class(x), and see if an object is an instance of a class using inherits(x, "classname").
class(x)
#> [1] "my_class"
inherits(x, "my_class")
#> [1] TRUE
inherits(x, "your_class")
#> [1] FALSE

mtcars %>%
  summarise(mean = mean(disp), n = n())

# Create a linear model
mod <- lm(log(mpg) ~ log(disp), data = mtcars)
class(mod)
#> [1] "lm"
mod
print(mod)
#> 
#> Call:
#> lm(formula = log(mpg) ~ log(disp), data = mtcars)
#> 
#> Coefficients:
#> (Intercept)    log(disp)  
#>       5.381       -0.459

new_difftime <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))
  
  structure(x,
            class = "difftime",
            units = units
  )
}
new_difftime
new_difftime(c(1, 10, 3600), "secs")
#> Time differences in secs
#> [1]    1   10 3600
new_difftime(52, "weeks")
#> Time difference of 52 weeks

validate_factor <- function(x) {
  values <- unclass(x)
  levels <- attr(x, "levels")
  
  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }
  
  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }
  
  x
}

validate_factor(new_factor(1:5, "a"))
#> Error: There must be at least as many `levels` as possible values in `x`
validate_factor(new_factor(0:1, "a"))
#> Error: All `x` values must be non-missing and greater than zero

difftime <- function(x = double(), units = "secs") {
  x <- as.double(x)
  new_difftime(x, units = units)
}

difftime(1:10)
#> Time differences in secs
#>  [1]  1  2  3  4  5  6  7  8  9 10












# 2 Names and values

#2 Names and values
#2.1 Introduction
df <- data.frame(runif(3), runif(3))
df
names(df) <- c(1, 2)
df$"1"
df$"3" <- df$"1" + df$"2"
df

x <- runif(1e6)
y <- list(x, x, x)
obj_size(y)
a <- c(1, 5, 3, 2)
a[[1]]
a[1]
a[1][1]
a
class(a)
a <- c(1, 2, 3)
b <- a
lobstr::obj_addr(a)
lobstr::obj_addr(b)


'if' <- 10
"if" <- 12
'if'
'if'
"if"
a <- 10
a
`if` <- 10
`if`
mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

lobstr::obj_addr(mean)
lobstr::obj_addr(base::mean)
lobstr::obj_addr(get("mean"))
lobstr::obj_addr(evalq(mean))
lobstr::obj_addr(match.fun("mean"))

#2.3 Copy-on-modify
x <- c(1, 2, 3)
tracemem(x)
cat(tracemem(x), "\n")
y <- x
y[[3]] <- 4L
y[[3]] <- 5L
y

#2.3.2 Function calls
#2.3.3 Lists
l1 <- list(1, 2, 3)
l2 <- l1
l1
l2
ref(l1, l2)
ref(l1)
ref(l2)
l2[[3]] <- 4
l2
ref(l1, l2)

#2.3.4 Data frames
d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))
d1
ref(d1)
d2 <- d1
ref(d2)
d2[, 2] <- d2[, 2] * 2
d3 <- d1
d3[1, ] <- d3[1, ] * 3
ref(d3)

x <- c("a", "a", "abc", "d")
x
ref(x, character = TRUE)

a <- 1:10
ref(a)
b <- list(a, a)
b
ref(b)
c <- list(b, a, 1:10)
c
ref(c)


x <- list(1:10)
ref(x)
x[[2]] <- x
x

#2.4 Object size
letters
obj_size(letters)
obj_size(ggplot2::diamonds)
a <- runif(1e6)
obj_size(a)
b <- list(a, a)
obj_size(b)
b[[1]][[1]] <- 10
obj_size(b)
obj_size(a, b)
b[[2]][[1]] <- 10
obj_size(b)
obj_size(a, b)

#2.5 Modify-in-place
e <- rlang::env()
e$self <- e
ref(e)
x <- list()
x
x[[1]] <- x
x

#2.6 Unbinding and the garbage collector
mem_used()
gc()
x <- c(1L, 2L, 3L)

x
tracemem(x)
x[[3]] <- 4
x

x <- list(1:10)
x
x[2]
x[1]
x[[2]]
x[[1]]
ref(x)
tracemem(x)
x[[2]] <- x
x
ref(x)


x <- runif(1e6)

obj_size(x)
vector("list", 2)

y <- list(x, x) 
obj_size(y)

gcinfo(TRUE)
lobstr::mem_used()

#3 Vectors
##3.1 Introduction

str(c("a", 1))
x <- c(FALSE, FALSE, TRUE)
x
as.numeric(x)
sum(x)
mean(x)
as.integer(c("1", "1.5", "a"))

c(1, FALSE)
c("a", 1)
c(TRUE, 1L)

-1 < FALSE

#3.3 Attributes
a <- 1:3
attr(a, "x") <- "abcdef"
attr(a, "x")
a
attr(a, "y") <- 4:6
a
attributes(a)
attr(a)
a[1]
array(1:3, 3)

#3.4 S3 atomic vectors

lgl_var <- c(TRUE, FALSE)
int_var <- c(1L, 6L, 10L)
dbl_var <- c(1, 2.5, 4.5)
chr_var <- c("these are", "some strings")
typeof(lgl_var) #> [1] "logical"
typeof(int_var) #> [1] "integer"
typeof(dbl_var) #> [1] "double"
typeof(chr_var) #> [1] "charact

x <- factor(c("a", "b", "b", "a"))
x
levels(x) <- rev(levels(x))
x
typeof(x)
attributes(x)
ls()
rm(list=ls())
grade <- ordered(c("b", "b", "a", "c"), levels = c("c", "b", "a"))
grade
sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))
sex_factor
factor(letters)
rev(factor(letters))
factor(letters, levels = rev(letters))

Sys.Date()
Sys.time()
today <- Sys.Date()
typeof(today)
attributes(today)
date <- as.Date("1970-02-01")
unclass(date)
unclass(today)

now_ct <- as.POSIXct("2018-08-01 22:00", tz = "UTC")
now_ct
typeof(now_ct)
unclass(now_ct)
attributes(now_ct)

Sys.Date()
Sys.time()
Sys.timezone(location = TRUE)
Sys.timezone()
str(OlsonNames())
OlsonNames()
table(sl <- grepl("/", OlsonNames()))
OlsonNames()[!sl]
OlsonNames()[sl]
head(Osl <- strsplit(OlsonNames()[sl], "/"))
(tOS1 <- table(vapply(Osl, `[[`, "", 1)))
table(lengths(Osl))
str(Osl[lengths(Osl) >= 3])


?as.POSIXct
structure(now_ct, tzone = "Asia/Tokyo")
#> [1] "2018-08-02 07:00:00 JST"
structure(now_ct, tzone = "America/New_York")
#> [1] "2018-08-01 18:00:00 EDT"
structure(now_ct, tzone = "Australia/Lord_Howe")
#> [1] "2018-08-02 08:30:00 +1030"
structure(now_ct, tzone = "Europe/Paris")
#> [1] "2018-08-02 CEST"
structure(Sys.time(), tzone = "Asia/Shanghai")


one_week_1 <- as.difftime(1, units = "weeks")
one_week_1
typeof(one_week_1)
attributes(one_week_1)
one_week_2 <- as.difftime(7, units = "days")
one_week_2
typeof(one_week_2)
attributes(one_week_2)
class(one_week_2)
str(one_week_2)
?class


#3.5 Lists
c(list(1, 2), list(3, 4))
li <- list(1:3)
li
typeof(li)
class(li)
str(li)

li2 <- as.list(1:3)
typeof(li2)
class(li2)
str(li2)

#3.6 Data frames and tibbles
df1 <- data.frame(x = 1:3, y = letters[1:3])
df1
typeof(df1)
class(df1)
attributes(df1)
length(df1)
nrow(df1)
ncol(df1)
rownames(df1)
colnames(df1)
str(df1)
summary(df1)



df2 <- tibble(x = 1:3, y = letters[1:3])
df2
str(df2)
class(df2)
typeof(df2)
attributes(df2)
length(df2)
names(df2)
rownames(df2)
colnames(df2)

df2 <- tibble(x = 1:3, y = letters[1:3])
typeof(df2)
class(df2)
typeof(df2)
attributes(df2)
length(df2)
names(df2)
rownames(df2)
colnames(df2)

df2 <- tibble(
  x = 1:3, 
  y = c("a", "b", "c")
)
df2

names(data.frame(`1` = 1))
names(tibble(`1` = 1))

tibble(
  x = 1:3,
  y = x * 2
)


tibble(x = 1:4, y = 1:4)

rlang::last_error()
tibble(
  x = 1:3,
  y = x * 2
)
df3 <- data.frame(
  age = c(35, 27, 18),
  hair = c("blond", "brown", "black"),
  row.names = c("Bob", "Susan", "Sam")
)
df3
df3[c(1,1,1),]

df4 <- as_tibble(df3, rownames = "name")
df4
is.data.frame(df4)
is_tibble(df4)
df4[1,]
df4[c(1,1),]
df4$name
df4$age
df4$hair
summary(df4)



df5 <- data.frame(xyz = "a", stringsAsFactors = F)
df5
df5$x
str(df5$x)
class(df5[["xyz"]])
df2 <- tibble(xyz = "a")
class(df2[["xyz"]])


df6 <- data.frame(x = 1:3)
df6
df6$y <- list(1:2, 1:3, 1:4)
df6

str(df6)
summary(df6)
typeof(df6)
class(df6)
attributes(df6)


list(1:2, 1:3, 1:4)
data.frame(
  x = 1:3, 
  y = I(list(1:2, 1:3, 1:4))
)
df7 <- tibble(
  x = 1:3, 
  y = list(1:2, 1:3, 1:4)
)
df7
dim(df7)
nrow(df7)
rownames(df7)


dfm <- data.frame(
  x = 1:3 * 10
)
dfm
dfm$y <- matrix(1:9, nrow = 3)
dfm
dfm$y
dfm$z <- data.frame(a = 3:1, b = letters[1:3], stringsAsFactors = FALSE)
dfm
dfm$z
str(dfm)
colnames(dfm)
dfm[1,]
t(dfm)
t(t(dfm))
dfm
as.matrix(dfm)
data.matrix(dfm)
attributes(dfm)


typeof(NULL)
#> [1] "NULL"

length(NULL)
#> [1] 0

x <- NULL
attr(x, "y") <- 1

is.null(NULL)

c()
I()
c(1:3,NULL)

x <- 1:10
nrow(x)
#> NULL
ncol(x)
#> NULL
NROW(x)
NCOL(x)
?NROW

x <- table(mtcars[c("vs", "cyl", "am")])
x
typeof(x)
attributes(x)

# Subsetting atomic vectors
(1:2)[3]
#> [1] NA
(1:2)[NA]
#> [1] NA NA

# Subsetting lists
as.list(1:2)[3]
#> [[1]]
#> NULL
as.list(1:2)[NA]
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL

df <- data.frame(x = 1:5, y = 5:1)
df
t(df)
t(t(df))
is.matrix(df)
#> [1] FALSE
is.matrix(t(df))
#> [1] TRUE
is.matrix(t(t(df)))
#> [1] TRUE

df_coltypes <- data.frame(
  a = c("a", "b"),
  b = c(TRUE, FALSE),
  c = c(1L, 0L),
  d = c(1.5, 2),
  e = c("one" = 1, "two" = 2),
  g = factor(c("f1", "f2")),
  stringsAsFactors = FALSE
)

df_coltypes
class(df_coltypes)
typeof(df_coltypes)
as.matrix(df_coltypes)
class(as.matrix(df_coltypes))
typeof(as.matrix(df_coltypes))

data.matrix(df_coltypes)
class(data.matrix(df_coltypes))
typeof(data.matrix(df_coltypes))




#4 Subsetting
#4.1 Introduction
#4.2 Selecting multiple elements



x <- c(2.1, 4.2, 3.3, 5.4)

order(x)
x[order(x)]
x[c(2.1, 2.9)]
str(x)
summary(x)
x[c(1,1,0,0)]
x[c(T,T,F,F)]
x[c(T,1,F,F)]
x[c(T,T,F,0)]
x[c(TRUE)]
x[c(1)]
x[c(TRUE,NA)]
x[c(NA,1)]

y <- setNames(x, letters[1:4])
y
y[c("d", "c", "a")]

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
df
df[1:2]
df[,1:2]
df$x
df$x == 2
df[df$x == 2, ]
df["x"]
df[, "x"]
str(df["x"])

df2 <- tibble::tibble(x = 1:3, y = 3:1, z = letters[1:3])
df2
str(df["x"])
str(df[,"x"])


#4.3 Selecting a single element
x <- list(1:3, "a", 4:6)
x
class(x[1])
typeof(x[1])
class(x[[1]])
typeof(x[[1]])

for (i in 2:length(x)) {
  out[[i]] <- fun(x[[i]], out[[i - 1]])
}

x[0]
x[T]
x[1][2]
x[[1]]
x[[1]][[2]]
str(x[[1]])
x[[1]][1]
x
x[c(1,2)]
length(x)

mtcars$qsec
mtcars[["cyl"]]
mtcars["cyl"]

x <- list(
  a = list(1, 2, 3),
  b = list(3, 4, 5)
)
purrr::pluck(x, "a", 1)


#4.4 Subsetting and assignment
x <- list(a = 1, b = 2)
str(x[["b"]])
str(x["b"])


#4.5 Applications
info <- data.frame(
  grade = 3:1,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F, F, T)
)
info
info$grade
grades <- c(1, 2, 2, 3, 1)
grades
match(grades, info$grade)
id <- match(grades, info$grade)
id
info[id, ]

df <- data.frame(x = c(1, 2, 3, 1, 2), y = 5:1, z = letters[1:5])
df
nrow(df)
sample(nrow(df))
df[sample(nrow(df)), ]
sample(nrow(df), 3)

x <- c("b", "c", "a")
order(x)

1:10 %% 2


#5 Control flow

Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
if (c(TRUE, FALSE)) 1

x <- -1
y <- if (x) 3
y
switch("x", x = , y = 2, z = 3)

#5.2 Choices
orgpic=readJPEG("pictures/123.jpg")
dim(orgpic)
orgpic[300,400,]
unique(c(orgpic))
hist(orgpic[,,1])
hist(orgpic[,,2])
hist(orgpic[,,3])
neg_pic=1-orgpic
writeJPEG(neg_pic, target = "pictures/neg_Desert.jpg", quality = 0.95)
writeJPEG(orgpic^2, target = "pictures/square_Desert.jpg", quality = 0.95)
writeJPEG(sqrt(orgpic), target = "pictures/sqrt_Desert.jpg", quality = 0.95)


logical()
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
if (c(TRUE, FALSE)) 1
x <- 1:10
ifelse(x %% 2 == 0, "even", "odd")

x_option <- function(x) {
  switch(x,
         a = "option 1",
         b = "option 2",
         c = "option 3",
         stop("Invalid `x` value")
  )
}
x_option("a")
x_option("b")
x_option("d")
ifelse(TRUE, 1, "no")
ifelse(FALSE, 1, "no")
ifelse(NA, 1, "no")
ifelse(1, 1, "no")
ifelse(0, 1, "no")
ifelse(2, 1, "no")
ifelse(-1, 1, "no")
ifelse("a", 1, "no")


#5.3 Loops
for (i in 1:10) {
  if (i < 3) 
    next
  
  print(i)
  
  if (i >= 5)
    break
}
means <- c(1, 50, 20)
out <- vector("list", length(means))
out
for (i in 1:length(means)) {
  out[[i]] <- rnorm(10, means[[i]])
}
out
seq_along(means)

x <- numeric()
x[1]
x[1] ^ 2
out <- vector("list", length(x))
out
1:length(x)
for (i in 1:length(x)) {
  out[i] <- x[i] ^ 2
}
out

#6 Functions.
#6.1 Introduction

f02 <- function(x, y) {
  # A comment
  x + y
}

formals(f02)


body(f02)


environment(f02)

class(f02)
typeof(f02)
str(f02)
attributes(f02)
attr(f02, "srcref")

`+`
attributes(`+`)
attr(`+`, "srcref")
body(`+`)
class(`+`)
typeof(`+`)
formals(`+`)
body(`+`)
environment(`+`)

lapply(mtcars, function(x) length(unique(x)))
lapply(mtcars, length(unique(x)))
?lapply
mtcars[[1]]
mtcars$mpg
sapply(mtcars, function(x) length(unique(x)))
vapply(mtcars, function(x) length(unique(x)), 1)


Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, pi)

function(x) sin(x) ^ 2
typeof(function(x) sin(x) ^ 2)
class(function(x) sin(x) ^ 2)
body(function(x) sin(x) ^ 2)
attributes(function(x) sin(x) ^ 2)



x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}

f1(1)()
f1(1)(1)
f1(1)


`+`(1, `*`(2, 3))
mean(, TRUE, x = c(1:10, NA))
mean(,1,x=c(1:10))
help(mean)
mean(c(1:10))
mean(c(1:10),1)

f2 <- function(a, b) {
  a * 10
}
f2(10, stop("This is an error!"))
f2(10)
f2(10,0)
f2(10,"")
f2(10,"if")
f2(10,break)


#6.2 Function fundamentals
f02 <- function(x, y) {
  # A comment
  x + y
}
formals(f02)
body(f02)
environment(f02)
attr(f02, "srcref")

sum
mean
f02
'['
`[`
'%'
class(sum)
class(`[`)
class(`$`)
class(`+`)
class(`*`)
class(sin)
sin
typeof(`[`)
typeof(sum)
typeof(`+`)
typeof(`-`)
typeof(`%%`)
typeof(`$`)
typeof(sin)

formals(sum)
formals(`[`)
body(sum)
environment(sum)

formals(sin)
lapply(mtcars, function(x) length(unique(x)))
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, pi)
integrate(function(x) return(1), 0, 1)

f2 <- function(x) {return(1)}
typeof(f2(2))


args <- list(1:10, na.rm = TRUE)
args
do.call(mean, args)

function(x) 3()
function(x) 3
(function(x) 3)()

objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
objs
funs <- Filter(is.function, objs)
funs

#6.3 Function composition

x <- runif(100)
deviation <- function(x) x - mean(x)
deviation
square <- function(x) x^2
x %>%
  deviation() %>%
  square() %>%
  mean() %>%
  sqrt()

#6.4 Lexical scoping
x <- 10
g01 <- function() {
  x <- 20
  x
}

g01()
x

x <- 1
g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}

g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}

g04()

x
y
z

rm(list = c("a"))
exists("a")
a <- 1
g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g11()
g11()
a

g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
}
g11()


g12 <- function() x + 1
codetools::findGlobals(g12)
x
environment(g12) <- emptyenv()
g12()

c <- 10
c(c = c)
c(c <- c)
c

f <- function(x) {
  f <- function(x) {
    f <- function() {
      x ^ 2
    }
    f() + 1
  }
  f(x) * 2
}
f(10)


#6.5 Lazy evaluation
h01 <- function(x) {
  10
}
h01(9)
h01(stop("This is an error!"))
?stop
stop("This is an error!")

y <- 10
h02 <- function(x) {
  y <- 100
  x + 1
}
h02(y)
h02(y <- 1000)
y

x

x <- 20
double <- function(x) { 
  message("Calculating...")
  x * 2
}
double(x)
h03 <- function(x) {
  c(x, x, x)
}
h03(double(x))


h05 <- function(x = ls()) {
  a <- 1
  x
}
h05()
h05(ls())
ls()
x

missing(x)
list(missing(x), x)
h06 <- function(x = 10) {
  list(missing(x), x)
}
h06()
x
str(h06())
h06(10)
h06(11)
str(h06(10))

sample(10)
sample(1:100, 10)
args(sample)

sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  if (is.null(size)) {
    size <- length(x)
  }
  
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}
sample
`%||%`


`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}  
`%||%`
sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  size <- size %||% length(x)
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}

`&`
`&&`
help(`&`)

c(T,T,F) & c(T,T,T)
c(T,T,F) && c(T,T,T)
c(F,T,F) & c(F,T,T)
c(F,T,F) && c(F,T,T)

f2 <- function(x = z) {
  z <- 100
  x
}
f2()
f2(1000)

{y <- 1; 2}


x
y
b
d
b <- 10
d <- 100
f2 <- function(b =1, d = 2)
{
  c(b,d)
}
f2(1,3)
f2()
b
d

x = {y <- 1; 2}
x


y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  c(x, y)
}
f1()
y

x = stop("Error!")
Sys.time()
show_time <- function(x = stop("Error!")) {
  stop <- function(...) Sys.time()
  x
}
show_time()

show_time <- function(x = stop(" ")) {
  stop <- function(.) Sys.time()
  x
}
show_time()

show_time <- function(x = stop(" ")) {
  op <- function(...) Sys.time()
  x
}
show_time()


show_time <- function(x = stop(" ")) {
  function(...) Sys.time()
  x
}
show_time()


stop()
stop(" ")
stop("Error!")

show_time <- function(x = stop()) {
  function(...) Sys.time()
  x
}
show_time()

help(stop)

show_time <- function(x = stop()) {
  function(...) Sys.time()
  x
}
show_time()

stop("a")


#6.6 ... (dot-dot-dot)

i03 <- function(...) {
  list(first = ..1, third = ..3)
}
i03(1, 2, 3)
str(i03(1, 2, 3))

i03 <- function(...) {
  list(..2, ..1, ..3)
}
i03(6,5,2)

i03 <- function(...) {
  c(..2, ..1, ..3)
}
i03(6,5,2)

i04 <- function(...) {
  list(...)
}
str(i04(a = 1, b = 2))
i04(2, 5)

i01 <- function(y, z) {
  list(y = y, z = z)
}

i02 <- function(x, ...) {
  i01(...)
}

i02(x = 1, y = 2, z = 3)
str(i02(x = 1, y = 2, z = 3))

x <- list(c(1, 3, NA), c(4, NA, 6))
x
lapply(x, mean, na.rm = TRUE)

print(factor(letters), max.levels = 4)
print(y ~ x, showEnv = TRUE)

mean(1:5)
mean(1:5, 0.5)
help(mean)

cat("Hello\n")

j06 <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)
  
  if (x) {
    return(10)
  } else {
    stop("Error")
  }
}

j06(TRUE)
j06(F)

x <- c(0:10, 50)
xm <- mean(x)
xm

sum(1, 2, 3, na.omit = TRUE)
sum(1, 2, 3, na.rm = TRUE)
?na.omit

na.omit = TRUE
na.omit

sum(1, 2, 3, omit = TRUE)

plot(1:10, col = "red", pch = 20, xlab = "x", col.lab = "blue")

plot.default(1:10)
plot.default


#6.7 Exiting a function
j03 <- function() 1
j03()

j04 <- function() invisible(1)
j04()
print(j04())
(j04())

withVisible(j04())

j05 <- function() {
  stop("I'm an error")
  return(10)
}
j05()


j06 <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)
  
  if (x) {
    return(10)
  } else {
    stop("Error")
  }
}

j06(TRUE)
j06(FALSE)


message("a")
?on.exit
on.exit(message("a"), add = TRUE)


j08 <- function() {
  on.exit(message("a"), add = TRUE)
  on.exit(message("b"), add = TRUE)
}
j08()


#6.8 Function forms
for(i in 1:10) print(i)
`for`(i, 1:10, print(i))

x + y
`+`(x, y)

runif(1)
runif(1)

replicate(50, (1 + 2))
sapply(1:5, `+`, 3)

install.packages("rticles")
library(rticles)

k01 <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}
k01(1, 2, 3)
k01(2, 3, abcdef = 5)
k01(2, 3, a = 1)
k01(2, 3, a = 2)
k01(1, 3, bcde2 = 1)

options(warnPartialMatchArgs = TRUE)
k01(a = 1, 2, 3)

paste(a, b)

-1
+10
`for`

runif(min = 0, max = 1, 20)
x <- sample(replace = TRUE, 20, x = c(1:10, NA))
y <- runif(min = 0, max = 1, 20)
cor(m = "k", y = y, u = "p", x = x)

get("x")
x
modify(get("x"), 1)

1 + 2
"a" + "b"


#7 Environments
#7.2 Environment basics

current_env()
global_env()
identical(global_env(), current_env())
global_env() == current_env()
globalenv()
environment()

empty_env()
e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)
e2c
e2d
env_parents(e2d)
env_parents(e2c)
env_parents(empty_env)


#7.3 Recursing over environments
where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}
where("yyy")
x <- 5
where("x")
where("mean")

e4a <- env(empty_env(), a = 1, b = 2)
e4a
e4b <- env(e4a, x = 10, a = 11)
e4b
where("a", e4b)
where("b", e4b)
where("c", e4b)

#7.4 Special environments
c(1,2,3)

search()
search_envs()
base_env()

y <- 1
f <- function(x) x + y
fn_env(f)
environment(f)
environment(mean)

env()
e <- env()
e$g <- function() 1
search()
search_envs()

sd

g <- function(x) {
  if (!env_has(current_env(), "a")) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
g(10)
g(10)

h2 <- function(x) {
  a <- x * 2
  current_env()
}

e <- h2(x = 10)
env_print(e)
fn_env(h2)

plus <- function(x) {
  function(y) x + y
}

plus_one <- plus(1)
plus_one
plus_one(2)
plus_one(1)
plus(1)(1)


#7.5 Call stacks
caller_env()
f <- function(x) {
  g(x = 2)
}
g <- function(x) {
  h(x = 3)
}
h <- function(x) {
  stop()
}
f(x = 1)
traceback()
lobstr::cst()

h <- function(x) {
  lobstr::cst()
}
f(x = 1)
f()

a <- function(x) b(x)
b <- function(x) c(x)
c <- function(x) x
a(f())

my_env <- new.env(parent = emptyenv())
my_env$a <- 1

my_env$a
get_a <- function() {
  my_env$a
}
set_a <- function(value) {
  old <- my_env$a
  my_env$a <- value
  invisible(old)
}


#8 Conditions
#8.1 Introduction
library(rlang)
#8.2 Signalling conditions
stop("This is what an error looks like")
warning("This is what a warning looks like")
message("This is what a message looks like")
Sys.time()

f <- function() g()
g <- function() h()
h <- function() stop("This is an error!")

f()
g()
h()
stop("This is an error!")
h <- function() stop("This is an error!", call. = FALSE)
f()
abort("This is an error!")

h <- function() abort("This is an error!")
f()
fw <- function() {
  cat("1\n")
  warning("W1")
  cat("2\n")
  warning("W2")
  cat("3\n")
  warning("W3")
}
fw()


#9 Functionals
triple <- function(x) x * 3
purrr::map(1:3, triple)
lapply(1:3, triple)

x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

# Select by name
map_dbl(x, "x")
#> [1] 1 4 8

# Or by position
map_dbl(x, 1)
#> [1] -1 -2 -3
map_dbl(x, 2)
# Or by both
map(x, "y")
map_dbl(x, list("y", 1))
#> [1] 2 5 9

# You'll get an error if a component doesn't exist:
map_chr(x, "z")
#> Error: Result 3 must be a single string, not NULL of length 0

# Unless you supply a .default value
map_chr(x, "z", .default = NA)
#> [1] "a" "b" NA

x <- list(1:5, c(1:10, NA))
lapply(x, mean, na.rm = TRUE)
map(x, mean, na.rm = TRUE)

map_dbl(x, ~ mean(.x, na.rm = TRUE))
#> [1] 3.0 5.5

x <- list(1:5, c(1:10))
x
map(x, mean)
map(x, "mean")
lapply(x, "mean")
lapply(x, mean)

map_dbl(mtcars, ~ length(unique(.x)))
lapply(mtcars, ~ length(unique(.x)))

`~`
as_mapper(~ length(unique(.x)))
#> <lambda>
#> function (..., .x = ..1, .y = ..2, . = ..1) 
#> length(unique(.x))
#> attr(,"class")
#> [1] "rlang_lambda_function" "function"

map(mtcars, c(unique, length))
lapply(mtcars, c(unique, length))

#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>   25    3   27   22   22   29   30    2    2    3    6

trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
?rcauchy
x

map_dbl(trims, function(trim) mean(x, trim = trim))
#> [1] -0.3500  0.0434  0.0354  0.0502

by_cyl <- split(mtcars, mtcars$cyl)
by_cyl
mtcars$cyl
as.factor(mtcars$cyl)

?split
?map
as.list(mtcars)
split(mtcars, cyl)
map(mtcars, split, f = as.factor(mtcars$cyl))
map(mtcars, ~ split(.x, f = mtcars$cyl))

by_cyl %>% 
  map(~ lm(mpg ~ wt, data = .x))

?coef
by_cyl %>% 
  map(~ lm(mpg ~ wt, data = .x)) %>% 
  map(coef)

by_cyl %>% 
  map(~ lm(mpg ~ wt, data = .x)) %>% 
  map(coef) %>% 
  map_dbl(2)
#>     4     6     8 
#> -5.65 -2.78 -2.19

by_cyl %>% 
  lapply(function(data) lm(mpg ~ wt, data = data)) %>% 
  lapply(coef) %>% 
  vapply(function(x) x[[2]], double(1))
#>     4     6     8 
#> -5.65 -2.78 -2.19

?vapply
?double

df <- data.frame(
  x = 1:3,
  y = 6:4
)

map(df, ~ .x * 2)
#> $x
#> [1] 2 4 6
#> 
#> $y
#> [1] 12 10  8


xs <- map(1:8, ~ runif(10))
xs
xs[[1]][[1]] <- NA
xs
?rpois
ws <- map(1:8, ~ rpois(10, 5) + 1)
ws

map_dbl(xs, mean)
#> [1]    NA 0.463 0.551 0.453 0.564 0.501 0.371 0.443

weighted.mean(xs[[2]], w = ws[[2]])
weighted.mean(xs[[3]], w = ws[[3]])

map_dbl(xs, weighted.mean, w = ws)
#> Error in weighted.mean.default(.x[[i]], ...): 'x' and 'w' must have the
#> same length
map2_dbl(xs, ws, weighted.mean)
#> [1]    NA 0.451 0.603 0.452 0.563 0.510 0.342 0.464


welcome <- function(x) {
  cat("Welcome ", x, "!\n", sep = "")
}
names <- c("Hadley", "Jenny")

# As well as generate the welcomes, it also shows 
# the return value of cat()
map(names, welcome)
#> Welcome Hadley!
#> Welcome Jenny!
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL

walk(names, welcome)
#> Welcome Hadley!
#> Welcome Jenny!

params <- tibble::tribble(
  ~ n, ~ min, ~ max,
  1L,     0,     1,
  2L,    10,   100,
  3L,   100,  1000
)
params
pmap(params, runif)
#> [[1]]
#> [1] 0.332
#> 
#> [[2]]
#> [1] 53.5 47.6
#> 
#> [[3]]
#> [1] 231 715 515
sample(1:10, 15, replace = T)
l <- map(1:4, ~ sample(1:10, 15, replace = T))
str(l)
l
#> List of 4
#>  $ : int [1:15] 7 1 8 8 3 8 2 4 7 10 ...
#>  $ : int [1:15] 3 1 10 2 5 2 9 8 5 4 ...
#>  $ : int [1:15] 6 10 9 5 6 7 8 6 10 8 ...
#>  $ : int [1:15] 9 8 6 4 4 5 2 9 9 6 ...

out <- l[[1]]
out
out <- intersect(out, l[[2]])
out
out <- intersect(out, l[[3]])
out
out <- intersect(out, l[[4]])
out
#> [1] 8 4

reduce(l, intersect)
#> [1] 8 4



#10 Function factories

power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

square <- power1(2)
square(8)
cube <- power1(3)
?env_print
env_print(square)

env_print(cube)
?fn_env
fn_env(square)$exp
#> [1] 2
fn_env(cube)$exp
#> [1] 3

df <- data.frame(x = 1, y = y)
df
y
core <- ggplot(df, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = 1, labels = NULL) +
  labs(x = NULL, y = NULL)

core
core + scale_y_continuous(
  labels = comma_format()
)
core + scale_y_continuous(
  labels = number_format(scale = 1e-3, suffix = " K")
)
core + scale_y_continuous(
  labels = scientific_format()
)

# construct some sample data with very different numbers in each cell
sd <- c(1, 5, 15)
n <- 100

?rnorm
rnorm(300, sd)
df <- data.frame(x = rnorm(3 * n, sd = sd), sd = rep(sd, n))
df
ggplot(df, aes(x)) + 
  geom_histogram(binwidth = 2) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)


binwidth_bins <- function(n) {
  force(n)
  
  function(x) {
    (max(x) - min(x)) / n
  }
}

ggplot(df, aes(x)) + 
  geom_histogram(binwidth = binwidth_bins(20)) + 
  facet_wrap(~ sd, scales = "free_x") + 
  labs(x = NULL)

?stopifnot
boxcox1 <- function(x, lambda) {
  stopifnot(length(lambda) == 1)
  
  if (lambda == 0) {
    log(x)
  } else {
    (x ^ lambda - 1) / lambda
  }
}

boxcox1 <- function(x, lambda) {
  stopifnot(length(lambda) == 1)
  
  if (lambda == 0) {
    log(x)
  } else {
    (x ^ lambda - 1) / lambda
  }
}

boxcox1(2, 4)
(2^4-1)/4

boxcox2 <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}

stat_boxcox <- function(lambda) {
  stat_function(aes(colour = lambda), fun = boxcox2(lambda), size = 1)
}

ggplot(data.frame(x = c(0, 5)), aes(x)) + 
  lapply(c(0.5, 1, 1.5), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))

# visually, log() does seem to make sense as the transformation
# for lambda = 0; as values get smaller and smaller, the function
# gets close and closer to a log transformation
ggplot(data.frame(x = c(0.01, 1)), aes(x)) + 
  lapply(c(0.5, 0.25, 0.1, 0), stat_boxcox) + 
  scale_colour_viridis_c(limits = c(0, 1.5))

names <- list(
  square = 2, 
  cube = 3, 
  root = 1/2, 
  cuberoot = 1/3, 
  reciprocal = -1
)
funs <- purrr::map(names, power1)

funs$root(64)
#> [1] 8
funs$root
#> function(x) {
#>     x ^ exp
#>   }
#> <bytecode: 0x1149b68>
#> <environment: 0x5369e98>
funs
with(funs, root(100))
#> [1] 10
attach(funs)
#> The following objects are masked _by_ .GlobalEnv:
#> 
#>     cube, square
root(100)
#> [1] 10
detach(funs)


#11 Function operators
#A function operator is a function that takes one (or more) functions as input and returns a function as output
chatty <- function(f) {
  force(f)
  
  function(x, ...) {
    res <- f(x, ...)
    cat("Processing ", x, "\n", sep = "")
    res
  }
}
f <- function(x) x ^ 2
s <- c(3, 2, 1)
chatty(f)(s)
purrr::map_dbl(s, chatty(f))
#> Processing 3
#> Processing 2
#> Processing 1
#> [1] 9 4 1


x <- list(
  c(0.512, 0.165, 0.717),
  c(0.064, 0.781, 0.427),
  c(0.890, 0.785, 0.495),
  "oops"
)

x
out <- rep(NA_real_, length(x))
out
seq_along(x)
for (i in seq_along(x)) {
  out[[i]] <- sum(x[[i]])
}
#> Error in sum(x[[i]]): invalid 'type' (character) of argument
out
#> [1] 1.39 1.27 2.17   NA
map_dbl(x, sum)
#> Error in .Primitive("sum")(..., na.rm = na.rm): invalid 'type' (character)
#> of argument
safe_sum <- safely(sum)
safe_sum
#> function (...) 
#> capture_error(.f(...), otherwise, quiet)
#> <bytecode: 0x4ea66d8>
#> <environment: 0x4eaa070>
safe_sum(x[[1]])
str(safe_sum(x[[1]]))
safely(sum(x[[1]]))
safely(sum)(x[[1]])

#> List of 2
#>  $ result: num 1.39
#>  $ error : NULL
safe_sum(x[[4]])
str(safe_sum(x[[4]]))
#> List of 2
#>  $ result: NULL
#>  $ error :List of 2
#>   ..$ message: chr "invalid 'type' (character) of argument"
#>   ..$ call   : language .Primitive("sum")(..., na.rm = na.rm)

#>   ..- attr(*, "class")= chr [1:3] "simpleError" "error" "condition"

diamonds <- ggplot2::diamonds

summary(diamonds$carat)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    0.20    0.40    0.70    0.80    1.04    5.01

summary(diamonds$cut)
#>      Fair      Good Very Good   Premium     Ideal 
#>      1610      4906     12082     13791     21551

#12 Base types
#13 S3

mydf <- c("up", "down", "left", "right", "front", "back","up", "down", "left", "right", "front", "back")
mydf <- factor(mydf, levels = c("up", "down", "left", "right", "front", "back"))
mydf
mydf2 <- factor(mydf, levels = c( "down", "left", "right", "front", "back", "up"))
mydf2

f <- factor(c("a", "b", "c"))

typeof(f)
#> [1] "integer"
attributes(f)
#> $levels
#> [1] "a" "b" "c"
#> 
#> $class
#> [1] "factor"


#8 Conditions
library(rlang)
stop("This is what an error looks like")
#> Error in eval(expr, envir, enclos): This is what an error looks like

warning("This is what a warning looks like")
#> Warning: This is what a warning looks like

message("This is what a message looks like")
#> This is what a message looks like

f <- function() g()
g <- function() h()
h <- function() stop("This is an error!")

f()
#> Error in h(): This is an error!
traceback()


h <- function() stop("This is an error!", call. = FALSE)
f()
#> Error: This is an error!

h <- function() abort("This is an error!")
f()
rlang::last_error()



fw <- function() {
  cat("1\n")
  warning("W1")
  cat("2\n")
  warning("W2")
  cat("3\n")
  warning("W3")
}
fw()


#9 Functionals
#9.1 Introduction
randomise <- function(f) f(runif(1e3))
randomise(mean)
#> [1] 0.506
randomise(mean)
#> [1] 0.501
randomise(sum)
#> [1] 489

library(purrr)
character(2)

map_chr(mtcars, typeof)
lapply(mtcars, typeof)
vapply(mtcars, typeof, FUN.VALUE=character(1))

list(mtcars)
map_dbl(mtcars, "Mazda RX4")

x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)

# Select by name
map_dbl(x, "x")
#> [1] 1 4 8

# Or by position
map_dbl(x, 1)
#> [1] -1 -2 -3

# Or by both
map_dbl(x, list("y", 1))
#> [1] 2 5 9

# You'll get an error if a component doesn't exist:
map_chr(x, "z")
#> Error: Result 3 must be a single string, not NULL of length 0

# Unless you supply a .default value
map_chr(x, "z", .default = NA)

df <- data.frame(
  x = 1:3,
  y = 6:4
)

map(df, ~ .x * 2)
#> $x
#> [1] 2 4 6
#> 
#> $y
#> [1] 12 10  8
#> [1] "a" "b" NA
#> 
?modify
modify(df, ~ .x * 2)
#>   x  y
#> 1 2 12
#> 2 4 10
#> 3 6  8
#> 
runif(10)
xs <- map(1:8, ~ runif(10))
xs
xs[[1]][[1]] <- NA
ws <- map(1:8, ~ rpois(10, 5) + 1)
ws

sample(1:10, 15, replace = TRUE)


l <- map(1:4, ~ sample(1:10, 15, replace = T))
str(l)

out <- l[[1]]
out
out <- intersect(out, l[[2]])
out
out <- intersect(out, l[[3]])
out
out <- intersect(out, l[[4]])
out

reduce(l, intersect)
unlist(l)
reduce(unlist(l))

?reduce


#10 Function factories
#10.1 Introduction


f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {
  if (!is.numeric(d)) {
    stop("`d` must be numeric", call. = FALSE)
  }
  d + 10
}
f("a")