set.seed(540)

n <- 10
B <- 4

x <- matrix(rnorm(n * B), nrow = n)
rownames(x) <- sprintf("obs%02d", 1:n)
colnames(x) <- sprintf("samp%02d", 1:B)
x

apply(x,2,mean)

B <- 1000
n <- 10^(1:4)
names(n) <- paste0("n", n)
getSampleMeans <- function(n, B) colMeans(matrix(rnorm(n * B), nrow = n))
x <- sapply(n, getSampleMeans, B, simplify = FALSE)
cbind(sampSize = n,
      trueSEM = 1 / sqrt(n),
      obsSEM = sapply(x, sd),
      sampMeanIQR = sapply(x, IQR),
      sampMeanMad = sapply(x, mad))

pnorm(3,5,2)
rnorm(500,5,2)
mean(rnorm(50,5,2)<=3)
# The two numbers tend to be fairly close

mean(rnorm(5000,5,2)<=3)
# With larger n's, the numbers tend to be closer

#Numbers above a threshold:
pnorm(3,5,2, lower.tail = FALSE)
mean(rnorm(5000,5,2)>=3)

#Probability of observed values falling in an interval
pnorm(2,5,2)-pnorm(1,5,2)
a <- rnorm(5000,5,2)
mean(a <= 2) - mean(a <= 1)

# Exploring the distribution of sample means and the CLT
library(lattice)

## theoretical vs. empirical distriubution for a single sample
## demo of the Central Limit Theorem
n <- 200
x  <- rnorm(n)

densityplot(~x, n = 200, ylim = dnorm(0) * c(-0.1, 1.15),
            panel = function(x, ...) {
              panel.densityplot(x, ...)
              panel.mathdensity(n = 200, col.line = "grey74")
            })


## empirical distribution of sample means for various sample sizes
B <- 1000
n <- round(10^(seq(from = 1, to = 2.5, length = 4)), 0)
names(n) <- paste0("n", n)
getSampleMeans <- function(n, B) colMeans(matrix(rnorm(n * B), nrow = n))
x <- data.frame(sapply(n, getSampleMeans, B))
## using the "extended formula interface" in lattice
jFormula <- as.formula(paste("~", paste(names(n), sep = "", collapse = " + ")))
## building the formula programmatically is slicker than the alternative, which
## is hard wiring to "~ n10 + n32 + n100 + n316", which is not a crime
densityplot(jFormula, x, xlab = "sample means",
            auto.key = list(x = 0.9, y = 0.9, corner = c(1, 1),
                            reverse.rows = TRUE))


## keeping the data "tidy", i.e. tall and skinny, for a happier life
xTallSkinny <- stack(x)
names(xTallSkinny) <- c("x","n")
xTallSkinny$n <- factor(xTallSkinny$n, levels = colnames(x))
densityplot(~ x, xTallSkinny, xlab = "sample means", groups = n,
            auto.key = list(x = 0.9, y = 0.9, corner = c(1, 1),
                            reverse.rows = TRUE))
