# Blog post on this topic: https://blog.midnightmechanism.com/post/in-cauda-venenum/

library(fitdistrplus)
library(extRemes)
library(evd)

### Case 1: Gumbel
E <- list()
for(i in 1:10^4){ E[[i]] = max(rnorm(3*10^4, mean = 0, sd = 1)); print(i)}
E <- do.call(rbind, E)
E <- as.numeric(E)
h <- hist(E, breaks = 30, ylim = c(0, 1800), col = "#0CB1B9")
params <- fitdist(E, "gumbel", method = "mse", lower = c(0, 0, 0), start = list(loc=0.45, scale=36), keepdata = T)
xfit <- seq(min(E), max(E), length = 500)
yfit <- dgumbel(xfit, loc = params$estimate[[1]], scale = params$estimate[[2]])
yfit <- yfit * diff(h$mids[1:2])*length(E)
lines(xfit, yfit, col="blue", lwd=2)

### Case 2: Fréchet
G <- list()
for(i in 1:10^4){ G[[i]] = max(rt(10^3, df = 3, ncp = 3)); print(i)}
G <- do.call(rbind, G)
G <- as.numeric(G)
h <- hist(G, breaks = 300, ylim = c(0, 1800), xlim = c(0, 600), col = "#0CB1B9")
params <- fitdist(G, "frechet", method = "mse", lower = c(0, 0, 0), start = list(loc=0.45, scale=36, shape=3), keepdata = T)
xfit <- seq(min(G), max(G), length = 500)
yfit <- dfrechet(xfit, loc = params$estimate[[1]], scale = params$estimate[[2]], shape = params$estimate[[3]])
yfit <- yfit * diff(h$mids[1:2]) * length(G)
lines(xfit, yfit, col="blue", lwd=2)

### Create the final chart
loc1 = params$estimate[[1]]
scale1 = params$estimate[[2]] # Beta in Taleb's paper
shape1 = params$estimate[[3]]
shape1

shape1

alphas = seq(1,5, by = 0.25)

three_beta = list()

ten_beta = list()
twenty_beta = list()

for(i in 1:length(alphas)){
  three_beta[[i]] = integrate(dfrechet, lower = 3 * scale1, upper = Inf, loc = loc1, scale = scale1, shape = alphas[[i]])$value ### Scale is the exponent
  ten_beta[[i]] = integrate(dfrechet, lower = 10 * scale1, upper = Inf, loc = loc1, scale = scale1, shape = alphas[[i]])$value ### Scale is the exponent
  twenty_beta[[i]] = integrate(dfrechet, lower = 20 * scale1, upper = Inf, loc = loc1, scale = scale1, shape = alphas[[i]])$value ### Scale is the exponent
  print(i)
}

three_beta = do.call(rbind, three_beta)
ten_beta = do.call(rbind, ten_beta)
twenty_beta = do.call(rbind, twenty_beta)

three_beta_inv = 1 / three_beta
ten_beta_inv = 1 / ten_beta
twenty_beta_inv = 1/ twenty_beta

final_chart = cbind.data.frame(alphas, three_beta_inv, ten_beta_inv, twenty_beta_inv)
final_chart
