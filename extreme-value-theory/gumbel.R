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
