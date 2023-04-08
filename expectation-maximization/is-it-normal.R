# https://blog.midnightmechanism.com/post/is-it-normal/

pacman::p_load("ggplot2",
               "RColorBrewer",
               "extrafont")
set.seed(1234)

### Define standard-normal random variables
sample_size = 10^5
dist1 = rnorm(sample_size, mean = 50, sd = 5)
dist2 = rnorm(sample_size, mean = 65, sd = 5)
dist3 = dist1 + dist2 # arithemtic sum of random variables

### Separate distributions: designated by position = "identity"
df1 <- data.frame(flip = factor(rep(c("H", "T"), each = sample_size)), value = round(c(dist1, dist2)))

ind_dist <- ggplot(df1, aes(x = value, color = flip, fill = flip)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.8, binwidth = 1) +
  scale_color_manual(values = c("#000000", "#000000", "#999999")) +
  scale_fill_manual(values = c("#2C3E50", "#0CB1B9", "#000000")) +
  labs(title = "Individual Distributions", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=10)) +
  theme(legend.position = "none") +
  stat_function(fun = dnorm, args = list(mean = mean(dist1), sd = sd(dist1))) +
  stat_function(fun = dnorm, args = list(mean = mean(dist2), sd = sd(dist2)))

ind_dist

### Summation of Random Variables with color interpolation
df2 <- data.frame(flip = factor(rep(c("HT"), 2*sample_size)), value = round(c(dist3)))
blend <- colorRampPalette(c("#2C3E50", "#0CB1B9"), interpolate = "spline")

arithmetic_sum <- ggplot(df2, aes(x = value, color = flip, fill = flip)) +
  geom_histogram(aes(y = ..density..), position = "stack", alpha = 0.8, binwidth = 1) +
  scale_color_manual(values = c("#000000", "#000000", "#999999")) +
  scale_fill_manual(values = blend(17)[[7]]) +
  labs(title = "Arithmetic Sum of Random Variables", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size = 10)) +
  theme(legend.position = "none") +
  stat_function(fun = dnorm, args = list(mean = mean(df2$value), sd = sd(df2$value)))

arithmetic_sum

### custom function returning dnorm equivalent of Gaussian Mixture
gaussian_mixture <- function(x, mean1, mean2, sd1, sd2){
  coeff1 = 1/sqrt(2*pi * sd1^2)
  coeff2 = 1/sqrt(2*pi * sd2^2)
  freq = coeff1 * exp( -((x - mean1)^2) / (2*sd1^2)) + coeff2 * exp(-(x - mean2)^2 / (2*sd2^2))
  return(freq)
}

### Add the distributions as opposed to adding the random variables
gauss_mix <- ggplot(df1, aes(x = value, color = flip, fill = flip)) +
  geom_histogram(aes(y = ..density..), position = "stack", alpha = 0.8, binwidth = 1) +
  scale_color_manual(values = c("#000000", "#000000", "#999999")) +
  scale_fill_manual(values = c("#2C3E50", "#0CB1B9", "#000000")) +
  labs(title = "Gaussian Mixture: Sum of Distributions", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  stat_function(fun = gaussian_mixture, args = list(mean1 = mean(dist1),
                                                    sd1 = sd(dist1),
                                                    mean2 = mean(dist2),
                                                    sd2 = sd(dist2)))
gauss_mix

