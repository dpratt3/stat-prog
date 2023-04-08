### Algorithm 8.1 Implementation
options(digits = 15)
### Prerequisite code from our previous post, "Is It Normal"
# Make sure to install.packages("pacman") if it isn't already available
pacman::p_load("ggplot2", # pacman::p_load is an awesome 2-for-1 substitute for base R install.packages() and library()
               "RColorBrewer",
               "extrafont",
               "gridExtra") # With pacman, you can avoid errors due to library discrepancies between environments!

### custom function returning dnorm equivalent of Gaussian Mixture
gaussian_mixture <- function(x, mean1, mean2, sd1, sd2){
  coeff1 = 1/sqrt(2*pi * sd1^2)
  coeff2 = 1/sqrt(2*pi * sd2^2)
  freq = coeff1 * exp( -((x - mean1)^2) / (2*sd1^2)) + coeff2 * exp(-(x - mean2)^2 / (2*sd2^2))
  return(freq)
}

### Define standard-normal random variables
set.seed(7654) # keep this seed
sample_size = 10^4
dist1 = rnorm(10^4, 40, 20) # 40:20 (mean-sd)
dist2 = rnorm(10^4, 50, 7)  # 50:7 (mean-sd)
df1 <- data.frame(flip = factor(rep(c("H", "T"), each = sample_size)), value = round(c(dist1, dist2))) # data set under consideration

### Add the distributions as opposed to adding the random variables
gauss_mix <- ggplot(df1, aes(x = value, color = flip, fill = flip)) +
  geom_histogram(aes(y = ..density..), position = "stack", alpha = 0.8, binwidth = 2) +
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

### Step 1: Take initial guesses
data = df1$value # look at distribution without head/tails breakdown

EM = function(data){
  set.seed(1234) # make function idempotent
  inital_mean_index = sample(1:length(data), 2)
  mean1 = data[inital_mean_index[[1]]]
  mean2 = data[inital_mean_index[[2]]]
  mean1
  mean2
  pi_hat = 0.5 # the mixing probability
  sd1 = sd(data)
  sd2 = sd(data)

  mean1_list = list()
  mean2_list = list()
  sd1_list = list()
  sd2_list = list()

  # Step 2: Expectation step
  rep = 1
  repeat{
    coeff1 = 1/sqrt(2*pi * sd1^2)
    coeff2 = 1/sqrt(2*pi * sd2^2)
    freq1 = function(x) coeff1 * exp(-((x - mean1)^2 ) / (2*sd1^2))
    freq2 = function(x) coeff2 * exp(-(x - mean2)^2 / (2*sd2^2))
    freq1(mean1)
    freq2(mean2)
    gamma_hat = (pi_hat * freq2(data) / ( (1 - pi_hat) * freq1(data) + pi_hat * freq2(data) ))

    ### Step 3: Maximization step
    mean_hat_1 = sum((1 - gamma_hat)*data)/sum(1-gamma_hat)
    mean_hat_2 = sum((gamma_hat)*data)/sum(gamma_hat)
    sd_hat_1 = sqrt(sum((1 - gamma_hat)*((data - mean1)^2))/sum(1-gamma_hat))
    sd_hat_2 =  sqrt(sum((gamma_hat)*((data - mean2)^2))/sum(gamma_hat))
    pi_hat = mean(gamma_hat)

    ### reset parameters and prepare to repeat
    mean1 = mean_hat_1
    mean2 = mean_hat_2
    sd1 = sd_hat_1
    sd2 = sd_hat_2

    ### monitor convergence
    mean1_list[[rep]] = mean1
    mean2_list[[rep]] = mean2
    sd1_list[[rep]] = sd1
    sd2_list[[rep]] = sd2

    error = 10^-8
    if(rep > 1 &&
       rep <= 1000 && # cap for repetitions in case of non-convergence
       abs(mean1_list[[rep]] - mean1_list[[rep-1]]) < error &&
       abs(mean2_list[[rep]] - mean2_list[[rep-1]]) < error &&
       abs(sd1_list[[rep]] - sd1_list[[rep-1]]) < error &&
       abs(sd2_list[[rep]] - sd2_list[[rep-1]]) < error)
    {
      break
    }
    rep = rep + 1
  }
  ### Monitor convergence
  mean1_list = do.call(rbind, mean1_list)
  mean2_list = do.call(rbind, mean2_list)
  sd1_list = do.call(rbind, sd1_list)
  sd2_list = do.call(rbind, sd2_list)
  parameters = cbind.data.frame(mean1_list, mean2_list, sd1_list, sd2_list)

  return(list(mean1_list, mean2_list, sd1_list, sd2_list, parameters))
}

variables = EM(data)

### Visualize the convergence of all parameters
param_mix1 <- ggplot(variables[[5]], aes(x = c(1:length(variables[[1]])))) +
  geom_line(stat = "identity", aes(y = variables[[1]]), col = "#0CB1B9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  ylab("Mean 1") +
  xlab("Step")

param_mix2 <- ggplot(variables[[5]], aes(x = c(1:length(variables[[2]])))) +
  geom_line(stat = "identity", aes(y = variables[[2]]), col = "#2C3E50") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  ylab("Mean 2") +
  xlab("Step")

param_mix3 <- ggplot(variables[[5]], aes(x = c(1:length(variables[[3]])))) +
  geom_line(stat = "identity", aes(y = variables[[3]]), col = "#0CB1B9") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  ylab("Standard Deviation 1") +
  xlab("Step")

param_mix4 <- ggplot(variables[[5]], aes(x = c(1:length(variables[[4]])))) +
  geom_line(stat = "identity", aes(y = variables[[4]]), col = "#2C3E50") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  ylab("Standard Deviation 2") +
  xlab("Step")

grid.arrange(param_mix1, param_mix2, param_mix3, param_mix4, ncol = 2, top = "Stepwise Parameter Convergence")

### Compare the competition: mixtools
pacman::p_load("mixtools",
               "microbenchmark") # pacman is your friend, for simple and reliable portability.
variables = EM(data)
mean1 = as.numeric(tail(variables[[1]], 1))
mean2 = as.numeric(tail(variables[[2]], 1))
sd1 = as.numeric(tail(variables[[3]], 1))
sd2 = as.numeric(tail(variables[[4]], 1))
competition = normalmixEM(data, ECM = F)
### Disclaimer: microbenchmark iterates many times and these example distributions are relatively large; running this benchmark is disabled by default for brevity.
# microbenchmark(normalmixEM(data, ECM = F))
# microbenchmark(EM(data))
round(sort(competition$sigma) - sort(c(sd1, sd2)), 5)
round(sort(competition$mu) - sort(c(mean1, mean2)), 5)

### Superimposed equations use parameters computed by EM Algorithm
df1 <- data.frame(flip = factor(rep(c("H", "T"), each = sample_size)), value = round(c(dist1, dist2)))

ind_dist <- ggplot(df1, aes(x = value, color = flip, fill = flip)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.8, binwidth = 2) +
  scale_color_manual(values = c("#000000", "#000000", "#999999")) +
  scale_fill_manual(values = c("#2C3E50", "#0CB1B9", "#000000")) +
  labs(title = "Individual Distributions", x = "", y = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size = 10)) +
  theme(legend.position = "none") +
  stat_function(fun = dnorm, args = list(mean = mean1, sd = sd1)) +
  stat_function(fun = dnorm, args = list(mean = mean2, sd = sd2))

ind_dist


