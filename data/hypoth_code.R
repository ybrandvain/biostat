# Hypothesis testing simulation
library(tidyverse)


# You will pick your parameter value by chance
my_param <- sample(c(.2,.4,.5), size = 1, prob = c(1/4,1/4,1/2))
my_param # enter this line to find your parameter value.

null_value <- .5

# Null model: coin comes up heads half the time
# Alternative model: coin does not come up heads  half the time
# Flip  your 10 coin ten times & count the number of heads

# Estimate from sample
n_flips <- 10 # change me to change sample size
my_estimate <- tibble(flips = sample(c("Heads", "Tails"),
                                     prob = c(my_param, 1 - my_param),
                                     replace = TRUE,
                                     size = n_flips)) %>%
  summarise(p_heads = mean(flips == "Heads")) %>%
  pull()

my_estimate # enter this line to find your estimate


# what was your true parameter?
# what was your estimate


# compare to null distribution
n_reps <- 100000
null_samp_dist <- tibble(flips = sample(c("Heads", "Tails"),
                                        prob = c(null_value, 1 - null_value),
                                        replace = TRUE,
                                        size = n_flips * n_reps),
                         coin  = rep(1:n_reps, each = n_flips)) %>%
  group_by(coin)%>%
  summarise(p_heads = mean(flips == "Heads"))


### Visualize the null sampling dist

ggplot(null_samp_dist, aes(x = p_heads)) +
  geom_histogram(binwidth = 1/n_flips, color = "white", fill = "black")+
  geom_vline(xintercept = my_estimate,
             color = "red",
             lty = 2) +# compare null dist to your estimate
  geom_vline(xintercept = my_param,
             color = "blue",
             lty = 2) +# compare null dist and estimate to the true parameter
  annotate(x = my_estimate, y = 11000, color = "red",label = "my_estimate", geom = "label", hjust = 0.5)+
  annotate(x = my_param, y = 10000, color = "blue",label = "my_param", geom = "label", hjust = .5, fontface = "bold")+
  annotate(x = .5, y = 3000, color = "black",label = "null distribution", geom = "label", hjust = .5, fontface = "bold")

### Find a p-value
null_samp_dist %>%
  summarise(as_or_more  = abs(p_heads - null_value ) >= abs(my_estimate- null_value))%>%
  summarise(mean(as_or_more))


# Go to top -- repeat with n_flips = 20, 50, 100...how does this change things?
# Pick a new param. How do things change.


# Sketch a bayesian approach to this problem with the priors as described above