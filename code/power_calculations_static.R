library(ggplot2)
library(viridisLite)

# cumulative hajek function
hj_f <- function(y,probs){
  # numerator is same as HT, changing NAs to zeros
  numer <- cumsum(ifelse(is.na(y/probs), 0, y/probs))
  # denominator takes 1/assignment probabilities, only if actually assigned
  denom <- cumsum(1/probs * !is.na(y))
  return(numer/denom)
}

set.seed(60637)

# Set simulation parameters ----

# Suppose there are three arms, and we're running an experiment with up to 1000 
# observations. We'll look at how this experiment plays out across 1000 
# repetitions.
n_arms <- 3
n_exp <- 1000
n_reps <- 1000

# True data is drawn from Binomial distributions. 
## arm 1: Binom(0.49)
## arm 2: Binom(0.5), 
## arm 3: Binom(0.55)

dgp <- c(mean = c(0.49, 0.5, 0.55))

# - Initialize priors.
alpha <- beta <- 1

# - Initialize posterior matrix for probabilities; sample from each of the 
#   posteriors ndraw times.
#   We'll overwrite this each time we update posteriors. 
ndraws <- 1000
mu_posterior_samples <- matrix(NA, nrow = ndraws, ncol = n_arms)

# - Initialize list of probability matrices, so we know with what probability we 
#   assigned each arm for each observation
# - Initialize list of assignment dummy matrices, so we know which observations 
#   were assigned which treatment
# - Initialize response array
probs_arr <- assign_arr <- y_arr <- array(
  dim = c(n_exp, n_reps, n_arms), 
  dimnames = list(paste0('obs_', 1:n_exp), 
                  paste0('rep_', 1:n_reps),
                  paste0('arm_', 1:n_arms))
)

# Run simulations ----

for(rep in 1:n_reps){
  # run simulation
  cat('\n', 'Iteration:', rep, '\n')
  
  for(i in 1:n_exp){
    if(i %in% seq(0, n_exp, 100))
      cat(i,'...')
    
    if(i <= 1000){ # assign treatment to first 100 observations uniformly, for a more stable algorithm
      
      probs_arr[i,rep,] <- mu_prob <- rep(1/n_arms, n_arms)
      
    } else {
      
      # get arm posterior parameters
      x <- colSums(y_arr[1:i,rep,]==1, na.rm = TRUE)
      no <- colSums(y_arr[1:i,rep,]==0, na.rm = TRUE)
      
      # sample from the posterior distribution of each arm 1k times
      for (arm in 1:n_arms) mu_posterior_samples[, arm] <- rbeta(ndraws, 
                                                                 x[arm] + alpha, 
                                                                 no[arm] +
                                                                   beta)
      
      # get overall posterior probabilities, based on the proportion of times each arm was best
      mu_prob_overall <- prop.table(table(factor(
        c(apply(mu_posterior_samples, 1, order, decreasing = TRUE)[1:2,]), 
        levels = 1:n_arms)))
      
      probs_arr[i,rep,] <- (1-mu_prob_overall*2)*0.1/n_arms + mu_prob_overall*(1-(n_arms-2)*(0.1/n_arms) )
      
      # sample according to algorithm
      theta_hat <- sapply(1:n_arms, function(arm){
        rbeta(1, x[arm] + alpha, no[arm] + beta)
      })
      max_idx <- order(theta_hat, decreasing = TRUE)[1:2]
      mu_prob[-max_idx] <- 0.1/n_arms
      mu_prob[max_idx] <- (1-(n_arms-2)*(0.1/n_arms) )/2
    }
    
    # assign treatment based on posterior probability
    assignment <- sample(x = n_arms, size = 1, prob =  mu_prob)
    # update dummy assignment matrix
    assign_arr[i, rep, assignment] <- 1
    
    # observe response under assignment treatment
    y_arr[i, rep, assignment] <- rbinom(n = 1, size = 1, prob = dgp[assignment])
  }
}

# Arrange data ----

# combine realized assignment and probability of realized assignment across reps
assign_mat <- apply(assign_arr, c(1,2), which.max)
pw_mat <- do.call('cbind', lapply(1:n_reps, 
                                  function(x){
                                    probs_arr[cbind(1:n_exp,x,assign_mat[,x])]
                                  } ))


# sample mean, cumulative estimates
sm_cum <- apply(y_arr, c(2,3), function(x) {
  (cumsum(ifelse(is.na(x), 0, x)))/cumsum(!is.na(x))
})

# across simulation, how often do we get it correct?
sm_correct <- rowMeans(apply(sm_cum, c(1,2), function(x) which.max(x)==which.max(dgp)))

# bias
sm_cumb <- apply(sm_cum - array(rep(dgp, each = n_exp*n_reps),
                                dim = c(n_exp, n_reps, n_arms), 
                                dimnames = list(paste0('obs_', 1:n_exp), 
                                                paste0('rep_', 1:n_reps),
                                                paste0('arm_', 1:n_arms))), 
                 c(1,3), mean, na.rm = TRUE)


# ht, cumulative estimates
ht_cum <- apply(y_arr/probs_arr, c(2,3), function(x) {
  cumsum(ifelse(is.na(x), 0, x))/seq_along(x)
})

# across simulation, how often do we get it correct?
ht_correct <- rowMeans(apply(ht_cum, c(1,2), function(x) which.max(x)==which.max(dgp)))

# bias
ht_cumb <- apply(ht_cum - array(rep(dgp, each = n_exp*n_reps),
                                dim = c(n_exp, n_reps, n_arms), 
                                dimnames = list(paste0('obs_', 1:n_exp), 
                                                paste0('rep_', 1:n_reps),
                                                paste0('arm_', 1:n_arms))), 
                 c(1,3), mean, na.rm = TRUE)

# hajek, cumulative estimates

hj_cum <- sapply(1:n_arms, # applying over repetitions
                 function(x){
                   sapply(1:n_reps, # and applying over each arm
                          # take the cumulative hj function
                          function(y) hj_f(y_arr[,y,x], probs_arr[,y,x])
                   )
                 }, 
                 simplify = 'array')

# across simulation, how often do we get it correct?
hj_correct <- rowMeans(apply(hj_cum, c(1,2), function(x) which.max(x)==which.max(dgp)))

# bias
hj_cumb <- apply(hj_cum - array(rep(dgp, each = n_exp*n_reps),
                                dim = c(n_exp, n_reps, n_arms), 
                                dimnames = list(paste0('obs_', 1:n_exp), 
                                                paste0('rep_', 1:n_reps),
                                                paste0('arm_', 1:n_arms))), c(1,3), mean, na.rm = TRUE)

# Plots ----
# Plot mapping cumulative assignment

cum_assign_l <- lapply(seq(dim(assign_arr)[2]), function(x) {
  cm <- data.frame(
    apply(assign_arr[ , x, ], 2, function(y){
      cumsum(ifelse(is.na(y), 0, y)) # get cum assignment, accounting for NAs
    }), 
    rep = x) # save repetition column
  cm_l <- reshape(cm, varying = names(cm)[1:n_arms], 
                  direction = 'long', 
                  idvar = 'n', timevar = 'p', sep = '_' )
} )


cum_assign_long <- do.call(rbind.data.frame, cum_assign_l)

sidx <- which(cum_assign_long$rep %in% sample(1:n_reps, size = min(n_reps, 100)))

gg1 <- ggplot(cum_assign_long[sidx,], aes(x = n, y = arm, color = as.factor(p), stroke = as.factor(rep))) +
  facet_grid(cols = vars(as.factor(p))) +
  geom_line(alpha = 0.2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = 'Top-two Thompson, arm assignment across repetitions', 
       x = 'Time', y = 'Cumulative Assignment', 
       color = 'Success\nProbability') +
  scale_colour_viridis_d(labels = as.character(dgp)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

ggsave(file = '../tables-figures/static_cumulative.png', 
       plot = gg1, 
       device = 'png',
       width = 8, height = 4)


# Plot of proportion correct across estimators
cmat <- data.frame(n = rep(1:n_exp, 3), correct_pct = c(sm_correct, ht_correct, hj_correct), 
                   estimator = rep(c('Sample mean', 'Horvitz-Thompson', 'Hajek'), each = n_exp))


gg2 <- ggplot(cmat, aes(x = n, y = correct_pct, color = as.factor(estimator))) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = 'Top-two Thompson, best arm selection', x = 'Time', 
       y = 'Proportion Correct', 
       color = 'Estimator') +
  scale_colour_viridis_d() +
  coord_cartesian(ylim = c(0,1))

ggsave(file = '../tables-figures/static_correct.png', 
       plot = gg2, 
       device = 'png',
       width = 8, height = 4)


# Bias plot
bias_l <- lapply(list(sm_cumb, ht_cumb, hj_cumb), function(x) {
  bdf <- reshape(as.data.frame(x), 
                 varying = list(1:n_arms), 
                 direction = 'long', timevar = 'p', idvar = 'n', sep = '_', v.names = 'bias')
})

bias_long <- cbind(estimator = rep(c('Sample mean', 'Horvitz-Thompson', 'Hajek'), each = 3*n_exp), do.call(rbind.data.frame, bias_l))

labelsp <- paste0('p = ', dgp)
names(labelsp) <- 1:n_arms

gg3 <- ggplot(bias_long, aes(x = n, y = bias, color = estimator)) +
  facet_grid(vars(p), labeller = as_labeller(labelsp)) +
  geom_hline(yintercept = 0, color = 'lightgray', lty = 'dashed') +
  geom_line() +
  theme_bw() + 
  coord_cartesian(ylim = c(-1,1)*0.025) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = 'Top-two Thompson, bias', x = 'Time', 
       y = 'Bias', 
       color = 'Estimator') +
  scale_colour_viridis_d()

ggsave(file = '../tables-figures/static_bias.png', 
       plot = gg3, 
       device = 'png',
       width = 6, height = 4)

write.csv(cmat, file = '../tables-figures/best_arm_sims_static.csv')