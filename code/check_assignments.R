library(ggplot2)

# Read in data ----
ff <- file.info(list.files('../data', pattern = 'dataclips_', full.names = TRUE))
file <- rownames(ff)[which.max(ff$mtime)]
df <- read.csv(file)

# Checks ----
table(df$engage_id)
table(df$concern_id)

addmargins(table(df$concern_id, df$treatment_id))

aggregate(df$treatment_id, by = list(df$concern_id), function(x) names(table(x)))
# only one or two off-assignment may be due to missed connections with the server;
# if assignment is not sent back in time, treatment is assigned randomly.



# Cleaning/manipulation ----
# Keep only assignments that respect policy
df <- df[which(df$treatment_id!=9),]
df <- df[-which(df$treatment_id==16 & df$concern_id == 2),] # TODO this step more rigorously



# count cumulative observations
df$count <- ave(rep(1,nrow(df)), df$treatment_id, df$concern_id, FUN = cumsum)

# record assigned probability
probidx <- sapply(paste0('prob_c', df$concern_id, '_m', df$treatment_id), function(x) which(names(df) %in% x))
probidx[sapply(probidx, function(x) length(x) ==0)] <- NA
df$prob <- as.numeric(df[cbind(1:nrow(df),unlist(probidx))])

# Get estimates ----

# sample mean
sm_out <- aggregate(df$success, by = list(treament = df$treatment_id, concern = df$concern_id), 
                    function(x) c(mean = mean(x), count = length(x)))

# hajek estimate
hj_out <- by(df[, c('success', 'prob', 'treatment_id', 'concern_id')], 
             list(treatment.concern = 
                    interaction(df$treatment_id, df$concern_id, drop = TRUE)), 
             function(dd) {
               y <- dd$success
               probs <- dd$prob
               numer <- ifelse(is.na(y/probs), 0, y/probs)
               # denominator takes 1/assignment probabilities, only if actually assigned
               denom <- sum(1/probs * !is.na(y))
               
               c(hj = sum( numer/denom ), count = length(y), message = mean(dd$treatment_id), concern = mean(dd$concern_id))
             },
             simplify = FALSE)


hj_out <- do.call(rbind, hj_out)

# primary result estimates for each treatment x concern consideration, hajek and sample mean
messages <- c(`1` = 'Risks',
              `2` = 'Benefits',
              `3` = 'Elite cues\n(political leaders)',
              `4` = 'Elite cues\n(WHO)',
              `5` = 'Elite cues\n(healthcare)',
              `6` = 'Elite cues\n(religious leaders)',
              `7` = 'Misinfo\n(Facebook tips)',
              `8` = 'Misinfo\n(AfricaCheck video)',
              `9` = 'Pledge',
              `10` = 'Misinfo\n(accuracy prime)',
              `11` = 'Misinfo\n(7 types)',
              `12` = 'Social considerations',
              `13` = 'How vaccines work',
              `14` = 'Vaccine\ndevelopment/approvals',
              `15` = 'Vaccine\nsafety/effectiveness',
              `16` = 'Vaccine side effects\n(mild)',
              `17` = 'Vaccine side effects\n(debunk)')


concerns <- c(`1` = 'Side effects',
              `2` = 'Vaccine does not work',
              `3` = 'COVID is not real',
              `4` = 'Protected by God',
              `5` = 'Do not trust healthcare workers',
              `6` = 'Do not trust government',
              `7` = 'Not sure what to believe')

results_df <- data.frame(sm = sm_out$x[,'mean'], 
                         hj = hj_out[,'hj'], 
                         sd_hj = hj_out[,'hj']*(1-hj_out[,'hj']),
                         error = hj_out[,'hj']*(1-hj_out[,'hj'])*0.05, # decision rule for including second best arm
                         count = hj_out[,'count'],
                         concerns = concerns[hj_out[,'concern']],
                         messages = gsub('\n', ' ', messages)[hj_out[,'message']]
                         )

results_df <- results_df[order(hj_out[,'concern'], -hj_out[,'count']),]


write.csv(results_df, file = paste0("../data/results_estimates_", Sys.Date(), '.csv'), row.names = FALSE)


# Plots ----

# # overall concerns
# ggplot(df, aes(concern_id)) +
#   geom_bar() +
#   scale_x_continuous(breaks = sort(unique(df$concern_id))) +
#   theme_bw()
# 
# # message by concern
# ggplot(df, aes(treatment_id)) +
#   facet_grid(rows = vars(concern_id)) +
#   geom_bar() +
#   scale_x_continuous(breaks = sort(unique(df$treatment_id))) +
#   theme_bw()

# assignment plots

for(cid in sort(unique(df$concern_id)) ){
  ddf <- df[which(df$concern_id == cid),]
  ddf$time <- 1:nrow(ddf)
  g <- ggplot(ddf, aes(x = time, y = count, color = as.factor(treatment_id))) +
    geom_line() + 
    scale_color_discrete(name = 'Message id', labels = as_labeller(messages)) +
    xlab('Observations') +
    ylab('Cumulative assignment') +
    ggtitle(paste0('Cumulative assignment under concern ', cid),
            subtitle = concerns[cid])
  
  ggsave(filename = paste0('../tables-figures/cum_assign', cid, '.pdf'), 
         plot = g, 
         width = 6, height = 4)
}

