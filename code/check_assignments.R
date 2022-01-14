library(ggplot2)
library


#  hajek function
hj <- function(y,probs){
  # numerator is same as HT, changing NAs to zeros
  numer <- cumsum(ifelse(is.na(y/probs), 0, y/probs))
  # denominator takes 1/assignment probabilities, only if actually assigned
  denom <- cumsum(1/probs * !is.na(y))
  return(numer/denom)
}

ff <- file.info(list.files('../data', pattern = 'dataclips_', full.names = TRUE))
file <- rownames(ff)[which.max(ff$mtime)]
df <- read.csv(file)

table(df$engage_id)
table(df$concern_id)

table(df$concern_id)

addmargins(table(df$concern_id, df$treatment_id))

aggregate(df$treatment_id, by = list(df$concern_id), function(x) names(table(x)))
# only one or two off-assignment may be due to missed connections with the server;
# if assignment is not sent back in time, treatment is assigned randomly.

aggregate(df, by = list(df$concern_id), function(x) apply(x, 2, function(y) all(!is.na(y))))

ggplot(df, aes(concern_id)) +
  geom_bar() +
  scale_x_continuous(breaks = sort(unique(df$concern_id))) +
  theme_bw()


ggplot(df, aes(treatment_id)) +
  facet_grid(rows = vars(concern_id)) +
  geom_bar() +
  scale_x_continuous(breaks = sort(unique(df$treatment_id))) +
  theme_bw()


df$time <- 1:nrow(df)
df$prob <- paste0('prob_c')
df_long <- reshape(df, direction = 'long', varying = prob)

df1 <- df[which(df$concern_id == 1),]

df11 <- df[which(df$concern_id == 1, df$treatment_id ==1),]


reshape((df1, direction = 'long', 
         ))

aggregate(df$success, by = list(df$treatment_id, df$concern_id), 
          function(x) c(mean = mean(x), count = length(x)))

prob_cols <- names(df)[grep('prob', names(df))]


prob_cols <- names(df)[grep('prob', names(df))]
aggregate.data.frame(df[,prob_cols],
          by = list(df$concern_id), print)
          function(dd) lapply(dd, function(x) sum(x == 0)))


dd <- df[which(df$concern_id == 3),prob_cols]
prob_cols[apply(dd, 2, function(x) sum(x == 0)==0)]



