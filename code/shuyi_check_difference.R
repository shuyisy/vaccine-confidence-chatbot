library(tidyverse)
library(dplyr)

## analyze heroku data

# read in heroku data
dfh <- read.csv("data_heroku.csv")
nrow(dfh) #4748

# check heroku data
dfh[rowSums(is.na(dfh)) > 0,]
table(dfh$engage_id)
table(dfh$concern_id)
table(dfh$treatment_id)
table(dfh$concern_id, dfh$treatment_id)

# keep only assignments that respect policy
dfh <- dfh %>% filter(engage_id != 3 & engage_id != 4 & engage_id != 5 & engage_id != 6)
dfh <- dfh %>% filter(treatment_id != 9)
dfh <- dfh %>% filter(!(concern_id == 2 & treatment_id ==16))
aggregate(dfh$treatment_id, by = list(dfh$concern_id), function(x) names(table(x)))
dfh$count <- ave(rep(1,nrow(dfh)), dfh$treatment_id, dfh$concern_id, FUN = cumsum) # for graph
nrow(dfh) #4738

# identify assigned probability
probidx <- sapply(paste0('prob_c', dfh$concern_id, '_m', dfh$treatment_id), function(x) which(names(dfh) %in% x))
dfh$prob <- as.numeric(dfh[cbind(1:nrow(dfh),unlist(probidx))])

# check assignments by success rate (higher rate leads to more assignments)
sm_out <- dfh %>% group_by(concern_id, treatment_id) %>% 
  summarise(sm = mean(success), count = length(success)) %>% 
  arrange(concern_id, treatment_id)

# hajek estimate
hj_out <- by(dfh[, c('success', 'prob', 'treatment_id', 'concern_id')], 
             list(treatment.concern = 
                    interaction(dfh$treatment_id, dfh$concern_id, drop = TRUE)), 
             function(dd) {
               y <- dd$success
               probs <- dd$prob
               numer <- y/probs
               denom <- sum(1/probs)
               c(hj = sum(numer/denom), count = length(y), message = mean(dd$treatment_id), concern = mean(dd$concern_id))
             },
             simplify = FALSE)
hj_out <- data.frame(do.call(rbind, hj_out))
hj_out <- hj_out[order(hj_out[,'concern'], -hj_out[,'hj']),]

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

results_df <- data.frame(sm = sm_out[,'sm'], 
                         hj = hj_out[,'hj'], 
                         sd_hj = sqrt(hj_out[,'hj']*(1-hj_out[,'hj'])),
                         error = hj_out[,'hj']*(1-hj_out[,'hj'])*0.1, # decision rule for including second best arm
                         count = hj_out[,'count'],
                         concerns = concerns[hj_out[,'concern']],
                         messages = gsub('\n', ' ', messages)[hj_out[,'message']]
)
results_df <- results_df[order(hj_out[,'concern'], -hj_out[,'count']),]


# graphs
for(cid in sort(unique(dfh$concern_id))){
  ddf <- dfh[which(dfh$concern_id == cid),]
  ddf$time <- 1:nrow(ddf)
  g <- ggplot(ddf, aes(x = time, y = count, color = as.factor(treatment_id))) +
    geom_line() + 
    scale_color_discrete(name = 'Message id', labels = as_labeller(messages)) +
    xlab('Observations') +
    ylab('Cumulative assignment') +
    ggtitle(paste0('Cumulative assignment under concern ', cid),
            subtitle = concerns[cid]) +
    coord_cartesian(ylim = c(0, ceiling(max(ddf$count)/100)*100))
  
  ggsave(filename = paste0('./tables-figures/cum_assign', cid, '.pdf'), 
         plot = g, 
         width = 6, height = 4)
}

# read in the chatfuel data
dfc <- read.csv("data_chatfuel.csv")
colnames(dfc)[order(colnames(dfc))] 
names(dfc)[names(dfc) == 'chatfuel.user.id'] <- 'user_id'
dfc <- dfc %>% relocate(template_id) %>% relocate(treatment_id) %>% relocate(concern_id) %>% relocate(user_id) 

# unique(dfm7$willingness)
# unique(dfm7$willingness_2)
# unique(dfm7$get_vaccinated)
# unique(dfm7$get_vaccinated_2)
# sum(is.na(dfm3$willingness))
# sum(is.na(dfm3$willingness_2))
# sum(is.na(dfm3$get_vaccinated))
# sum(is.na(dfm3$get_vaccinated_2))
# sum(is.na(dfm3$benefitsv2))

# merge the two datasets
dfm1 <- merge(dfh, dfc, by="user_id")
dfm2 <- dfm1 %>% select(-c(id)) 
dfm3 <- dfm2 %>% relocate(user_id, success.x, willingness, willingness_2, get_vaccinated, get_vaccinated_2)
dfm4 <- dfm3[!(dfm3$willingness=='Skip'| dfm3$willingness=='skip'),]
dfm5 <- dfm4[!(dfm4$willingness_2==''| dfm4$willingness_2=='Skip'| 
                 dfm4$willingness_2=='Am done answering a repeated questions'),]
dfm6 <- dfm5[!(dfm5$get_vaccinated=='Skip'| dfm5$get_vaccinated=='SKip'| dfm5$get_vaccinated=='Morderate'|
                 startsWith(dfm5$get_vaccinated, "https://")),]
dfm7 <- dfm6[!(dfm6$get_vaccinated_2==''| dfm6$get_vaccinated_2=='Skip'| dfm6$get_vaccinated_2=='0'),]
dfm8 <- dfm7 %>% 
  mutate(get_vaccinated = case_when(get_vaccinated == 'No, definitely not' ~ 1,
                                    get_vaccinated == 'No, probably not' ~ 2,
                                    get_vaccinated == 'Yes, probably' ~ 3,
                                    get_vaccinated == 'Yes, definitely' ~ 4,
                                    TRUE ~ NA_real_),
         get_vaccinated_2 = case_when(get_vaccinated_2 == 'No, definitely not' ~ 1,
                                      get_vaccinated_2 == 'No, probably not' ~ 2,
                                      get_vaccinated_2 == 'Yes, probably' ~ 3,
                                      get_vaccinated_2 == 'Yes, definitely' ~ 4,
                                      TRUE ~ NA_real_),
         willingness = case_when(willingness == 'Not at all' ~ 1,
                                 willingness == 'A little' ~ 2,
                                 willingness == 'Moderately' ~ 3,
                                 willingness == 'Very much' ~ 4,
                                 TRUE ~ NA_real_),
         willingness_2 = case_when(willingness_2 == 'Not at all' ~ 1,
                                   willingness_2 == 'A little' ~ 2,
                                   willingness_2 == 'Moderately' ~ 3,
                                   willingness_2 == 'Very much' ~ 4,
                                   TRUE ~ NA_real_),
         response_diff = (get_vaccinated_2 + willingness_2) - (get_vaccinated + willingness)
  ) %>% relocate(user_id, response_diff, success.x)

# hajek estimate for response_diff
hj_response <- by(dfm8[, c('response_diff', 'prob', 'treatment_id.x', 'concern_id.x')], 
             list(treatment.concern = 
                    interaction(dfm8$treatment_id.x, dfm8$concern_id.x, drop = TRUE)), 
             function(dd) {
               y <- dd$response_diff
               probs <- dd$prob
               numer <- y/probs
               denom <- sum(1/probs)
               c(hj = sum(numer/denom), count = length(y), message = mean(dd$treatment_id.x), 
                 concern = mean(dd$concern_id.x))
             },
             simplify = FALSE)
hj_response <- data.frame(do.call(rbind, hj_response))
hj_response <- hj_response[order(hj_response[,'concern'], -hj_response[,'hj']),]

# hajek estimate for willingness_2
hj_willing <- by(dfm8[, c('willingness_2', 'prob', 'treatment_id.x', 'concern_id.x')], 
                  list(treatment.concern = 
                         interaction(dfm8$treatment_id.x, dfm8$concern_id.x, drop = TRUE)), 
                  function(dd) {
                    y <- dd$willingness_2
                    probs <- dd$prob
                    numer <- y/probs
                    denom <- sum(1/probs)
                    c(hj = sum(numer/denom), count = length(y), message = mean(dd$treatment_id.x), 
                      concern = mean(dd$concern_id.x))
                  },
                  simplify = FALSE)
hj_willing <- data.frame(do.call(rbind, hj_willing))
hj_willing <- hj_willing[order(hj_willing[,'concern'], -hj_willing[,'hj']),]

# hajek estimate for get_vaccinated_2
hj_vaccine <- by(dfm8[, c('get_vaccinated_2', 'prob', 'treatment_id.x', 'concern_id.x')], 
                 list(treatment.concern = 
                        interaction(dfm8$treatment_id.x, dfm8$concern_id.x, drop = TRUE)), 
                 function(dd) {
                   y <- dd$get_vaccinated_2
                   probs <- dd$prob
                   numer <- y/probs
                   denom <- sum(1/probs)
                   c(hj = sum(numer/denom), count = length(y), message = mean(dd$treatment_id.x), 
                     concern = mean(dd$concern_id.x))
                 },
                 simplify = FALSE)
hj_vaccine <- data.frame(do.call(rbind, hj_vaccine))
hj_vaccine <- hj_vaccine[order(hj_vaccine[,'concern'], -hj_vaccine[,'hj']),]

