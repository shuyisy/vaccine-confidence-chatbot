library(tidyverse)
library(viridisLite)

# Read in data ----
# heroku
ff <- file.info(list.files('../data', pattern = 'dataclips_', full.names = TRUE))
file <- rownames(ff)[which.max(ff$mtime)]
dfh <- read.csv(file)
nrow(dfh) 

# chatfuel
ff <- file.info(list.files('../data', pattern = 'Data_For_Society', full.names = TRUE))
file <- rownames(ff)[which.max(ff$mtime)]
dfc <- read.csv(file)
colnames(dfc)[order(colnames(dfc))] 
names(dfc)[names(dfc) == 'chatfuel.user.id'] <- 'user_id'
dfc <- dfc %>% relocate(template_id) %>% relocate(treatment_id) %>% relocate(concern_id) %>% relocate(user_id) 

# Checks ----
# check heroku data
dfh[rowSums(is.na(dfh)) > 0,]
table(dfh$engage_id)
table(dfh$concern_id)
table(dfh$treatment_id)
table(dfh$concern_id, dfh$treatment_id)

addmargins(table(dfh$concern_id, dfh$treatment_id))


# Cleaning/manipulation ----
# keep only heroku assignments that respect policy
dfh <- dfh %>% filter(engage_id != 3 & engage_id != 4 & engage_id != 5 & engage_id != 6)
dfh <- dfh %>% filter(treatment_id != 9)
dfh <- dfh %>% filter(!(concern_id == 2 & treatment_id ==16))
aggregate(dfh$treatment_id, by = list(dfh$concern_id), function(x) names(table(x)))
dfh$count <- ave(rep(1,nrow(dfh)), dfh$treatment_id, dfh$concern_id, FUN = cumsum) # for graph
nrow(dfh) 

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


# Merge datasets ----
df_m <- merge(dfh, dfc, by = 'user_id',
              suffixes = c('', '_c')) %>% 
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
  )


# create separate dfhs for Kenya, Nigeria
df_mK <- df_m[which(tolower(df_m$country1) == 'kenya'),]
df_mN <- df_m[which(tolower(df_m$country1) == 'nigeria'),]


# Get estimates ----

# * sample mean ----
sm_out <- aggregate(df_m$success, 
                    by = list(treament = df_m$treatment_id, 
                              concern = df_m$concern_id), 
                    function(x) c(mean = mean(x), count = length(x)))

sm_out_willingness2 <- aggregate(df_m$willingness_2, 
                                 by = list(treament = df_m$treatment_id, 
                                           concern = df_m$concern_id), 
                                 function(x) c(mean = mean(x, na.rm = TRUE), count = sum(!is.na(x))))

sm_out_get_vaccinated_2 <- aggregate(df_m$get_vaccinated_2, 
                                     by = list(treament = df_m$treatment_id, 
                                               concern = df_m$concern_id), 
                                     function(x) c(mean = mean(x, na.rm = TRUE), count = sum(!is.na(x))))

sm_out_response_diff <- aggregate(df_m$response_diff, 
                                  by = list(treament = df_m$treatment_id, 
                                            concern = df_m$concern_id), 
                                  function(x) c(mean = mean(x, na.rm = TRUE), count = sum(!is.na(x))))

sm_outK <- aggregate(df_mK$success, 
                     by = list(treament = df_mK$treatment_id, 
                               concern = df_mK$concern_id), 
                     function(x) c(mean = mean(x), count = length(x)))

sm_outN <- aggregate(df_mN$success, 
                     by = list(treament = df_mN$treatment_id, 
                               concern = df_mN$concern_id), 
                     function(x) c(mean = mean(x), count = length(x)))

# * hajek estimate ----
hj_out <- by(df_m[, c('success', 'prob', 'treatment_id', 'concern_id')], 
             list(treatment.concern = 
                    interaction(df_m$treatment_id, df_m$concern_id, drop = TRUE)), 
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

# Kenya
hj_outK <- by(df_mK[, c('success', 'prob', 'treatment_id', 'concern_id')], 
              list(treatment.concern = 
                     interaction(df_mK$treatment_id, df_mK$concern_id, drop = TRUE)), 
              function(dd) {
                y <- dd$success
                probs <- dd$prob
                numer <- ifelse(is.na(y/probs), 0, y/probs)
                # denominator takes 1/assignment probabilities, only if actually assigned
                denom <- sum(1/probs * !is.na(y))
                
                c(hj = sum( numer/denom ), count = length(y), message = mean(dd$treatment_id), concern = mean(dd$concern_id))
              },
              simplify = FALSE)


hj_outK <- do.call(rbind, hj_outK)

# Nigeria
hj_outN <- by(df_mN[, c('success', 'prob', 'treatment_id', 'concern_id')], 
              list(treatment.concern = 
                     interaction(df_mN$treatment_id, df_mN$concern_id, drop = TRUE)), 
              function(dd) {
                y <- dd$success
                probs <- dd$prob
                numer <- ifelse(is.na(y/probs), 0, y/probs)
                # denominator takes 1/assignment probabilities, only if actually assigned
                denom <- sum(1/probs * !is.na(y))
                
                c(hj = sum( numer/denom ), count = length(y), message = mean(dd$treatment_id), concern = mean(dd$concern_id))
              },
              simplify = FALSE)


hj_outN <- do.call(rbind, hj_outN)
# hajek estimate for response_diff
hj_response_diff <- by(df_m[which(!is.na(df_m$response_diff)), c('response_diff', 'prob', 'treatment_id', 'concern_id')], 
                       list(treatment.concern = 
                              interaction(df_m$treatment_id[which(!is.na(df_m$response_diff))], df_m$concern_id[which(!is.na(df_m$response_diff))], drop = TRUE)), 
                       function(dd) {
                         y <- dd$response_diff
                         probs <- dd$prob
                         numer <- y/probs
                         denom <- sum(1/probs)
                         c(hj = sum(numer/denom), count = length(y), message = mean(dd$treatment_id), 
                           concern = mean(dd$concern_id))
                       },
                       simplify = FALSE)
hj_response_diff <- data.frame(do.call(rbind, hj_response_diff))

# hajek estimate for willingness_2
hj_willingness_2 <- by(df_m[which(!is.na(df_m$willingness_2)), c('willingness_2', 'prob', 'treatment_id', 'concern_id')], 
                       list(treatment.concern = 
                              interaction(df_m$treatment_id[which(!is.na(df_m$willingness_2))], df_m$concern_id[which(!is.na(df_m$willingness_2))], drop = TRUE)), 
                       function(dd) {
                         y <- dd$willingness_2
                         probs <- dd$prob
                         numer <- y/probs
                         denom <- sum(1/probs)
                         c(hj = sum(numer/denom), count = length(y), message = mean(dd$treatment_id), 
                           concern = mean(dd$concern_id))
                       },
                       simplify = FALSE)
hj_willingness_2 <- data.frame(do.call(rbind, hj_willingness_2))

# hajek estimate for get_vaccinated_2
hj_get_vaccinated_2 <- by(df_m[which(!is.na(df_m$get_vaccinated_2)), c('get_vaccinated_2', 'prob', 'treatment_id', 'concern_id')], 
                          list(
                            treatment.concern = 
                              interaction(
                                df_m$treatment_id[which(!is.na(df_m$get_vaccinated_2))], 
                                df_m$concern_id[which(!is.na(df_m$get_vaccinated_2))], drop = TRUE)), 
                          function(dd) {
                            y <- dd$get_vaccinated_2
                            probs <- dd$prob
                            numer <- y/probs
                            denom <- sum(1/probs)
                            c(hj = sum(numer/denom), count = length(y), message = mean(dd$treatment_id), 
                              concern = mean(dd$concern_id))
                          },
                          simplify = FALSE)
hj_get_vaccinated_2 <- data.frame(do.call(rbind, hj_get_vaccinated_2))




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

hj_outK <- hj_outK[order(hj_out[,'concern'], -hj_out[,'hj']),]
hj_outN <- hj_outN[order(hj_out[,'concern'], -hj_out[,'hj']),]
hj_willingness_2 <- hj_willingness_2[order(hj_out[,'concern'], -hj_out[,'hj']),]
hj_get_vaccinated_2 <- hj_get_vaccinated_2[order(hj_out[,'concern'], -hj_out[,'hj']),]
hj_response_diff <- hj_response_diff[order(hj_out[,'concern'], -hj_out[,'hj']),]
sm_out <- sm_out[order(hj_out[,'concern'], -hj_out[,'hj']),]
sm_outK <- sm_outK[order(hj_out[,'concern'], -hj_out[,'hj']),]
sm_outN <- sm_outN[order(hj_out[,'concern'], -hj_out[,'hj']),]
sm_out_willingness2 <- sm_out_willingness2[order(hj_out[,'concern'], -hj_out[,'hj']),]
sm_out_get_vaccinated_2 <- sm_out_get_vaccinated_2[order(hj_out[,'concern'], -hj_out[,'hj']),]
sm_out_response_diff <- sm_out_response_diff[order(hj_out[,'concern'], -hj_out[,'hj']),]
sm_outN <- sm_outN[order(hj_out[,'concern'], -hj_out[,'hj']),]


hj_out <- hj_out[order(hj_out[,'concern'], -hj_out[,'hj']),]

results_df_m <- data.frame(sm = sm_out$x[,'mean'], 
                           hj = hj_out[,'hj'], 
                           smK = sm_outK$x[,'mean'], 
                           smN = sm_outN$x[,'mean'], 
                           sm_willing = sm_out_willingness2$x[,'mean'],
                           sm_get_vaccinated = sm_out_get_vaccinated_2$x[,'mean'],
                           sm_response_diff = sm_out_response_diff$x[,'mean'],
                           sd_hj = sqrt(hj_out[,'hj']*(1-hj_out[,'hj'])),
                           error = sqrt(hj_out[,'hj']*(1-hj_out[,'hj']))*.1, # decision rule for including second best arm
                           count = hj_out[,'count'],
                           countK = hj_outK[,'count'],
                           countN = hj_outN[,'count'],
                           concerns = concerns[hj_out[,'concern']],
                           messages = gsub('\n', ' ', messages)[hj_out[,'message']]
)



write.csv(results_df_m, file = paste0("../tables-figures/learning_results_estimates_", Sys.Date(), '.csv'), row.names = FALSE)

# * Pledge analysis ----
df_m_pledge <- df_m %>% 
  filter(treatment_id %in% c(7,8, 10)) %>% 
  mutate(pledge_treatment = case_when(engage_id == 0 ~ pledge1_assigned,
                                      # programming error; pledge treatment 
                                      # saved as pledge1_assigned for both 1st
                                      # and second engagement
                                      engage_id == 1 ~ pledge1_assigned, 
                                      engage_id == 2 ~ pledge2_assigned,
                                      TRUE ~ NA_integer_))

sm_out_pledge <- aggregate(df_m_pledge$success, by = list(as.factor(df_m_pledge$pledge_treatment)), 
                           function(x) c(mean = mean(x), count = length(x)))

hj_pledge <- by(df_m_pledge[, c('success', 'prob', 'pledge_treatment')], 
                as.factor(df_m_pledge$pledge_treatment), 
                function(dd) {
                  y <- dd$success
                  probs <- dd$prob
                  numer <- y/probs
                  denom <- sum(1/probs)
                  c(hj = sum(numer/denom), count = length(y))
                },
                simplify = FALSE)

hj_pledge <- data.frame(do.call(rbind, hj_pledge))


# * Benefits analysis ----
df_m_benefits <- df_m %>% 
  filter(treatment_id == 2)

sm_out_benefits <- aggregate(df_m_benefits$success, by = list(as.factor(df_m_benefits$benefitsv2==1)), 
                             function(x) c(mean = mean(x), count = length(x)))

hj_benefits <- by(df_m_benefits[, c('success', 'prob', 'benefitsv2')], 
                  as.factor(df_m_benefits$benefitsv2==1), 
                  function(dd) {
                    y <- dd$success
                    probs <- dd$prob
                    numer <- y/probs
                    denom <- sum(1/probs)
                    c(hj = sum(numer/denom), count = length(y))
                  },
                  simplify = FALSE)

hj_benefits <- data.frame(do.call(rbind, hj_benefits))


results_df_bp <- data.frame(sm_pledge = sm_out_pledge$x[,'mean'], 
                            hj_pledge = hj_pledge[,'hj'], 
                            sm_benefits = sm_out_benefits$x[,'mean'], 
                            hj_benefits = hj_benefits[,'hj'], 
                            count_pledge = sm_out_pledge$x[,'count'], 
                            count_benefits = sm_out_benefits$x[,'count'],
                            group = c(0,1)
)


write.csv(results_df_bp, file = paste0("../tables-figures/learning_results_estimates_benefits_plege", Sys.Date(), '.csv'), row.names = FALSE)


# Plots ----

# # overall concerns
# ggplot(dfh, aes(concern_id)) +
#   geom_bar() +
#   scale_x_continuous(breaks = sort(unique(dfh$concern_id))) +
#   theme_bw()
# 
# # message by concern
# ggplot(dfh, aes(treatment_id)) +
#   facet_grid(rows = vars(concern_id)) +
#   geom_bar() +
#   scale_x_continuous(breaks = sort(unique(dfh$treatment_id))) +
#   theme_bw()

# assignment plots

for(cid in sort(unique(dfh$concern_id)) ){
  ddfh <- dfh[which(dfh$concern_id == cid),]
  ddfh$time <- 1:nrow(ddfh)
  g <- ggplot(ddfh, aes(x = time, y = count, color = as.factor(treatment_id))) +
    geom_line() +
    scale_colour_viridis_d(name = 'Message id', labels = as_labeller(messages)) + 
    xlab('Observations') +
    ylab('Cumulative assignment') +
    ggtitle(paste0('Cumulative assignment under concern ', cid),
            subtitle = concerns[cid]) + 
    coord_cartesian(ylim = c(0, ceiling(max(ddfh$count)/100)*100)) +
    theme_bw()
  
  ggsave(filename = paste0('../tables-figures/cum_assign', cid, '.pdf'), 
         plot = g, 
         width = 6, height = 4)
}

# Static assignment
for(cid in sort(unique(dfh$concern_id)) ){
  ddfh <- dfh[which(dfh$concern_id == cid),]
  ddfh$time <- 1:nrow(ddfh)
  ddfh_alt <- ddfh
  ddfh_alt$treatment_id <- rep(unique(ddfh$treatment_id), ceiling(nrow(ddfh)/length(unique(ddfh$treatment_id))))[1:nrow(ddfh)]
  ddfh_alt$count <- ave(rep(1,nrow(ddfh_alt)), ddfh_alt$treatment_id, FUN = cumsum)
  
  g <- ggplot(ddfh_alt, aes(x = time, y = count, color = as.factor(treatment_id))) +
    geom_line() + 
    scale_colour_viridis_d(name = 'Message id', labels = as_labeller(messages)) + 
    xlab('Observations') +
    ylab('Cumulative assignment') +
    ggtitle(paste0('Static cumulative assignment under concern ', cid),
            subtitle = concerns[cid]) + 
    coord_cartesian(ylim = c(0, ceiling(max(ddfh$count)/100)*100)) +
    theme_bw()
  
  ggsave(filename = paste0('../tables-figures/cum_assign_static', cid, '.pdf'), 
         plot = g, 
         width = 6, height = 4)
}


