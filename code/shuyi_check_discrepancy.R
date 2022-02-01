library(tidyverse)
library(dplyr)

## analyze heroku data

# read in data
dfh <- read.csv("data_heroku.csv")
nrow(dfh) #4748

# check data
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

results_df <- data.frame(sm = sm_out[,'sm'], 
                         hj = hj_out[,'hj'], 
                         sd_hj = sqrt(hj_out[,'hj']*(1-hj_out[,'hj'])),
                         error = hj_out[,'hj']*(1-hj_out[,'hj'])*0.05, # decision rule for including second best arm
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



## check the unique rows in heroku data
nrow(dfh) #4738
nrow(unique(dfh[c("user_id", "engage_id")])) #4738
nrow(unique(dfh[c("user_id", "concern_id")])) #4395
nrow(unique(dfh[c("user_id", "concern_id", "treatment_id")])) #4587

# certain (user_id, concenr_id) pairs appear more than once
# some of them are assigned the same treatment
# some of them are assigned different treatments
occur <- data.frame(table(dfh$user_id, dfh$concern_id))
occur_more <- occur %>% filter(Freq > 1)
names(occur_more)[names(occur_more) == 'Var1'] <- 'user_id'
names(occur_more)[names(occur_more) == 'Var2'] <- 'concern_id'
dfh_more <- merge(x=dfh, y=occur_more, by=c("user_id", "concern_id"))



# read in the chatfuel data
dfc <- read.csv("data_chatfuel.csv")
colnames(dfc)[order(colnames(dfc))] 
names(dfc)[names(dfc) == 'chatfuel.user.id'] <- 'user_id'
dfc <- dfc %>% relocate(template_id) %>% relocate(treatment_id) %>% relocate(concern_id) %>% relocate(user_id) 

# some concern_id are not numbers
# is.character(dfc$concern_id)
# dfc$concern_id_num <- as.numeric(dfc$concern_id)
# dfc_na <- dfc %>% filter(is.na(dfc$concern_id_num))
# View(dfc_na)



# construct a new dataset and do some cleaning
dfc2 <- read.csv("data_chatfuel.csv")
names(dfc2)[names(dfc2) == 'chatfuel.user.id'] <- 'user_id'
dfc2 <- dfc2 %>% relocate(template_id) %>% relocate(treatment_id) %>% relocate(concern_id) %>% relocate(user_id)
dfc2$concern_id <- as.numeric(dfc2$concern_id)
dfc2 <- dfc2 %>% filter(!is.na(concern_id))

# some concern_id are not correct numbers, delete those rows
table(dfc2$concern_id)
dfc2 <- dfc2 %>% filter(concern_id != 0.4 & concern_id != 1.4 & concern_id != 3.4 & concern_id != 8)
dfc2$concern_id <- as.integer(dfc2$concern_id)
table(dfc2$concern_id)

# map to correct concern_id
table(dfc2$template_id)
v1 <- c(1, 2, 3, 4, 5, 6, 7)
v2 <- c(7, 6, 2, 1, 3, 4, 5)
v3 <- c(3, 4, 6, 5, 1, 7, 2)
v4 <- c(6, 5, 1, 7, 4, 2, 3)
v5 <- c(2, 1, 7, 3, 6, 5, 4)
v6 <- c(4, 3, 5, 2, 7, 1, 6)
v7 <- c(5, 7, 4, 6, 2, 3, 1)
matrix <- rbind(v1, v2, v3, v4, v5, v6, v7)
ids <- list()
for (i in 1:nrow(dfc2)){
  templateId <- dfc2[i, 'template_id']
  concernId <- dfc2[i, 'concern_id']
  true_concern_id <- as.numeric(matrix[templateId, concernId])
  ids <- c(ids, true_concern_id)
}
dfc2$concern_id <- unlist(ids)

# some treatment_id are 9s, delete these rows
# dfc2_na <- dfc2 %>% filter(is.na(dfc2$treatment_id)) # no NAs in treatment_id
# View(dfc2_na)
table(dfc2$treatment_id)
dfc2 <- dfc2 %>% filter(treatment_id != 9)
table(dfc2$treatment_id)

# check interplay of concern_id & treatment_id 
table(dfc2$concern_id, dfc2$treatment_id)
dfc2 <- dfc2 %>% filter(!(concern_id == 2 & treatment_id == 16))
aggregate(dfc2$treatment_id, by = list(dfc2$concern_id), function(x) names(table(x)))



# test extra rows in heroku data
dfh2 <- dfh %>% select(user_id, concern_id, treatment_id, engage_id, success)
dfh_extra_1 <- anti_join(dfh2, dfc, by=c("user_id")) #0 extra user_ids in heroku data
dfh_extra_2 <- anti_join(dfh2, dfc2, by=c("user_id")) #182 extra user_ids in heroku data
dfh_compare1 <- dfh2 %>% filter(user_id %in% dfh_extra_2$user_id)
dfc_compare1 <- dfc %>% filter(user_id %in% dfh_extra_2$user_id)
View(dfh_compare1)
View(dfc_compare1)

dfh3 <- anti_join(dfh2, dfh_extra_2, by=c("user_id"))
dfh_extra_3 <- anti_join(dfh3, dfc2, by=c("user_id", "concern_id")) #593
dfh_compare2 <- dfh3 %>% filter(user_id %in% dfh_extra_3$user_id) #3.533298
dfc_compare2 <- dfc2 %>% filter(user_id %in% dfh_extra_3$user_id)
dfh_compare7 <- dfh2 %>% filter(user_id %in% dfh_extra_3$user_id)
dfc_compare7 <- dfc %>% filter(user_id %in% dfh_extra_3$user_id)
View(dfh_compare2)
View(dfc_compare2)
View(dfh_compare7)
View(dfc_compare7)
as.character(unique(dfh_extra_3$user_id)[1:10])
as.character((dfh_extra_3[order(dfh_extra_3$user_id),]$user_id)[1:10])
df_new <- dfh2 %>% filter(user_id == 3533298386794902)
df_new2 <- dfc %>% filter(user_id == 3533298386794902)
df_new3 <- dfc2 %>% filter(user_id == 3533298386794902)
View(df_new)
View(df_new2)
View(df_new3)

# get user_ids for Molly
table1 <- data.frame(table(dfh_compare2$user_id))
table1_single <- table1 %>% filter(Freq == 1)
table1_single
dfc_singled1 <- dfc2 %>% filter(user_id %in% table1_single$Var1) %>% mutate(id = as.character(user_id)) %>% select(id, concern_id)
dfc_singled1

dfh4 <- anti_join(dfh3, dfh_extra_3, by=c("user_id", "concern_id"))
dfh_extra_4 <- anti_join(dfh4, dfc2, by=c("user_id", "concern_id", "treatment_id")) #194
dfh_compare3 <- dfh4 %>% filter(user_id %in% dfh_extra_4$user_id) #4.554415
dfc_compare3 <- dfc2 %>% filter(user_id %in% dfh_extra_4$user_id)
View(dfh_compare3)
View(dfc_compare3)

# get user_ids for Molly
table2 <- data.frame(table(dfh_compare3$user_id))
table2_single <- table2 %>% filter(Freq == 1)
table2_single
dfc_singled2 <- dfc2 %>% filter(user_id %in% table2_single$Var1) %>% mutate(id = as.character(user_id)) %>% select(id, concern_id)
dfc_singled2


# test extra rows in chatfuel dataset
dfc_extra_1 <- anti_join(dfc2, dfh2, by=c("user_id")) #722 extra user_ids in chatfuel dataset
dfc_compare4 <- dfc2 %>% filter(user_id %in% dfc_extra_1$user_id) %>% relocate(success)

# get user_ids for Molly
dfc_compare9 <- dfc_compare4 %>% filter(!(is.na(success)))
View(dfc_compare9)
dfc_compare9_time <- dfc_compare9 %>% mutate(time = strptime(signed.up, format = "%Y-%m-%d %H:%M:%OS", tz = "EST"))
standard_time <- strptime("2022-1-11 17:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "EST")
dfc_timed1 <- dfc_compare9_time %>% filter(time > standard_time) %>% relocate(signed.up) %>%
  mutate(id = as.character(user_id)) %>% select(id, signed.up)
dfc_timed1

dfc3 <- anti_join(dfc2, dfc_extra_1, by=c("user_id"))  
dfc_extra_2 <- anti_join(dfc3, dfh2, by=c("user_id", "concern_id")) #53 
dfc_compare5 <- dfc3 %>% filter(user_id %in% dfc_extra_2$user_id) %>% relocate(success)
dfh_compare5 <- dfh2 %>% filter(user_id %in% dfc_extra_2$user_id)
View(dfc_compare5)
View(dfh_compare5)

# get user_ids for Molly
dfc_compare10 <- dfc_compare5 %>% filter(!(is.na(success)))
View(dfc_compare10)
dfc_compare10_time <- dfc_compare10 %>% mutate(time = strptime(signed.up, format = "%Y-%m-%d %H:%M:%OS", tz = "EST"))
dfc_timed2 <- dfc_compare10_time %>% filter(time > standard_time) %>% relocate(signed.up) %>% 
  mutate(id = as.character(user_id)) %>% select(id, signed.up, concern_id)
dfc_timed2


dfc4 <- anti_join(dfc3, dfc_extra_2, by=c("user_id", "concern_id"))
dfc_extra_3 <- anti_join(dfc4, dfh2, by=c("user_id", "concern_id", "treatment_id")) #17
dfc_compare6 <- dfc4 %>% filter(user_id %in% dfc_extra_3$user_id) %>% relocate(success)
dfh_compare6 <- dfh2 %>% filter(user_id %in% dfc_extra_3$user_id)
View(dfc_compare6)
View(dfh_compare6)

# get user_ids for Molly
dfc_compare11 <- dfc_compare6 %>% filter(!(is.na(success)))
View(dfc_compare11)
dfc_compare11_time <- dfc_compare11 %>% mutate(time = strptime(signed.up, format = "%Y-%m-%d %H:%M:%OS", tz = "EST"))
dfc_timed3 <- dfc_compare11_time %>% filter(time > standard_time) %>% relocate(signed.up) %>%
  mutate(id = as.character(user_id)) %>% select(id, signed.up, concern_id, treatment_id)
dfc_timed3















