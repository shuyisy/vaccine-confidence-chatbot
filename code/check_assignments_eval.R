# Read in data ----
ff <- file.info(list.files('../data', pattern = 'Data_For_Society', full.names = TRUE))
file <- rownames(ff)[which.max(ff$mtime)]
df <- read.csv(file)

# check the vaccine status of the in-take survey takers for the whole dataset
df_temp <- df %>% filter(vax_status == "No" | vax_status == "Yes, 1 dose" | vax_status == "Yes, 2 doses or more")
table(df_temp$vax_status)

# check the vaccine status of the in-take survey takers for the evaluation dataset
standard_time <- strptime("2022-02-03 12:00:00", format = "%Y-%m-%d %H:%M:%OS", tz = "EST")
df_temp <- df %>% mutate(sign_time = strptime(signed.up, format = "%Y-%m-%d %H:%M:%OS", tz = "EST"))
df_temp <- df_temp %>% filter(sign_time > standard_time)
df_temp <- df_temp %>% filter(vax_status == "No" | vax_status == "Yes, 1 dose" | vax_status == "Yes, 2 doses or more")
table(df_temp$vax_status)

# select only users assigned to one of the three evaluation treatment groups
df <- df[which(!is.na(df$survey_group)),]


# resort concerns based on templates

df$concern_id <- as.numeric(df$concern_id)
df$concern_id1 <- as.numeric(df$concern_id1)
df$concern_id2 <- as.numeric(df$concern_id2)
df$concern_id3 <- as.numeric(df$concern_id3)

templatel <- list(
  c(1, 2, 3, 4, 5, 6, 7),
  c(7, 6, 2, 1, 3, 4, 5),
  c(3, 4, 6, 5, 1, 7, 2),
  c(6, 5, 1, 7, 4, 2, 3),
  c(2, 1, 7, 3, 6, 5, 4),
  c(4, 3, 5, 2, 7, 1, 6),
  c(5, 7, 4, 6, 2, 3, 1)  
)

for(i in 1:length(templatel)){
  df$concern_id[which(df$template_id == i)] <- templatel[[i]][df$concern_id[which(df$template_id == i)]]
  df$concern_id1[which(df$template_id == i)] <- templatel[[i]][df$concern_id1[which(df$template_id == i)]]
  df$concern_id2[which(df$template_id == i)] <- templatel[[i]][df$concern_id2[which(df$template_id == i)]]
  df$concern_id3[which(df$template_id == i)] <- templatel[[i]][df$concern_id3[which(df$template_id == i)]]
}

# check that only people in survey group 2 are receiving messages
table(df$survey_group, df$treatment_id1)
table(df$survey_group, df$treatment_id2)
table(df$survey_group, df$treatment_id3)

# check that messages are on policy
table(df$concern_id1, df$treatment_id1)
table(df$concern_id, df$treatment_id)
table(df$concern_id, df$treatment_id)
table(df$concern_id, df$treatment_id)


# check for users that are missing treatment id; did they stop before responding?
as.character(df$messenger.user.id[which(df$survey_group ==2 & is.na(df$treatment_id))])


# delete those who were assigned to survey_group 2 but did not proceed to get the treatment_id assigned
df <- df %>% filter(!(survey_group == 2 & is.na(treatment_id)))

# turn responses into numeric values
df <- df %>% 
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
                                   TRUE ~ NA_real_)
  )

# impute for post-treatment responses
df[is.na(df$willingness_2), "willingness_2"] <- df[is.na(df$willingness_2), "willingness"]
df[is.na(df$get_vaccinated_2), "get_vaccinated_2"] <- df[is.na(df$get_vaccinated_2), "get_vaccinated"]
df[is.na(df$get_vaccinated_2), "get_vaccinated_2"] <- mean(df$get_vaccinated_2, na.rm=TRUE)

# standardize responses
df <- df %>% mutate_at(c("willingness_2", "get_vaccinated_2"), ~(scale(.) %>% as.vector))
df <- df %>% mutate(response = willingness_2+get_vaccinated_2)

# compare across groups for responses
(df %>% group_by(survey_group) %>% summarize(mean = mean(response)))

# compare across groups for sharing the WHO posts, with imputing zeros and standardization
df <- df %>% mutate(share_post_numeric = case_when(share_post == "Yes" ~ 1, 
                                                   share_post == "No" ~ 0,
                                                   TRUE ~ 0))
df <- df %>% mutate_at(c("share_post_numeric"), ~(scale(.) %>% as.vector))
(df %>% group_by(survey_group) %>% summarize(mean = mean(share_post_numeric)))

# compare across groups for sharing the true stimuli, with imputing zeros and standardization
df <- df %>% mutate(share_true_numeric = case_when(share_true == "Yes" ~ 1, 
                                                   share_true == "No" ~ 0, 
                                                   TRUE ~ 0))
df <- df %>% mutate_at(c("share_true_numeric"), ~(scale(.) %>% as.vector))
(df %>% group_by(survey_group) %>% summarize(mean = mean(share_true_numeric)))

# compare across groups for sharing the false stimuli, with imputing zeros and standardization
df <- df %>% mutate(share_false_numeric = case_when(share_false == "Yes" ~ 1, 
                                                    share_false == "No" ~ 0, 
                                                    TRUE ~ 0))
df <- df %>% mutate_at(c("share_false_numeric"), ~(scale(.) %>% as.vector))
(df %>% group_by(survey_group) %>% summarize(mean = mean(share_false_numeric)))







