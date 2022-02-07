# Read in data ----
ff <- file.info(list.files('../data', pattern = 'Data_For_Society', full.names = TRUE))
file <- rownames(ff)[which.max(ff$mtime)]
df <- read.csv(file)

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
