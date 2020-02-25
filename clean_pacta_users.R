library(tidyverse)

USER.NAME <- sub("/.*","",sub(".*Users/","",getwd()))
DROPBOX.PATH <- paste0("C:/Users/",USER.NAME,"/Dropbox (2° Investing)/")
PACTA.PATH <- paste0(DROPBOX.PATH, "/2° Investing Team/1. RESEARCH/1. Studies (projects)/PACTA . Regulator Monitoring/UNPRI/User Data")
setwd(PACTA.PATH)

date <- "2020-01-30_new"

## import survey results
survey_results_raw <- read_csv(file = paste0("survey_email_",date,".csv"))

## import list of banned email and organization parts
banned_list <- read_csv(file = "banned_list.csv")

## remove banned entries from survey results
survey_results <- survey_results_raw %>%
  filter(!email %in% banned_list$banned_emails & !organization %in% banned_list$banned_organizations)

## joined peer group
peer_group <- survey_results %>%
  filter(join_peer_group == 1) %>%
  select( organization, email, country,institution_type) %>%
  distinct(email, .keep_all = TRUE)

## unique users
unique_users <- survey_results %>%
  select(email, country, organization) %>%
  distinct(email, .keep_all = TRUE)

## unique organizations
organizations <- survey_results %>%
  distinct(organization)

## count by institution type (distinct emails)
institution_count <- survey_results %>%
  distinct(email, .keep_all = TRUE) %>%
  group_by(institution_type) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

## count by country (distinct emails)
country_count <- survey_results %>%
  distinct(email, .keep_all = TRUE) %>%
  group_by(country) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

## unique uses per day
date_count <- survey_results %>%
  mutate(date1 = as.Date(created_at, format = "%m/%d/%Y")) %>%
  mutate(date2 = as.character(date1)) %>%
  mutate(date3 = str_replace(date2, "0018", "2018")) %>%
  mutate(date = as.Date(date3, format = "%Y-%m-%d")) %>%
  distinct(email, date, .keep_all = TRUE) %>%
  group_by(date) %>%
  summarize(uses = n())

ggplot(date_count, aes(date, uses)) + geom_line() + ylab("Uses per Day")


survey_results$EmailTag = str_split_fixed(survey_results$email,"@", n =2)[,2]

print(length(unique(survey_results$EmailTag)))


## unique organizations
organizations <- survey_results %>%
  distinct(organization)


write.csv(survey_results, paste0("User Data_",date,".csv"),row.names = F)
write.csv(peer_group, paste0("Peer Group_",date,".csv"),row.names = F)
  
