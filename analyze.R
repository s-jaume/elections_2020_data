# #download raw fivethirtyeight data
# url <- "https://projects.fivethirtyeight.com/polls-page/president_polls.csv"
# destination_file <- "data/polls.csv"
# download.file(url, destination_file)
# 
# wrangle raw data
# polls <- read_csv("data/polls.csv")
# polls <- polls %>% subset(select = -c(race_id, seat_number, seat_number,
#                                       url, sponsor_ids, pollster_id,
#                                       poll_id, question_id, seat_name))
# 
# save(polls, file = "rdas/polls.rda")
# load("rdas/polls.rda")

#declare libraries
library(tidyverse)
library(ggplot2)

"Biden" %in% polls$answer
select_col <- c("pollster", "state",
                "sample_size", 
                "end_date", "fte_grade",
                "answer", "pct")
select_grade <- c("A+", "A", "A-", "A/B", "B+")

#select the desired columns, filter undesirable data
filtered_poll <- polls %>% subset(select = select_col) %>%
  filter(end_date >= "4/1/20",
         fte_grade == select_grade | is.na(fte_grade),
         answer == c("Biden","Trump")) 
filtered_poll

#add spread and party
#-1 represents REP, 1 represents DEM
poll_spread <- filtered_poll %>% 
  mutate(party = ifelse(answer == "Trump", -1, 1),
         spread = party*abs(2*pct - 100))
poll_spread

#calculate avg and se
#NA sd means that there was only one result for a pollster
poll_calc <- poll_spread %>%
  group_by(pollster) %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
poll_calc

#TEST ZONE
####################

##################

# ggsave("figs/barplot.png")

