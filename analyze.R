#declare libraries
library(tidyverse)
library(ggplot2)
library(tidyr)

#_______________
# #download raw fivethirtyeight data
# url <- "https://projects.fivethirtyeight.com/polls-page/president_polls.csv"
# destination_file <- "data/polls.csv"
# download.file(url, destination_file)
# 
# #wrangle raw data
# polls <- read_csv("data/polls.csv")
# polls <- polls %>% subset(select = -c(race_id, seat_number, seat_number,
#                                       url, sponsor_ids, pollster_id,
#                                       poll_id, question_id, seat_name))
# 
# save(polls, file = "rdas/polls.rda")
# load("rdas/polls.rda")
#________________

"Biden" %in% polls$answer
select_col <- c("pollster", "state",
                "sample_size", 
                "end_date", "fte_grade",
                "answer", "pct")
select_grade <- c("A+", "A", "A-", "A/B", "B+", "B")

#select the desired columns, filter undesirable data
filtered_poll <- polls %>% subset(select = select_col) %>%
  filter(end_date >= "4/1/20",
         fte_grade == select_grade | is.na(fte_grade),
         answer == c("Biden","Trump")) 

#add party
#-1 represents REP, 1 represents DEM
poll_spread <- filtered_poll %>% 
  #removed spread column due to data quality
  mutate(party = ifelse(answer == "Trump", -1, 1))
poll_spread

#calculate avg and se, remove NAs
#NA sd means that there was only one result for a pollster
poll_results <- poll_spread %>%
  group_by(party) %>%
  summarize(avg = mean(pct), se=sd(pct)/sqrt(length(pct))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
poll_results

#subtract each number to calc avg spread
#if negative, rep; if positive, dem
spread <- poll_results[2, ] - poll_results[1, ]
spread

spread_mean <- spread$avg
spread_mean
spread_se <- spread$se
spread_se

# 95% credible interval of spread
interval <- spread_mean + c(-1.96, 1.96)*spread_se
interval
# probability of d > 0 (ie. DEM winning)
probability <- 1 - pnorm(0, spread_mean, spread_se)
mean(probability)

#add the probability of DEMS winning, subtract 50 to show difference
prediction <- poll_results %>% 
  mutate(prob = ifelse(sign(probability)==sign(party), 
                       probability, 
                       1-probability),
         par_name = ifelse(party == -1, "Rep", "Dem")) %>% 
  arrange(desc(prob))
prediction
  
#plot the probability for each party
plot <- prediction %>% ggplot(aes(x=reorder(par_name,prob), y=prob, fill = prob>0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity") +
  labs(title = "Probability of a Party Winning", 
       subtitle = "Last updated 07-08-2020",
       x = "Party Name", 
       y = "Probability of Win",
       caption = "Source: fivethirtyeight (https://projects.fivethirtyeight.com/polls-page/president_polls.csv)",
       fill = "Winning Party") +
  scale_fill_discrete(name = "Winning Party", labels = c("Republican", "Democrat"))
plot
#In the future, I would like to create a graph that looks like
#a "tug of war over the 50% line

ggsave("figs/barplot.png")
