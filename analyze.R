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

#calculate avg and se, remove NAs
#NA sd means that there was only one result for a pollster
poll_results <- poll_spread %>%
  group_by(pollster) %>%
  summarize(avg = mean(spread), se=sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se) %>%
  drop_na()
poll_results

#Computing the posterior mean, standard error, credible interval and probability
#mu is the predicted spread, tau is the average spread in past years
mu <- 0
tau <- 0.035
sigma <- poll_results$se
Y <- poll_results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se
# 95% credible interval of spread
interval <- posterior_mean + c(-1.96, 1.96)*posterior_se
# probability of d > 0 (ie. DEM winning)
probability <- 1 - pnorm(0, posterior_mean, posterior_se)
mean(probability)

#add the probability of DEMS winning, subtract 50 to show difference
prediction <- poll_results %>% 
  mutate(prob = probability-0.5) %>% 
  arrange(desc(prob))
prediction
  
#plot the probability for each pollster
plot <- prediction %>% ggplot(aes(x=reorder(pollster,prob), y=prob, fill = prob>0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity") +
  labs(title = "Probability of DEM Winning by Pollster", 
       subtitle = "Last updated 06-16-2020",
       x = "Pollster Name", 
       y = "Probability of Win",
       caption = "Source: fivethirtyeight (https://projects.fivethirtyeight.com/polls-page/president_polls.csv)",
       fill = "Winning Party") +
  scale_fill_discrete(name = "Winning Party", labels = c("Republican", "Democrat"))
plot

# ggsave("figs/barplot.png")