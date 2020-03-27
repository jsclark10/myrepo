install.packages("tidyverse")

library("tidyverse")


PHI_tutorial_data <- read_csv("https://github.com/megha1106/HG_Tutorial/blob/master/PHI_tutorial_data.csv?raw=true")



PHI_tutorial_data <- read.csv("https://raw.githubusercontent.com/hockey-graphs/HG_intro_tutorial/master/PHI_tutorial_data.csv")

copy <-PHI_tutorial_data

goals <-PHI_tutorial_data %>%
  filter(event_type=="GOAL")

goals_5v5 <- PHI_tutorial_data %>%
  filter(event_type == "GOAL" &
           game_strength_state=="5v5")

goals_special_teams <- PHI_tutorial_data %>%
  filter(event_type =="GOAL" &
           game_strength_state =="5v4"|
           game_strength_state == "4v5")

goals_5v5_ST <- PHI_tutorial_data %>%
  filter(event_type=="GOAL" & 
           game_strength_state %in% c("5v5","5v4","4v5"))

goals_small <- PHI_tutorial_data %>%
  select(game_id,game_date,event_type,event_detail,event_team,event_player_1)


goal_variable <- PHI_tutorial_data %>%
  mutate(goal = ifelse(event_type == "GOAL", 1,0))

sum(goal_variable$goal)

count(goal_variable, event_type)

goals_by_game <- goal_variable %>%
  group_by(game_id) %>%
  summarize(total_goals = sum(goal))

goals_by_game_team <- goal_variable %>%
  group_by(game_id, event_team) %>%
  summarize(goals= sum(goal))

goals_by_game_team <- goal_variable %>%
  filter(!is.na(event_team)) %>%
  group_by(game_id, event_team) %>%
  summarize(goals = sum(goal))

goals_by_game_team <- goals_by_game_team %>%
  arrange(desc(goals))

ggplot(data = PHI_tutorial_data) + 
  geom_bar(aes(x = event_zone))

ggplot(data = PHI_tutorial_data) + 
  geom_bar(aes(x = event_zone, fill = event_zone))

ggplot(data = PHI_tutorial_data) + 
  geom_bar(aes(x = event_zone, fill = event_zone)) +
  labs(y = "Number of Events")

