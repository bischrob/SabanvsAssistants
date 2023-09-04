library(tidyverse)
library(magrittr)
library(cfbfastR)

# teams = cfbd_team_info(only_fbs = T)

schedule = map_df(2010:2022,function(y)cfbd_game_info(year = y, team = "Alabama",season_type = 'both'))

ssn = 2020:2022
tm = "Texas"
schedule %>%
  filter(season %in% ssn) %>%
  filter(home_team == tm | away_team == tm) %>%
  select(game_id, season, home_team, away_team)

games = c(
  302962633,
  312950333,
  322942633,
  332640333,
  400852681,
  400926943,
  310010127,
  400852732,
  312740057,
  400548384,
  401110794,
  400933827,
  401012284,
  401110822,
  401237093,
  401282103,
  401403909,
  401056705,
  401237107,
  401331242,
  401282154,
  401012318,
  401110824,
  401237114,
  401012292,
  401237100,
  401282089,
  401403943,
  401403868
)

vsAssistants = schedule %>%
  filter(game_id %in% games)

game = cfbd_game_info(year = vsAssistants$season[1], game_id = vsAssistants$game_id[1],)

winProbs = map_df(1:nrow(vsAssistants),function(i){
  tryCatch(cfbd_metrics_wp_pregame(year = vsAssistants$season[i],week = vsAssistants$week[i],team = vsAssistants$away_team[i],season_type = vsAssistants$season_type[i]),error = function(e)return(tibble()))
})

results = inner_join(vsAssistants,winProbs)
sim = bind_rows(results %>% select(game_id,season,team = home_team,win_prob = home_win_prob, opponent = away_team, opponent_win_prob = away_win_prob),
                results %>% select(game_id,season,team = away_team,win_prob = away_win_prob, opponent = home_team, opponent_win_prob = home_win_prob)
) %>%
  filter(team == "Alabama")

# simulate
record = NULL
for(i in 1:10000){
sim = sim %>%
  mutate(win = case_when(runif(n = 1) < win_prob~TRUE,TRUE~FALSE))
record = c(record,sum(sim$win == T))
# print("record")
# print(paste(sum(sim$win == TRUE),"&",sum(sim$win == F)))
}
sum(record >= 22) / length(record)
hist(record)
recorddf = tibble(wins = record)
recorddf %>%
  ggplot(aes(x = wins)) +
  geom_histogram(bins = 24,boundary = 0) +
  geom_vline(xintercept = 22.5, linewidth = 2, color = "red") +
  theme_light() +
  ggtitle("Saban's Simulated Win Totals vs Assistants*") +
  labs(caption = "*Only includes last 25 games") +
  ylab("number of simulations")
ggsave("SabanSimulatedWins.png",dpi = 300)
