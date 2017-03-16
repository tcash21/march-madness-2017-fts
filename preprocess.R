library(readr)
library(dplyr)
library(feather)
library(RcppRoll)

## read in the kaggle provided datasets
reg_season_det <- read.csv("data/kaggle/RegularSeasonDetailedResults.csv")
reg_season_comp <- read.csv("data/kaggle/RegularSeasonCompactResults.csv")

seasons <- read.csv("data/kaggle/Seasons.csv")
teams <- read.csv("data/kaggle/Teams.csv")
tourney_compact_results <- read.csv("data/kaggle/TourneyCompactResults.csv")
tourney_det <- read.csv("data/kaggle/TourneyDetailedResults.csv")
tourney_seeds <- read.csv("data/kaggle/TourneySeeds.csv")
tourney_slots <- read.csv("data/kaggle/TourneySlots.csv")
submission <- read.csv("data/kaggle/sample_submission.csv")

## Disaggregate reg season games into individual team stats, one row for each team
## Better ways to do this?
wteam <- reg_season_det[c("Season", "Daynum", colnames(reg_season_det)[grep("^W", colnames(reg_season_det))])]
lteam <- reg_season_det[c("Season", "Daynum", colnames(reg_season_det)[grep("^L", colnames(reg_season_det))])]

colnames(wteam) <- gsub("^W", "", colnames(wteam))
colnames(lteam) <- gsub("^L", "", colnames(lteam))

wteam <- subset(wteam, select=-c(loc))
all_reg_teams <- rbind(wteam, lteam)
all_reg_teams <- all_reg_teams[order(all_reg_teams$team, all_reg_teams$Season, all_reg_teams$Daynum),]



# Numeric seeds
tourney_seeds_numeric = tourney_seeds %>%
  mutate(
    seed_number = as.numeric(gsub("[ab]", "", gsub("[WXYZ]","",Seed)))
  ) %>%
  select(-Seed)



# Tourney games is going to be our basic data structure as we are interested in predicting
# the outcome of tournament games.

tourney_matchup_training = tourney_compact_results %>%
  select(Season, Wteam, Lteam) %>%
  rename(team.1 = Wteam, team.2 = Lteam) %>%
  mutate(team.1.win = 1) %>%
  union(tourney_compact_results %>%
          select(Season, Wteam, Lteam) %>%
          rename(team.2 = Wteam, team.1 = Lteam) %>%
          mutate(team.1.win = 0))


# Create general team features (not season-specific)
team_features = teams %>%
  select(Team_Id, Team_Name)




# Create team features by season

## Calculate moving averages, last 5 games
mav_last5 <- 
  all_reg_teams %>%
  group_by(team) %>%
  mutate(temp.lag1 = dplyr::lag(score, n = 1)) %>%
  mutate(fiveday = roll_mean(temp.lag1, 5, align = "right", fill=NA)) %>%
  group_by(Season, team) %>%
  top_n(n=1, wt=Daynum)
  

## calculates each teams' regular season stat averages
reg_season_stats <-
  all_reg_teams %>%
  select(-Daynum) %>%
  group_by(team, Season) %>%
  summarise_each(funs(mean))

team_features_by_season = tourney_matchup_training %>%
  select(Season, team.1) %>%
  rename(Team_Id = team.1) %>%
  distinct(Season, Team_Id) %>%
  left_join(team_features) %>%
  left_join(reg_season_stats, by = c("Season", "Team_Id" = "team")) %>%
  left_join(tourney_seeds_numeric, by = c("Season", "Team_Id" = "Team")) %>%
  left_join(mav_last5 %>% select(Season, team, fiveday), by = c("Season", "Team_Id" = "team"))


# Join team features to the matchup and create matchup-specific features
tourney_matchup_training = tourney_matchup_training %>%
  left_join(team_features_by_season, by = c("Season", "team.1" = "Team_Id")) %>%
  left_join(team_features_by_season, by = c("Season", "team.2" = "Team_Id"), suffix = c(".1", ".2")) %>%
# Currently the statistics features only go back to 2003
  filter(Season >= 2003) %>%
  mutate(
    # Derived features
    fgp.1 = fgm.1 / fga.1,
    fgp.2 = fgm.2 / fga.2,
    fgp.diff = fgp.2 - fgp.1,
    fgp3.1 = fgm3.1 / fga3.1,
    fgp3.2 = fgm3.2 / fga3.2,
    fgp3.diff = fgp3.2 - fgp3.1,
    ftp.1 = ftm.1 / fta.1,
    ftp.2 = ftm.2 / fta.2,
    ftp.diff = ftp.2 - ftp.1,
    fgm.diff = fgm.2 - fgm.1,
    fga.diff = fga.2 - fga.1,
    score.diff = score.2 - score.1,
    seed.diff = seed_number.2 - seed_number.1,
    fiveday.diff = fiveday.2 - fiveday.1,
    fgm3.diff = fgm.2 - fgm.1,
    fga3.diff = fga.2 - fga.1,
    ftm.diff = ftm.2 - ftm.1,
    pf.diff = pf.2 - pf.1,
    to.diff = to.2 - to.1,
    fta.diff = fta.2 - fta.1,
    or.diff = or.2 - or.1,
    dr.diff = dr.2 - dr.1,
    ast.diff = ast.2 - ast.1,
    stl.diff = stl.2 - stl.1,
    blk.diff = blk.2 - blk.1
  )


write_feather(tourney_matchup_training, 'data/processed/tourney_matchup_training.feather')
