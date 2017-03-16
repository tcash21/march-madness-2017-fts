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

tourney_seeds_numeric = tourney_seeds %>%
  mutate(
    seed_number = as.numeric(gsub("[ab]", "", gsub("[WXYZ]","",Seed)))
  ) %>%
  select(-Seed)

# Create general team features (not season-specific)
team_features = teams %>%
  select(Team_Id, Team_Name)

## calculates each teams' regular season stat averages
reg_season_stats <-
  all_reg_teams %>%
  select(-Daynum) %>%
  group_by(team, Season) %>%
  summarise_each(funs(mean))

reg_season_features = tourney_matchup_training %>%
  select(Season, team.1) %>%
  rename(Team_Id = team.1) %>%
  distinct(Season, Team_Id) %>%
  left_join(team_features) %>%
  left_join(reg_season_stats, by = c("Season", "Team_Id" = "team")) %>%
  left_join(tourney_seeds_numeric, by = c("Season", "Team_Id" = "Team")) %>%
  rename(reg_score = score, 
         reg_fgm = fgm,
         reg_fga = fga,
         reg_fgm3 = fgm3,
         reg_fga3 = fga3,
         reg_ftm = ftm,
         reg_fta = fta,
         reg_or = or,
         reg_dr = dr,
         reg_ast = ast,
         reg_to = to,
         reg_stl = stl, 
         reg_blk = blk,
         reg_pf = pf)
  


### Tourney seeds with Tourney Detailed Results
Tourney_delta_values <- tourney_det %>%
  left_join(reg_season_features, tourney_det, by = c("Season", 'Wteam' = 'Team_Id')) %>%
  rename(Winning_seed = seed_number) %>%
  mutate( Wscore_delta = (Wscore/reg_score)* 100,
          Wfgm_delta = (Wfgm / reg_fgm) * 100,
          Wfga_delta = (Wfga / reg_fga) * 100,
          Wfg_per_delta = ((Wfgm/Wfga)/(reg_fgm/reg_fga))* 100,
          Wfgm3_delta = (Wfgm3/reg_fgm3) * 100,
          Wfga3_delta = (Wfga3/reg_fga3) * 100,
          Wfg3_per_delta = ((Wfgm3/Wfga3)/(reg_fgm3/reg_fga3))* 100,
          Wftm_delta = (Wftm/reg_ftm) * 100,
          Wfta_delta = (Wfta/reg_fta) * 100,
          Wft_per_delta = ((Wftm/Wfta)/(reg_ftm/reg_fta))*100,
          Wor_delta = (Wor/reg_or)*100,
          Wdr_delta = (Wdr/reg_dr)*100,
          Wast_delta = (Wast/reg_ast)*100,
          Wto_delta = (Wto/reg_to)*100,
          Wstl_delta = (Wstl/reg_stl)*100,
          Wblk_delta = (Wblk/reg_blk) * 100,
          Wpf_delta = (Wpf/reg_pf)*100) %>%
  select(-c(reg_score, reg_fgm, reg_fga, reg_fgm3, reg_fga3, reg_ftm, reg_fta, reg_or, reg_dr, reg_ast, reg_to, reg_stl, reg_blk, reg_pf)) %>%
  left_join(reg_season_features, tourney_det, by = c("Season", 'Lteam' = 'Team_Id')) %>%
  rename(Losing_seed = seed_number) %>%
  mutate( Lscore_delta = (Lscore/reg_score)* 100,
          Lfgm_delta = (Lfgm / reg_fgm) * 100,
          Lfga_delta = (Lfga / reg_fga) * 100,
          Lfg_per_delta = ((Lfgm/Lfga)/(reg_fgm/reg_fga))* 100,
          Lfgm3_delta = (Lfgm3/reg_fgm3) * 100,
          Lfga3_delta = (Lfga3/reg_fga3) * 100,
          Lfg3_per_delta = ((Lfgm3/Lfga3)/(reg_fgm3/reg_fga3))* 100,
          Lftm_delta = (Lftm/reg_ftm) * 100,
          Lfta_delta = (Lfta/reg_fta) * 100,
          Lft_per_delta = ((Lftm/Lfta)/(reg_ftm/reg_fta))*100,
          Lor_delta = (Lor/reg_or)*100,
          Ldr_delta = (Ldr/reg_dr)*100,
          Last_delta = (Last/reg_ast)*100,
          Lto_delta = (Lto/reg_to)*100,
          Lstl_delta = (Lstl/reg_stl)*100,
          Lblk_delta = (Lblk/reg_blk) * 100,
          Lpf_delta = (Lpf/reg_pf)*100) %>%
  select(-c(reg_score, reg_fgm, reg_fga, reg_fgm3, reg_fga3, reg_ftm, reg_fta, reg_or, reg_dr, reg_ast, reg_to, reg_stl, reg_blk, reg_pf)) %>%
  select(c(Winning_seed, Losing_seed, Wscore_delta, Wfgm_delta, Wfga_delta, Wfg_per_delta, Wfgm3_delta, Wfga3_delta, Wfg3_per_delta, Wftm_delta, Wfta_delta, Wft_per_delta, 
           Wor_delta, Wdr_delta, Wast_delta, Wto_delta, Wstl_delta, Wblk_delta, Wpf_delta, Lscore_delta, Lfgm_delta, Lfga_delta, Lfg_per_delta, Lfgm3_delta, Lfga3_delta, 
           Lfg3_per_delta, Lftm_delta, Lfta_delta, Lft_per_delta, Lor_delta, Ldr_delta, Last_delta, Lto_delta, Lstl_delta,Lblk_delta,Lpf_delta))

### Match up delta values W vs L  
Tourney_matchUp_delta_values <- Tourney_delta_values %>%  
  group_by(Winning_seed, Losing_seed) %>%
  summarise_each(funs(mean))

### Number of times each matchup has happened
Tourney_matchup_numbers <- Tourney_delta_values %>%
  group_by(Winning_seed, Losing_seed) %>%
  summarise(Number = n())

Tourney_matchup_numbers$trying <- ifelse(Tourney_matchup_numbers$Winning_seed < Tourney_matchup_numbers$Losing_seed, paste(Tourney_matchup_numbers$Winning_seed, '-',
                                                                                                                            Tourney_matchup_numbers$Losing_seed),
                                         paste(Tourney_matchup_numbers$Losing_seed, '-', Tourney_matchup_numbers$Winning_seed))


