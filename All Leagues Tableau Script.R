##### World Football Portal Matches Script #####
##### By: Stephan Teodosescu #####
##### August 2022 #####

library(tidyverse)
library(googlesheets4)


## -------------------------------- English Premier League --------------------------------------

epl_results <- read.csv("https://www.football-data.co.uk/mmz4281/2223/E0.csv", 
                        stringsAsFactors = FALSE)

# Process data frame to get one row per team-game
epl_results2 <- epl_results %>% 
    select(Date:FTR) %>%
    pivot_longer(contains('Team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
    mutate(score = ifelse(home_away == "HomeTeam", FTHG, FTAG),
           opp_score = ifelse(home_away == "AwayTeam", FTHG, FTAG),
           Pts = case_when(score > opp_score ~ 3,
                           score == opp_score ~ 1,
                           TRUE ~ 0),
           win = ifelse(Pts == 3, 1, 0))


# calculate the running counts for points, wins, and GD
premier_league <- epl_results2 %>% 
    select(-c(Time, FTHG, FTAG, FTR)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>%
    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())


## ------------------------------ Bundesliga ---------------------------------------------

bund_results <- read.csv("https://www.football-data.co.uk/mmz4281/2223/D1.csv", 
                         stringsAsFactors = FALSE)


# Process data frame to get one row per team-game
bund_results2 <- bund_results %>% 
    select(Date:FTR) %>%
    pivot_longer(contains('Team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
    mutate(score = ifelse(home_away == "HomeTeam", FTHG, FTAG),
           opp_score = ifelse(home_away == "AwayTeam", FTHG, FTAG),
           Pts = case_when(score > opp_score ~ 3,
                           score == opp_score ~ 1,
                           TRUE ~ 0),
           win = ifelse(Pts == 3, 1, 0))


# calculate the running counts for points, wins, and GD
bundesliga <- bund_results2 %>% 
    select(-c(Time, FTHG, FTAG, FTR)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 
    
    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())


## -------------------------- Spanish La Liga ------------------------------------------
esp_results <- read.csv("https://www.football-data.co.uk/mmz4281/2223/SP1.csv", 
                        stringsAsFactors = FALSE)


# Process data espame to get one row per team-game
esp_results2 <- esp_results %>% 
    select(Date:FTR) %>%
    pivot_longer(contains('Team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
    mutate(score = ifelse(home_away == "HomeTeam", FTHG, FTAG),
           opp_score = ifelse(home_away == "AwayTeam", FTHG, FTAG),
           Pts = case_when(score > opp_score ~ 3,
                           score == opp_score ~ 1,
                           TRUE ~ 0),
           win = ifelse(Pts == 3, 1, 0))


# calculate the running counts for points, wins, and GD
la_liga <- esp_results2 %>% 
    select(-c(Time, FTHG, FTAG, FTR)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 
    
    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())


## ----------------------- French Ligue 1 ---------------------------------

fr_results <- read.csv("https://www.football-data.co.uk/mmz4281/2223/F1.csv", 
                       stringsAsFactors = FALSE)


# Process data frame to get one row per team-game
fr_results2 <- fr_results %>% 
    select(Date:FTR) %>%
    pivot_longer(contains('Team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
    mutate(score = ifelse(home_away == "HomeTeam", FTHG, FTAG),
           opp_score = ifelse(home_away == "AwayTeam", FTHG, FTAG),
           Pts = case_when(score > opp_score ~ 3,
                           score == opp_score ~ 1,
                           TRUE ~ 0),
           win = ifelse(Pts == 3, 1, 0))


# calculate the running counts for points, wins, and GD
ligue_1 <- fr_results2 %>% 
    select(-c(Time, FTHG, FTAG, FTR)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 
    
    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())



## --------------------------- Serie A --------------------------------------

ita_results <- read.csv("https://www.football-data.co.uk/mmz4281/2223/I1.csv", 
                        stringsAsFactors = FALSE)


# Process data itaame to get one row per team-game
ita_results2 <- ita_results %>% 
    select(Date:FTR) %>%
    pivot_longer(contains('Team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
    mutate(score = ifelse(home_away == "HomeTeam", FTHG, FTAG),
           opp_score = ifelse(home_away == "AwayTeam", FTHG, FTAG),
           Pts = case_when(score > opp_score ~ 3,
                           score == opp_score ~ 1,
                           TRUE ~ 0),
           win = ifelse(Pts == 3, 1, 0))


# calculate the running counts for points, wins, and GD
serie_a <- ita_results2 %>% 
    select(-c(Time, FTHG, FTAG, FTR)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 
    
    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())


## ----------------------- Romanian Liga 1 --------------------

rou_results <- read.csv("https://www.football-data.co.uk/new/ROU.csv", 
                        stringsAsFactors = FALSE) %>% 
    filter(Season == "2022/2023")


# Process data frame to get one row per team-game
rou_results2 <- rou_results %>% 
    select(Country:PA) %>% 
    pivot_longer(Home:Away, names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
    mutate(score = ifelse(home_away == "Home", HG, AG),
           opp_score = ifelse(home_away == "Away", HG, AG),
           Pts = case_when(score > opp_score ~ 3,
                           score == opp_score ~ 1,
                           TRUE ~ 0),
           win = ifelse(Pts == 3, 1, 0))


# calculate the running counts for points, wins, and GD
liga_1 <- rou_results2 %>% 
    select(-c(HG, AG, Res)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 
    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())


## ---------------------------------- Major Laague Soccer --------------------------------------

mls_results <- read.csv("https://www.football-data.co.uk/new/USA.csv", 
                        stringsAsFactors = FALSE) %>% 
    filter(Season == 2022)


# Process data frame to get one row per team-game
mls_results2 <- mls_results %>% 
    select(Country:PA) %>% 
    pivot_longer(Home:Away, names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
    mutate(score = ifelse(home_away == "Home", HG, AG),
           opp_score = ifelse(home_away == "Away", HG, AG),
           Pts = case_when(score > opp_score ~ 3,
                           score == opp_score ~ 1,
                           TRUE ~ 0),
           win = ifelse(Pts == 3, 1, 0))


# calculate the running counts for points, wins, and GD
mls <- mls_results2 %>% 
    select(-c(HG, AG, Res)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 
    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())


## Create list of data frames to be fed into one Google Sheet
my_data_frames <- list(premier_league = premier_league,
                       bundesliga = bundesliga,
                       la_liga = la_liga,
                       ligue_1 = ligue_1,
                       serie_a = serie_a,
                       liga_1 = liga_1,
                       mls = mls)


## Write to googlesheets for Tableau fun

# Need to do this only once to initialize new googlesheet. Creates a sheet for each league. The workbook name is matches 22/23.

# tableau <- gs4_create("matches_22/23", sheets = my_data_frames)
# tableau



## Update data (run this every week after the games are over)
#sheet_write(matchday_table, ss = "https://docs.google.com/spreadsheets/d/1s9bxaqr98KT35OaScBPjWfL8rEqfErxW_4OorxiOZCM/edit#gid=594354188", sheet = "matchday_table")

