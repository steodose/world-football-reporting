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
    select(Div:FTR) %>%
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
    select(Div:FTR) %>%
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
    select(Div:FTR) %>%
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
    select(Div:FTR) %>%
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
    select(Div:FTR) %>%
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
    select(League:PA) %>% 
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

# reorder coluns to match other leagues df
liga_1 <- liga_1 %>%
    rename("Div" = "League") %>%
    select(Div, Date, home_away, team, score, opp_score, Pts, win, 
           goal_diff, points_running, gd_running, wins_running, match_count)



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

# reorder coluns to match other leagues df
mls <- mls %>%
    rename("Div" = "League") %>%
    select(Div, Date, home_away, team, score, opp_score, Pts, win, 
           goal_diff, points_running, gd_running, wins_running, match_count)



## combine into one dataframe and specify the leagues

my_data_frames <- list(premier_league = premier_league,
                       bundesliga = bundesliga,
                       la_liga = la_liga,
                       ligue_1 = ligue_1,
                       serie_a = serie_a,
                       liga_1 = liga_1,
                       mls = mls)

all_leagues <- bind_rows(my_data_frames)


## ------------------------- Write to googlesheets for Tableau fun ------------------------------

# Need to do this only once to initialize new googlesheet. Creates a worksheet for each league. The spreadsheet (ie. workbook) name is matches_22/23.

#tableau <- gs4_create("matches_22/23", sheets = all_leagues)
#tableau

## Update data in Google Sheets (run this every week after the games are over)

# deauth
gs4_deauth()

# sheets reauth with specified token and email address
gs4_auth(
    cache = ".secrets",
    email = "steodose@gmail.com"
)


sheet_write(all_leagues, ss = "https://docs.google.com/spreadsheets/d/1wnNFwYEgUv6_O1RWzMzxWRFiIH3mTmjF39ESfb6A6Xk/edit#gid=893416354",
            sheet = "all_leauges")

# read into WD as well
write_csv(all_leagues, "all_leagues.csv")



