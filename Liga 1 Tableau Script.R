##### 2021-22 Liga 1 (Romania) Matches Script for Tableau Dashboard #####
##### By: Stephan Teodosescu #####
##### May 2022 #####

library(tidyverse)
library(worldfootballR)
library(googlesheets4)


##### Load Data and Wrangle #####

# load MLS Game Data from football-data.com: https://www.football-data.co.uk/usa.php
rou_results <- read.csv("https://www.football-data.co.uk/new/ROU.csv", 
                        stringsAsFactors = FALSE) %>% 
    filter(Season == "2021/2022")


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
matchday_table_rou <- rou_results2 %>% 
    select(-c(HG, AG, Res)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 
    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())


##### Write to googlesheets for Tableau fun #####

# Need to do this only once to initialize new googlesheet. Creates a sheet called matchday_table. The workbook name is sheets.
#tableau_rou <- gs4_create("matches_rou", sheets = matchday_table_rou)

# calling this will write to Googlesheets
#tableau_rou

# update data (run this every week after the games are over)
sheet_write(matchday_table_mls, ss = "https://docs.google.com/spreadsheets/d/1IKLODOE_LLCIkMYrCN9Jpbo_mt479n5iRxMMZI0n4Qc/edit#gid=1184361520", 
            sheet = "matchday_table_mls")


