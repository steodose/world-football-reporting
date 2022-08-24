##### Premier League Matches Script for Tableau Dashboard #####
##### By: Stephan Teodosescu #####
##### April 2022 #####

library(tidyverse)
library(worldfootballR)
library(googlesheets4)


##### Load Data and Wrangle #####

# Data Scraping (not working bc of SSL certificate error when scraping Transfermarkt)

#matchday_table <- tm_matchday_table(country_name="England", start_year="2021", matchday=c(1:30))

# Alternatively load 2022-23 Premier League Game Data from football-data.com
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
matchday_table <- epl_results2 %>% 
    select(-c(Time, FTHG, FTAG, FTR)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 
    
    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())


##### Write to googlesheets for Tableau fun #####

# Need to do this only once to initialize new googlesheet. Creates a sheet called matchday_table. The workbook name is sheets.
#tableau <- gs4_create("matches", sheets = matchday_table)

# calling this will write to Googlesheets
#tableau

# update data (run this every week after the games are over)
sheet_write(matchday_table, ss = "https://docs.google.com/spreadsheets/d/1pS1jdqyP_dcMtEv2jjoHOGgZBVd0eUfzj6IX9A7lZYc/edit#gid=1752010332", 
            sheet = "matchday_table")

