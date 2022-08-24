##### 2021-22 Big 5 European Leagues Results for Tableau Dashboard #####
##### By: Stephan Teodosescu #####
##### June 2022 #####

library(tidyverse)
library(worldfootballR)
library(googlesheets4)


##### Load Data and Wrangle #####

league_tables <- get_season_team_stats(country = c("ENG", "GER", "FRA", "ESP", "ITA"), 
                                      gender = "M", season_end_year = c(2012:2023), 
                                      tier = "1st", stat_type = "league_table")



##### Write to googlesheets for Tableau fun #####

# Need to do this only once to initialize new googlesheet. Creates a sheet called matchday_table. The workbook name is sheets.
#tableau_big5 <- gs4_create("big_5_league_tables", sheets = league_tables)

# calling this will write to Googlesheets
#tableau_big5

# update data (run this every week after the games are over)
sheet_write(league_tables, ss = "https://docs.google.com/spreadsheets/d/1wGEV-jvE3N07ZynqaVFSB3VJl4Ms7iWvwIhIQVmVqcM/edit#gid=780773239", 
            sheet = "league_tables")




