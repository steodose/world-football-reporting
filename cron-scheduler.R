require("cronR")
# list the contents of a crontab
cron_ls()

# list the full path of where the rscript is located
path = "/Users/Stephan/Desktop/R Projects/world-football-reporting/Big_5_Leagues.R"

# Create a command to execute an R-script
cmd = cron_rscript(path)

# add the command and specify the days/times to start
cron_add(command= cmd, frequency = 'daily', at="10:55", days_of_week = c(1:4),
         id = 'BigFiveLeagues', description = 'Big Five Leagues Update')

# remove it by 'id'
cron_rm(id = "BigFiveLeagues")


# cron schedule for days of the week:
# 0 - Sunday
# 1 - Monday
# 2 - Tuesday
# 3 - Wednesday
# 4 - Thursday
# 5 - Friday
# 6 - Saturday
# 7 - Sunday