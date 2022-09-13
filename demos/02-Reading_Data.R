# Statcast: show clips.
# Observations and features (or variables).
# What variables would you want to record?
# Data types: numeric, integer, string, factor, boolean, datetime.
# Show raw file.

statcast <- read.csv("~/GitHub/MLB/data/statcast.csv")
head(statcast)
View(statcast)
str(statcast)
# Environment viewer.

# Discuss batter/pitcher variables.
chadwick <- read.csv("https://github.com/chadwickbureau/register/raw/master/data/people.csv")
View(chadwick)

# Spotify: show raw.
# Non-relational and relational databases: how do relational DBs know fields?
spotify <- jsonlite::fromJSON("./data/Spotify/endsong_0.json")
View(spotify)
# Relational databases and linking tables.


# Tips for loading your own dataset:
#  Inspect using text editor; check the file extension for clues.
#  Google an R function for reading the type of file.
#  If you don't get what you expected, look at the help page for that function.
