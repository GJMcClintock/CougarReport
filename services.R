library(glue)
library(stringr)
library(lubridate)
library(logging)
library(jsonlite)
basicConfig()

source("./utils.R")

# Configurable environment variables
selected_school <- Sys.getenv("TARGET_SCHOOL")
selected_school <- ifelse(is.na(selected_school) || str_length(selected_school) == 0, "Georgia Tech", selected_school)
target_year <- Sys.getenv("TARGET_YEAR")
target_year <- ifelse(is.na(target_year) || str_length(target_year) == 0, 2022, target_year)

send_empty_updates <- Sys.getenv("SEND_EMPTY_UPDATES")
send_empty_updates <- !(is.na(send_empty_updates) || str_length(send_empty_updates) == 0 || tolower(as.character(send_empty_updates)) == "false" || send_empty_updates == FALSE)

last_updated <- tryCatch(
    {
        tmp <- read_json("./last_updated.json")$date
        tmp <- trim(tmp)
        loginfo("Found last_updated file, using old date for comparison")
        ymd_hms(tmp, tz = "UTC")
    },
    error = function(cond) {
        loginfo("No last_updated file found, using current date")
        logerror(glue::glue("Error shown: {cond}"))
        now()
    }
)
loginfo(glue("Last updated recruits at: {last_updated}"))

httr::user_agent("Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:85.0) Gecko/20100101 Firefox/85.0")

## -- ACTUAL DATA SCRAPING --
# source("./RF Scraper.R") # Rivals scraper
source("./CB Scraper Run.R")  # 247 scraper

# ADD SERVICES HERE
#
# Slack
slack_enabled <- Sys.getenv("SLACK_ENABLED")
slack_enabled <- !(is.na(slack_enabled) || str_length(slack_enabled) == 0 || tolower(as.character(slack_enabled)) == "false" || slack_enabled == FALSE)
if (slack_enabled) {
    message("Service [Slack] is enabled, messages WILL be sent there")
    source("./slack.R")
} else {
    message("Service [Slack] is not enabled, messages will not be sent there")
}

# Twitter
twitter_enabled <- Sys.getenv("TWITTER_ENABLED")
twitter_enabled <- !(is.na(twitter_enabled) || str_length(twitter_enabled) == 0 || tolower(as.character(twitter_enabled)) == "false" || twitter_enabled == FALSE)
if (twitter_enabled) {
    message("Service [Twitter] is enabled, messages WILL be sent there")
    source("./twitter.R")
} else {
    message("Service [Twitter] is not enabled, messages will not be sent there")
}

