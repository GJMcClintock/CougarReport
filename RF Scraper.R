library(rvest)
library(tidyverse)
library(lubridate)
library(glue)
library(httr)
library(jsonlite)
library(stringr)
library(logging)
basicConfig()

loginfo("Starting Rivals National FutureCast scraping...")

# Configurable environment variables
selected_rivals_prefix <- Sys.getenv("TARGET_RIVALS_PREFIX") # prefix for a team-specific rivals blog. Will usually be before the ".rivals.com" portion of the team site URL.
selected_rivals_prefix <- ifelse(is.na(selected_rivals_prefix) || str_length(selected_rivals_prefix) == 0, "byu", selected_rivals_prefix)

national_rivals_prefix <- "n" # national Rivals Futurecast Feed

query_futurecasts <- function(link) {
    rf_html <- read_html(link)

    # ----- FutureCast Forecasters -----
    loginfo(glue("Looking for Forecaster information at link {link}..."))
    forecast_nodes <- rf_html %>%
        html_nodes("[class^=\"ForecasterThumbnail_forecasterInformation__\"]")

    loginfo(glue("Found {length(forecast_nodes)} forecaster nodes. Parsing..."))
    forecasters <- data.frame(
        title = column(forecast_nodes, "[class^=\"ForecasterThumbnail_forecasterTitle__\"]"),
        first_name = column(forecast_nodes, "[class^=\"Link_link__1xDdm ForecasterThumbnail_forecasterName__\"] > div:nth_child(1)"),
        last_name = column(forecast_nodes, "[class^=\"Link_link__1xDdm ForecasterThumbnail_forecasterName__\"] > div:nth_child(2)"),
        accuracy = column(forecast_nodes, "[class^=\"ForecasterThumbnail_forecasterAccuracy__\"]"),
        stringsAsFactors = FALSE
    )

    loginfo(glue("Parsed {nrow(forecasters)} forecaster rows at link {link}. Cleaning..."))
    forecasters <- forecasters %>%
        mutate(
            title = trim(title),
            first_name = trim(first_name),
            last_name = trim(last_name),
            full_name = glue("{first_name} {last_name}"),
            accuracy = as.numeric(gsub("% accuracy", "", accuracy))
        ) %>%
        select(full_name, title, accuracy)

    loginfo(glue("Found/parsed/cleaned {nrow(forecasters)} forecaster rows"))

    # ----- FutureCasts -----
    loginfo(glue("Looking for recruit futurecast nodes at link {link}..."))

    fc_nodes <- rf_html %>%
        html_nodes("[class^=\"ForecastActivity_forecastActivity__\"] > [class^=\"ForecastActivity_activityText__\"]")

    loginfo(glue("Found {length(fc_nodes)} recruit futurecast nodes, parsing..."))
    ftrcsts <- data.frame(
        forecaster = column(fc_nodes, "[class^=\"ForecastActivity_forecastText__\"] > b:nth-child(1)"),
        recruit = column(fc_nodes, "[class^=\"ForecastActivity_forecastText__\"] > b:nth_child(2)"),
        profile_url = fc_nodes %>% html_node("[class^=\"ForecastActivity_forecastText__\"] > b:nth_child(2) > a") %>% html_attr('href'),
        full_text = column(fc_nodes, "[class^=\"ForecastActivity_forecastText__\"]"),
        time_since = column(fc_nodes, "[class^=\"ForecastActivity_forecastTime__\"]"),
        stringsAsFactors = FALSE
    )

    loginfo(glue("Found {nrow(ftrcsts)} recruit futurecast rows at link {link}..."))
    loginfo(glue("Cleaning data to properly format dates..."))

    now <- now(tz = "UTC")
    ftrcsts <- ftrcsts %>%
        mutate(
            forecaster = trim(forecaster),
            recruit = sapply(recruit, splitJoin),
            time_since = gsub(" ago", "", gsub("about ", "", time_since)),
            time_since = sapply(time_since, splitJoin),
            unit_elapsed = case_when(
                grepl("month", time_since) == TRUE ~ "month",
                grepl("s", time_since) == TRUE ~ "second",
                grepl("m", time_since) == TRUE ~ "minute",
                grepl("h", time_since) == TRUE ~ "hour",
                grepl("d", time_since) == TRUE ~ "day",
                TRUE ~ "unknown"
            ),
            value_elapsed = as.numeric(gsub("d", "", gsub("h", "", gsub("m", "", gsub("s", "", gsub("month", "", time_since)))))),
            date = case_when(
                unit_elapsed == "second" ~ (now %m-% seconds(value_elapsed)),
                unit_elapsed == "minute" ~ (now %m-% minutes(value_elapsed)),
                unit_elapsed == "hour" ~ (now%m-% hours(value_elapsed)),
                unit_elapsed == "day" ~ (now %m-% days(value_elapsed)),
                unit_elapsed == "month" ~ (now %m-% months(value_elapsed))
            ),
            player_id = as.numeric(sub(".*-", "", profile_url)),
            year = str_extract(full_text, "\\((\\d{4}),"),
            year = sub("\\D","", year),
            year = sub(",","", year),
            forecasted_team = str_extract(full_text, "to (.*)\\."),
            forecasted_team = sub("to ","", forecasted_team),
            forecasted_team = sub("\\.","", forecasted_team),
            forecasted_team = sub("is now unlikely", "", forecasted_team),
            forecasted_team = sapply(forecasted_team, splitJoin),
            elapsed = as.double(difftime(date,
                                         last_updated,
                                         units = "secs")),
            year = as.numeric(year),
            unlikely = if_else(grepl("unlikely", full_text, fixed = T), 1, 0),
            update = if_else(grepl("updates", full_text, fixed = T), 1, 0),
            original_school = if_else(update == 0, "None", str_extract(full_text, "from\\s+(.*)\\s+to\\s+")),
            original_school = sub("\\s+to\\s+","", original_school),
            original_school = sub("from\\s+","", original_school),
            original_school = sapply(original_school, splitJoin)
        ) %>%
        select(-time_since, -unit_elapsed, -value_elapsed)
    return(ftrcsts)
}

query_croots <- function(name, year) {
    body <- paste0("{
        \"search\": {
            \"member\": \"Prospect\",
            \"query\": \",",strip_suffix(name),",\",
            \"sport\": \"Football\",
            \"page_number\": \"1\",
            \"page_size\": \"50\",
            \"recruit_year\": \"",year,"\"
        }
    }")
    croot_req <- POST("https://n.rivals.com/api/v1/people", add_headers(
        "User-Agent"="Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:85.0) Gecko/20100101 Firefox/85.0",
        Accept="application/json, text/plain, */*",
        "Accept-Language"="en-US,en;q=0.5",
        Referer="https://n.rivals.com/search",
        "Content-Type"="application/json;charset=utf-8",
        "X-XSRF-TOKEN"="JWSTp21Yj1wpNjaVz7W7x/wNgbUCNxLTFP0oHbOtYWUXdXMt2hvX+NQ3ejC8as5FAb1RE/KwiH3bY4SlsEl+Sg==",
        "Origin"="https://n.rivals.com",
        "Connection"="keep-alive",
        "Cookie"="A1=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; A3=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; A1S=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; GUC=AQEBAQFgIB9gKEId1wSk; XSRF-TOKEN=JWSTp21Yj1wpNjaVz7W7x%2FwNgbUCNxLTFP0oHbOtYWUXdXMt2hvX%2BNQ3ejC8as5FAb1RE%2FKwiH3bY4SlsEl%2BSg%3D%3D; _rivalry_session_v2=ZWZZQVZSQVh3ZnNaSWxaZlgwNU95aXV3a3VFREM2SE9iME9wZG4yaERvd0hHNjJOekhGZG1BOHo4WDM5OFdCSnFCczRWQ0hKelRzMWJna1NDZXV3MndjNHhaZlU1S0ZEdmJFaTVUR1doQ1RHdUJhRlQxUis2cWRBblUyVlhKcXVnVHE0aFlDODZscVMvQWxwRWxQUmpwUGk4QWdXRWUwdTlIZ2lxUk5YdzlJa3RxVVdac3lBdGRiQkxBOEErYXl2djNGT1dXa1g0c3ozSHJJdDZyMkNFZmd0eDZhUmE5TVFHU2RLc045RHlqMFFjUjJvbzVVOU9MNG9ORU9qdHY4K21wTjdtVlNVVU1KWTFNWlY0MUp6eDY3cnNZMHdNYVg5aGQ5TUZoM2doZnZEK1NvWkRkNjBiVVFlYmlyTzVqb21WVFlRRXlmWEVZSmtHZTVsc0tENkdBPT0tLVNNQmpEbzFNMGI1blVuKzVyR1Q3SFE9PQ%3D%3D--0947bcffa7fd9b9703f5522cb9afa8fac7d83254; GUCS=Ae_tyY5k; ywandp=10002066977754%3A1333922239; _cb_ls=1; _ga=GA1.2.770716812.1612631490; _gid=GA1.2.1018170693.1612631490; _gat=1; A1=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; A1S=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; A3=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; GUC=AQEBAQFgIB9gKEId1wSk; _rivalry_session_v2=QW9Cckk0MGF3bnZtSFBLMFVhNlp3M281bFkzajM0dW9KWjh5clJEUWNNVytqU0w5dDg4OGJFY0NiMDNLZ0N1b3g2NEhWcVNvWm40QVdDNmlROGozMXJGazdMQmsrWUo0ZitnZkhHSFcweERuQkwxNUdQdmFESng0bTdPZlJPcTlhYy94QndBNlNWdERSMzRMVWtSbHFxT1l1VlB3eC9TbGN0OFJId2R0S1AwRE16R09GcDArbDVSUVFWcldxQzFoekJibkxNWjVQOVlRZS9vZVBkcTVpWkFqYUFIRkhLMVluaktxdllUNDRUWGw2SjdteDR3K2VHbmk0TytGWFZHdzVJaUNhanN3OFNlbDlxVGMxVEFNeXUrWFVORmt2N0ZkYnVBRUVEaEtJRTBjbHUzSUxiemRiZU8wN3hqZGJDcFA4Nnk3eDJHOGVRVzdmb2pRYUNNNDJnPT0tLXJ4c1ZUUjFXY0dqUHltWE1sSitjUHc9PQ%3D%3D--ab9e9cd73c80953acce2a86a86eb53ab3b0bfabf",
        "TE"="Trailers"
    ), body = body, encode = "json")

    result <- content(croot_req, "text")
    result <- fromJSON(result)$people
    return(result)
}

get_croot_info <- function(name, player_id, year) {
    result <- query_croots(name, year)
    result <- result %>%
        mutate(
            year = as.numeric(year)
        ) %>%
        filter(
            as.numeric(prospect_id) == as.numeric(player_id)
        ) %>%
        head(1)
    return(result)
}

futurecasts <- data.frame()
links <- c(selected_rivals_prefix, national_rivals_prefix)
for (item in links) {
    tmp = query_futurecasts(glue("https://{item}.rivals.com/futurecast"))
    futurecasts <- rbind(futurecasts, tmp)
}
loginfo(glue("Found {nrow(futurecasts)} FutureCasts from team and national feeds"))

expanded_data <- data.frame()
player_slim_list <- futurecasts %>%
    select(recruit, player_id, year) %>% group_by(player_id, year) %>%
    summarise(recruit = first(recruit))
total <- nrow(player_slim_list)

if (total > 0) {
    loginfo(glue("Retrieving expanded info from Rivals API for {nrow(futurecasts)} FutureCasts"))
    for (row in 1:total) {
        player_id <- player_slim_list[row, "player_id"]
        name <- player_slim_list[row, "recruit"]
        year  <- player_slim_list[row, "year"]

        tryCatch(
            {
                loginfo(glue("Starting loading {row}/{total}: {name} (ID: {player_id}, Year: {year})"))
                result <- get_croot_info(name, player_id, year)
                if (nrow(result) > 0) {
                    colleges_interested_str = ""
                    if (length(result$prospect_colleges) > 0 && !is.na(result$prospect_colleges)) {
                        colleges_interested_tmp = result$prospect_colleges[[1]]
                        if (nrow(colleges_interested_tmp) > 0) {
                            colleges_interested_str = paste(colleges_interested_tmp$college_common_name, collapse=', ')
                        } else {
                            colleges_interested_str = ""
                        }
                    } else {
                        colleges_interested_str = ""
                    }

                    result$colleges_interested <- c(colleges_interested_str)
                    expanded_data <- rbind(expanded_data, result %>% select(-prospect_colleges))
                } else {
                    loginfo(glue("Did not find Rivals API data for {name} (ID: {player_id}, Year: {year})"))
                }
            },
            error = function(cond) {
                logwarn(paste("Error: ", cond))
            },
            finally = {
                loginfo(glue("Done loading {row}/{total}: {name} (ID: {player_id}, Year: {year})"))
            }
        )
    }

    expanded_data <- expanded_data %>%
        mutate(
            year = as.numeric(year)
        )

    futurecasts <- left_join(futurecasts, expanded_data, by = c("player_id" = "prospect_id", "year" = "year"))
    loginfo(glue("Found and joined expanded info for {nrow(futurecasts)} total FutureCasts"))

    loginfo(glue("Filtering {nrow(futurecasts)} total FutureCasts based on criteria: school ({selected_school}), year ({target_year}), and time since last updated ({last_updated})"))
    futurecasts <- futurecasts %>%
        filter(
            (grepl(selected_school, forecasted_team) == TRUE) &
            (unlikely == 0) &
            (elapsed >= 0) &
            (as.numeric(year) == as.numeric(target_year)) &
            (grepl(selected_school, colleges_interested) == TRUE)
        )
    loginfo(glue("Found {nrow(futurecasts)} FutureCasts that match criteria, sending to posting services"))
} else {
    loginfo(glue("Did not have any FutureCasts to find expanded info for, returning empty dataframe to posting services"))
    futurecasts <- data.frame()
}
