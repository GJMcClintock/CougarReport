library(rvest)
library(tidyverse)
library(lubridate)
library(glue)
library(logging)
library(httr)
basicConfig()

loginfo(glue("Starting 247 Crystal Ball scraping for target year {target_year}..."))

# Configurable environment variables
selected_247_prefix <- Sys.getenv("TARGET_247_PREFIX") # prefix for a team-specific 247Sports blog. Will usually be after the "https://247sports.com/college/" portion of the team site URL.
selected_247_prefix <- ifelse(is.na(selected_247_prefix) || str_length(selected_247_prefix) == 0, "byu", selected_247_prefix)

national_url <- paste0("https://247sports.com/Season/",target_year,"-Football/TargetPredictions/")
team_url <- paste0("https://247sports.com/college/",selected_247_prefix,"/Season/",target_year,"-Football/CurrentTargetPredictions/")

query_crystal_balls <- function(url) {
  # # check connection
  # test_result <- GET(url = year_url)
  # loginfo(glue("GET Result test status code: {test_result$status_code}"))
  # loginfo(glue("any http errors? {http_error(test_result)}"))
  
  loginfo(glue("Scraping 247 URL at {url}"))
  
  cb <- read_html(url)
  
  span <- cb %>%
    html_elements("span") %>%
    html_attr("class")
  
  loginfo(glue("Found {length(span)} 247 Crystal Balls, parsing..."))
  
  image_names<-cb %>% html_nodes("img") %>% html_attr("alt")
  image_height<-cb %>% html_nodes("img") %>% html_attr("height")
  image_class<-cb %>% html_nodes("img") %>% html_attr("class")
  plinks<-cb %>% html_nodes("a") %>% html_attr("href")
  pred_date<-cb %>% html_elements(".prediction-date") %>% html_text()
  player_names<-cb %>% html_nodes(".name")
  names <- html_children(player_names)
  predictor_names<-cb %>% html_nodes(".predicted-by") %>% html_nodes("a") %>%
    html_nodes("span") %>% html_text()
  forecaster_links <- cb %>% html_elements(".predicted-by") %>% html_elements("a") %>% html_attr("href")
  flinks <- data.frame(link = forecaster_links, number = 1:length(forecaster_links))
  confidence<-cb %>% html_nodes(".confidence") %>% html_nodes(".confidence-wrap") %>%
    html_text()
  confidence <- data.frame(confidence <- confidence)
  sep <- confidence %>% separate(col = confidence....confidence, into = c("A", "B", "C"), sep = "                ")
  confidence <- sep$B
  confidence <- gsub("\n","",confidence)
  
  player_info <- data.frame()
  
  rng <- length(player_names)-1
  
  for(i in 0:rng) {
    info <- data.frame(name = html_text(names[[((i*3)+1)]]),
                       pos = html_text(names[[((i*3)+2)]]),
                       rank = html_text(names[[((i*3)+3)]]))
    
    player_info <- bind_rows(player_info, info)
  }
  
  player_info$number <- 1:nrow(player_info)
  
  images <- data.frame(names = image_names, height = as.integer(image_height), class = image_class)
  images$class <- replace_na(images$class, "")
  teams <- images %>% dplyr::filter(height == 24, class != "old")
  
  zero <- data.frame(name = span)
  zero <- zero %>% dplyr::slice(31:((nrow(player_info)*14)+31))
  
  zeroes <- which(zero == "icon-zero")
  emptys <- floor(zeroes/14)
  emptys <- sort(emptys)
  
  if(length(emptys!=0)) {
    teams$number <- 1:nrow(teams)
    for(i in 1:length(emptys)) {
      current_empty <- emptys[i]
      cut <- teams %>% dplyr::filter(number>(current_empty-1))
      teams <- teams %>% dplyr::filter(number<(current_empty))
      cut <- cut %>% mutate(new = number+1) %>% select(-number, number = new)
      teams <- bind_rows(teams, cut)
      new_row <- data.frame(names = "icon-zero", height = 24, number = current_empty)
      teams <- bind_rows(teams, new_row)
    }
  } else{teams$number <- 1:nrow(player_info)}
  pred_date <- as.data.frame(pred_date)
  pred_date$number = 1:nrow(pred_date)
  teams <- left_join(teams, pred_date, by="number")
  
  targets <- data.frame(plink = plinks)
  sep <- targets %>% separate(col = plink, into = c("prefix", "body"), sep = 8)
  sep <- sep %>% separate(col = "body", into = c("site", "body"), sep = 9)
  sep <- sep %>% separate(col = "body", into = c("suffix", "body"), sep = 5)
  sep <- sep %>% separate(col = "body", into = c("type", "body"), sep = 6)
  targets <- targets %>% mutate(site = sep$site, type = sep$type) %>% filter(type == "Player")
  targets$number = 1:nrow(targets)
  
  new_names <- data.frame(name = player_info$name)
  sep <- new_names %>% separate(col = name, into = c("A", "B", "C", "D", "E"), sep = "                ")
  player_info$name <- sep$B
  player_info$name <- gsub("\n","",player_info$name)
  
  new_pos <- data.frame(pos = player_info$pos)
  sep <- new_pos %>% separate(col = pos, into = c("A", "B", "C"), sep = "/")
  new_pos$pos <- sep$A
  new_pos$ht <- sep$B
  new_pos$wt <- sep$C
  new_pos$pos <- gsub("\n                ","",new_pos$pos)
  sep3 <- new_pos %>% separate(col = wt, into = c("A", "B"), sep = "            ")
  player_info$ht <- sep3$ht
  player_info$ht <- gsub(" ","",player_info$ht)
  player_info$wt <- sep3$A
  player_info$wt <- gsub(" |\n","",player_info$wt)
  player_info <- player_info %>% mutate(wt = as.integer(wt))
  player_info$pos <- new_pos$pos
  
  new_rank <- data.frame(rank = player_info$rank)
  sep <- new_rank %>% separate(col = rank, into = c("A", "B"), sep = "                \n                ")
  new_rank$rank <- sep$B
  sep <- new_rank %>% separate(col = rank, into = c("A", "B"), sep = "            ")
  player_info$rank <- sep$A
  player_info <- player_info %>% mutate(star = if_else(rank>0.9832, "5-Star",
                                                       if_else(rank>0.8900, "4-Star",
                                                               "3-Star")))
  
  cb_list <- left_join(teams, targets, by="number")
  loginfo(glue("Parsed/Cleaned {nrow(cb_list)} records from 247Sports link {url}"))
  
  cb_list$pred_date <- with_tz(mdy_hm(cb_list$pred_date, tz = "America/New_York"), tzone = "UTC")
  
  cb_list <- cb_list %>% mutate(elapsed = as.double(difftime(pred_date,
                                                             last_updated,
                                                             units = "secs")))
  
  cb_list <- left_join(cb_list, player_info, by="number")
  seqA <- seq(1,(length(predictor_names)-1), by = 2)
  seqB <- seq(2,length(predictor_names), by=2)
  predictor_info <- data.frame(predictor = predictor_names[seqA],
                               title = predictor_names[seqB],
                               flink = flinks$link, number = 1:nrow(flinks))
  predictor_info$confidence <- as.integer(confidence)
  cb_list <- left_join(cb_list, predictor_info, by="number")
  return(cb_list)
}

new_cbs <- data.frame()
links <- c(national_url, team_url)
for (item in links) {
  tmp = query_crystal_balls(item)
  new_cbs <- rbind(new_cbs, tmp)
}
loginfo(glue("Found {nrow(new_cbs)} total Crystal Balls, deduping and filtering based on criteria: school ({selected_school}), year ({target_year}), and time since last updated ({last_updated})..."))

new_cbs <- new_cbs %>%
  group_by(predictor, name, names) %>%
  slice(1) %>%
  ungroup() %>%
  filter(elapsed >= 0 & (grepl(selected_school, names) == TRUE))

loginfo(glue("Found {nrow(new_cbs)} Crystal Balls that match given criteria."))
