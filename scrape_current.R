
library(tidyverse)
library(rvest)

#### aktuellen Spieltag scrapen & speichern und vollen Datensatz ergänzen ####


url_start = "https://www.kicker.de/Bundesliga/spieltag/2024-25/"

page_start = read_html(url_start)

game_links <- 
  page_start %>% html_nodes("[class^='kick__v100-gameList__gameRow__stateCell__indicator kick__v100-gameList__gameRow__stateCell__indicator--schema']") %>% 
  html_attr("href")

game_links <- paste0("https://www.kicker.de", game_links) %>% 
  gsub(pattern = "analyse", replacement = "schema")

players_list <- list()
l <- 1

for (url in game_links) {
  
  page <- read_html(url)
  
  #### Grundlegende Infos ####
  
  spt_full <- page %>% html_node("div.kick__v100-scoreboardInfo") %>% html_text(trim = TRUE) %>% 
    str_split_1(pattern = " ") %>% gsub("[,.]", "", .)
  
  spt <- spt_full[3] %>% as.numeric()
  
  team_home <- 
    page %>% html_element("#kick__fixtureheader > 
                        div.kick__modul__item > 
                        div.kick__v100-gameCell.kick__v100-gameCell--big > 
                        a:nth-child(1) > 
                        div.kick__v100-gameCell__team__name") %>% html_text()
  team_home <- team_home %>% substr(1, nchar(team_home)-1)
  
  team_away <- 
    page %>% html_element("#kick__fixtureheader > 
                        div.kick__modul__item > 
                        div.kick__v100-gameCell.kick__v100-gameCell--big > 
                        a:nth-child(3) > 
                        div.kick__v100-gameCell__team__name") %>% html_text()
  team_away <- team_away %>% substr(1, nchar(team_away)-1)
  
  team_goals_home <- 
    page %>% html_elements("#kick__fixtureheader > 
                         div.kick__modul__item > 
                         div.kick__v100-gameCell.kick__v100-gameCell--big > div > 
                         div:nth-child(1) > div:nth-child(1)") %>% html_text()
  
  team_goals_away <- 
    page %>% html_elements("#kick__fixtureheader > 
                         div.kick__modul__item > 
                         div.kick__v100-gameCell.kick__v100-gameCell--big > div > 
                         div:nth-child(1) > div:nth-child(3)") %>% html_text()
  
  
  sections <- page %>% html_nodes("[class^='kick__section-item']")
  
  #### Startelf ####
  
  section_aufstellung <- sections[grepl("Aufstellung", html_text(sections))]
  
  split_player_grade <- function(string){
    player <- unlist(strsplit(string, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", perl = TRUE))[1]
    grade <- paste0(unlist(strsplit(string, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", perl = TRUE))[-1], collapse = "") %>% 
      gsub(",", ".", .)
    grade <- ifelse(nchar(grade) == 0, NA, grade) %>% as.numeric()
    return(c(player, grade))
  }
  
  lineup <- 
    page %>% 
    html_nodes("div.kick__lineup-text__unorderedList a") %>% 
    html_text2()
  
  coach_home <- lineup[23]
  coach_away <- lineup[24]
  
  lineup_home <- data.frame(player = "", grade = "")
  
  for (i in 1:11) {
    lineup_home[i,] <- c(split_player_grade(lineup[i])[1],
                         split_player_grade(lineup[i])[2])
  }
  
  lineup_away <- data.frame(player = "", grade = "")
  
  for (i in 1:11) {
    lineup_away[i,] <- c(split_player_grade(lineup[i+11])[1],
                         split_player_grade(lineup[i+11])[2])
  }
  
  
  #### Wechsel ####
  
  section_wechsel <- sections[grepl("Wechsel", html_text(sections))]
  
  subs_html <- 
    section_wechsel %>%
    html_nodes("[class^='kick__data-grid__main']")
  
  subs_players_home <- 
    subs_html[1] %>% 
    html_nodes("[class^='kick__substitutions__player']") %>% 
    html_text2()
  
  subs_players_away <- 
    subs_html[2] %>% 
    html_nodes("[class^='kick__substitutions__player']") %>% 
    html_text2()
  
  subs_times_home <- 
    subs_html[1] %>% 
    html_nodes("[class^='kick__substitutions__time']") %>% 
    html_text2() %>% gsub("[']", "", .)
  
  subs_times_away <- 
    subs_html[2] %>% 
    html_nodes("[class^='kick__substitutions__time']") %>% 
    html_text2() %>% gsub("[']", "", .)
  
  
  subs_home <- data.frame(player = "", time = "", time_extra = "", grade = "", out = "")
  t <- 1
  
  if(length(subs_players_home) > 0) {
    for (i in 1:(length(subs_players_home)/2)) {
      sub_player <- split_player_grade(subs_players_home[2*i-1])[1]
      sub_grade <- split_player_grade(subs_players_home[2*i-1])[2]
      sub_out <- split_player_grade(subs_players_home[2*i])[1]
      sub_time <- str_split_1(subs_times_home[t], " ")[1]
      sub_time_extra <- str_split_1(subs_times_home[t], " ")[2] %>% gsub("[+]", "", .)
      ifelse (subs_times_home[t+1] == "", t <- t+2, t <- t+3)
      subs_home[i,] <- c(sub_player, sub_time, sub_time_extra, sub_grade, sub_out)
    }
  }
  
  subs_away <- data.frame(player = "", time = "", time_extra = "", grade = "", out = "")
  t <- 1
  
  if(length(subs_players_away) > 0) {
    for (i in 1:(length(subs_players_away)/2)) {
      sub_player <- split_player_grade(subs_players_away[2*i-1])[1]
      sub_grade <- split_player_grade(subs_players_away[2*i-1])[2]
      sub_out <- split_player_grade(subs_players_away[2*i])[1]
      sub_time <- str_split_1(subs_times_away[t], " ")[1]
      sub_time_extra <- str_split_1(subs_times_away[t], " ")[2] %>% gsub("[+]", "", .)
      ifelse (subs_times_away[t+1] == "", t <- t+2, t <- t+3)
      subs_away[i,] <- c(sub_player, sub_time, sub_time_extra, sub_grade, sub_out)
    }
  }
  
  subs <- rbind(subs_home, subs_away)
  
  #### Bank ####
  
  section_bank <- sections[grepl("Reservebank", html_text(sections))]
  
  bench_html <- section_bank %>% html_nodes("[class^='kick__substitutions__team']")
  
  bench_home <- bench_html[1] %>% html_nodes("div.kick__lineup-text a") %>% html_text2()
  
  bench_away <- bench_html[2] %>% html_nodes("div.kick__lineup-text a") %>% html_text2()
  
  
  
  
  #### zusammenführung ####
  
  players_start_home <- lineup_home %>% mutate(status = "start", .before = player) %>% 
    mutate(begin = 0, .before = grade)
  players_start_away <- lineup_away %>% mutate(status = "start", .before = player) %>% 
    mutate(begin = 0, .before = grade)
  
  
  players_subs_home <- data.frame(status = "sub", player = subs_home$player, begin = subs_home$time, 
                                  grade = subs_home$grade)
  players_subs_away <- data.frame(status = "sub", player = subs_away$player, begin = subs_away$time, 
                                  grade = subs_away$grade)
  
  players_bench_home <- data.frame(status = "bench", player = bench_home, begin = NA, grade = NA)
  players_bench_away <- data.frame(status = "bench", player = bench_away, begin = NA, grade = NA)
  
  players_home <- rbind(players_start_home, players_subs_home, players_bench_home) %>% 
    mutate(team = team_home, .before = status) %>% mutate(location = "home", opponent = team_away)
  players_away <- rbind(players_start_away, players_subs_away, players_bench_away) %>% 
    mutate(team = team_away, .before = status) %>% mutate(location = "away", opponent = team_home)
  
  players_home <- players_home %>% mutate(end = 0, .after = begin)
  
  for (p in players_home$player){
    if (players_home$status[players_home$player == p] == "bench") {
      players_home$end[players_home$player == p] <- NA
    } else if (p %in% subs_home$out) {
      players_home$end[players_home$player == p] <- subs_home$time[subs_home$out == p]
    } else {
      players_home$end[players_home$player == p] <- 90
    }
  }
  
  players_away <- players_away %>% mutate(end = 0, .after = begin)
  
  for (p in players_away$player){
    if (players_away$status[players_away$player == p] == "bench") {
      players_away$end[players_away$player == p] <- NA
    } else if (p %in% subs_away$out) {
      players_away$end[players_away$player == p] <- subs_away$time[subs_away$out == p]
    } else {
      players_away$end[players_away$player == p] <- 90
    }
  }
  
  players_home$tG <- team_goals_home
  players_home$tGA <- team_goals_away
  
  players_away$tG <- team_goals_away
  players_away$tGA <- team_goals_home
  
  players <- rbind(players_home, players_away)
  
  
  #### Tore ####
  
  goals_html <- page %>% html_nodes("[class^='kick__goals__row']")
  
  goals_df <- data.frame(time = "", player = "", subtxt = "", body_part = "", assist = "", team = "")
  
  players <- players %>% mutate(npG = 0, pG = 0, npA = 0, pA = 0, ownG = 0)
  
  if (!(sum(grepl("Fehlanzeige", goals_html)) > 0)) {
    
    for (i in 1:length(goals_html)) {
      time <- goals_html[i] %>% html_elements("[class^='kick__goals__time']") %>% 
        html_text2() %>% gsub("[\r|'| ]", "", .)
      # bestimmen ob Tor für Heim oder Auswärtsteam
      marker_home <- goals_html[i] %>% html_elements("[class^='kick__goals__team kick__goals__team--left']") %>% 
        html_text2() %>% gsub("[\r|'| ]", "", .) %>% nchar()
      time = ifelse(marker_home == 0, time[2], time[1])
      team = ifelse(marker_home == 0, team_away, team_home)
      player = goals_html[i] %>% html_elements("[class^='kick__substitutions--hide-mobile']") %>% 
        html_text2()
      subtxt = goals_html[i] %>% html_elements("[class^='kick__goals__player-subtxt']") %>% 
        html_text2() %>% gsub("[\r| ]", "", .)
      info <- goals_html[i] %>% html_elements("[class^='kick__assist__player']") %>% 
        html_text2() %>% str_split_1(", ")
      body_part <- info[1]
      assist <- info[2]
      goals_df <- goals_df %>% rbind(c(time, player, subtxt, body_part, assist, team))
    }
    
    goals_df <- goals_df[-1,]
    
    for (p in players$player){
      # falls der Spieler in goals_df auftaucht
      if (any(apply(goals_df, 1, function(row) any(grepl(p, row))))) {
        players[players$player == p,]$npG <- goals_df %>% 
          filter(player == p & subtxt != "(Elfmeter)" & subtxt != "(Eigentor)") %>% nrow()
        players[players$player == p,]$pG <- goals_df %>% 
          filter(player == p & subtxt == "(Elfmeter)") %>% nrow()
        players[players$player == p,]$npA <- goals_df %>% 
          filter(assist == p & subtxt != "(Elfmeter)") %>% nrow()
        players[players$player == p,]$pA <- goals_df %>% 
          filter(assist == p & subtxt == "(Elfmeter)") %>% nrow()
        players[players$player == p,]$ownG <- goals_df %>% 
          filter(player == p & subtxt == "(Eigentor)") %>% nrow()
      }
    }
    
  }
  
  #### Karten ####
  
  section_karten <- sections[grepl("Karten", html_text(sections))] %>% 
    html_nodes("[class^='kick__data-grid__main']")
  
  cards_players_home <- 
    section_karten[1] %>% 
    html_nodes("[class^='kick__substitutions__player']") %>% 
    html_text2()
  
  cards_players_away <- 
    section_karten[2] %>% 
    html_nodes("[class^='kick__substitutions__player']") %>% 
    html_text2()
  
  cards_times_home <- 
    section_karten[1] %>% 
    html_nodes("[class^='kick__substitutions__time']") %>% 
    html_text2() %>% gsub("[']", "", .)
  
  cards_times_away <- 
    section_karten[2] %>% 
    html_nodes("[class^='kick__substitutions__time']") %>% 
    html_text2() %>% gsub("[']", "", .)
  
  cards_html_home <- 
    section_karten[1] %>%
    html_nodes("[class^='kick__substitutions__icon-box']")
  
  cards_html_away <- 
    section_karten[2] %>%
    html_nodes("[class^='kick__substitutions__icon-box']")
  
  
  
  cards_home <- data.frame(player = "", time = "", time_extra = "", type = "")
  t <- 1
  
  if (length(cards_players_home) > 0) {
    
    for (i in 1:(length(cards_players_home)/2)) {
      card_player <- cards_players_home[2*i-1]
      card_time <- str_split_1(cards_times_home[t], " ")[1]
      card_time_extra <- str_split_1(cards_times_home[t], " ")[2] %>% gsub("[+]", "", .)
      ifelse (is.na(str_split_1(cards_times_home[t], " ")[2]), t <- t+1, t <- t+2)
      if (grepl(cards_html_home[i], pattern = "kick__ticker-icon-array")) {
        card_type <- "yellow_red"
      } else if (grepl(cards_html_home[i], pattern = "kick__icon-Gelb")) {
        card_type <- "yellow"
      } else if (grepl(cards_html_home[i], pattern = "kick__icon-Rot")) {
        card_type <- "red"
      }
      cards_home[i,] <- c(card_player, card_time, card_time_extra, card_type)
    }
    
  }
  
  cards_away <- data.frame(player = "", time = "", time_extra = "", type = "")
  t <- 1
  
  if (length(cards_players_away) > 0) {
    
    for (i in 1:(length(cards_players_away)/2)) {
      card_player <- cards_players_away[2*i-1]
      card_time <- str_split_1(cards_times_away[t], " ")[1]
      card_time_extra <- str_split_1(cards_times_away[t], " ")[2] %>% gsub("[+]", "", .)
      ifelse (is.na(str_split_1(cards_times_away[t], " ")[2]), t <- t+1, t <- t+2)
      if (grepl(cards_html_away[i], pattern = "kick__ticker-icon-array")) {
        card_type <- "yellow_red"
      } else if (grepl(cards_html_away[i], pattern = "kick__icon-Gelb")) {
        card_type <- "yellow"
      } else if (grepl(cards_html_away[i], pattern = "kick__icon-Rot")) {
        card_type <- "red"
      }
      cards_away[i,] <- c(card_player, card_time, card_time_extra, card_type)
    }
    
  }
  
  cards <- rbind(cards_home, cards_away)
  
  
  players <- players %>% mutate(ylw = 0, ylwred = 0, red = 0)
  
  for (p in players$player){
    # falls der Spieler in goals_df auftaucht
    if (any(apply(cards, 1, function(row) any(grepl(p, row))))) {
      players[players$player == p,]$ylw <- cards %>% 
        filter(player == p & type == "yellow") %>% nrow()
      players[players$player == p,]$ylwred <- cards %>% 
        filter(player == p & type == "yellow_red") %>% nrow()
      players[players$player == p,]$red <- cards %>% 
        filter(player == p & type == "red") %>% nrow()
    }
  }
  
  
  #### finalisieren mit Spieltag und Spieler des Spiels ####
  
  sds <- gsub("schema", "spielinfo", url) %>% read_html() %>% 
    html_element("[class^='kick__gameinfo__person']") %>% 
    html_text() %>% gsub("[\r\n]", "", .) %>% str_split_1(pattern = ", ")
  
  players <- players %>% mutate(spt = spt, .before = team) %>% 
    rowwise() %>% 
    mutate(sds = ifelse(grepl(pattern = sds[1], x = player), TRUE, FALSE))
  
  players_list[[l]] <- players
  
  l <- l+1
  
}

players_final <- do.call(rbind, players_list)

players_final$begin <- as.numeric(players_final$begin)
players_final$end <- as.numeric(players_final$end)
players_final$grade <- as.numeric(sprintf("%.1f", as.numeric(players_final$grade)))

players_final <- as.data.frame(players_final)

saveRDS(players_final, 
        file = paste0("data/2324/", sprintf("%02d", spt), "_players_", format(Sys.Date(), "%Y-%m-%d"), ".RDS"))

write.csv2(players_final, 
           file = paste0("data/2324/", sprintf("%02d", spt), "_players_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
           fileEncoding = "UTF-8")

if (spt == 1){
  saveRDS(players_final, 
          file = paste0("data/2324/players_full.RDS"))
} else {
  
  players_previous <- readRDS("data/2324/players_full.RDS")
  
  players_final <- rbind(players_previous, players_final)
  
  saveRDS(players_final, 
          file = paste0("data/2324/players_full.RDS"))
  
  write.csv2(players_final, 
             file = paste0("data/2324/players_full.csv"),
             fileEncoding = "UTF-8")
}
