
library(tidyverse)
library(worldfootballR)
library(fuzzyjoin)
library(xlsx)


#### Spielerdaten von fbref importieren ####


fbref_players <- 
  load_fb_big5_advanced_season_stats(season_end_year = 2025, stat_type = "standard", 
                                                    team_or_player = "player") %>%
  filter(Comp == "Bundesliga")
  
fbref_players <- fbref_players %>% 
  rename(Matches = MP_Playing, Starts = Starts_Playing, Minutes = Min_Playing, 
         Nineties = Mins_Per_90_Playing, 
         G = Gls, A = Ast, npG = G_minus_PK, 
         PrgC = PrgC_Progression, PrgP = PrgP_Progression, PrgR = PrgR_Progression, 
         G_p90 = Gls_Per, A_p90 = Ast_Per, npG_p90 = G_minus_PK_Per, 
         xG = xG_Expected, npxG = npxG_Expected, xAG = xAG_Expected, 
         xG_p90 = xG_Per, xAG_p90 = xAG_Per, npxG_p90 = npxG_Per) %>% 
  rename_with(~c("G_A", "G_A_p90", "npG_A_p90", "npxG_xAG", "xG_xAG_p90", "npxG_xAG_p90"), 
              c("G+A", "G+A_Per", "G+A_minus_PK_Per", "npxG+xAG_Expected", "xG+xAG_Per",
                "npxG+xAG_Per")) %>% 
  mutate(Player_latin = stringi::stri_trans_general(Player, id = "Latin-ASCII"),
         Squad = fct_recode(Squad, "FC Augsburg" = "Augsburg", "Bayern München" = "Bayern Munich",
                            "VfL Bochum" = "Bochum", "Borussia Dortmund" = "Dortmund",
                            "Eintracht Frankfurt" = "Eint Frankfurt", "SC Freiburg" = "Freiburg",
                            "Bor. Mönchengladbach" = "Gladbach", "1. FC Heidenheim" = "Heidenheim",
                            "TSG Hoffenheim" = "Hoffenheim", "Bayer 04 Leverkusen" = "Leverkusen",
                            "1. FSV Mainz 05" = "Mainz 05", "FC St. Pauli" = "St. Pauli",
                            "VfB Stuttgart" = "Stuttgart", "1. FC Union Berlin" = "Union Berlin",
                            "VfL Wolfsburg" = "Wolfsburg")) %>% 
  select(-c(Season_End_Year, Comp))


saveRDS(fbref_players, "data/2425/fbref_players.RDS")


###### Namen zusammenführen ###########


names_kicker <- players %>% group_by(Name_lang, team) %>% summarise()
names_kicker$playerID <- 1:nrow(names_kicker)

names_fbref <- fbref_players %>% select(Player_latin, Squad)

players_jointable <- stringdist_full_join(names_kicker, names_fbref, 
                                    by = c(Name_lang = "Player_latin", team = "Squad"))

write.xlsx(as.data.frame(players_jointable), "data/2425/players_jointable.xlsx")


players_jointable_edit <- read.xlsx("data/2425/players_jointable_edit.xlsx", sheetIndex = 1)

players_jointable_edit <- players_jointable_edit %>% 
  select(-NA.) %>% 
  group_by(playerID) %>% 
  fill(everything(), .direction = "downup") %>% 
  distinct() %>%
  ungroup()


players_ssn <- readRDS("data/2425/players_ssn.RDS")

players_ssn <- players_ssn %>% 
  full_join(players_jointable_edit, by = c(name_long = "Name_lang", team = "team"), keep = FALSE)

players_ssn <- players_ssn %>% 
  full_join(fbref_players, by = c(Player_latin = "Player_latin", team = "Squad"), keep = FALSE)

players_ssn <- players_ssn %>% 
  mutate(npG_perf = npG.x - npxG,
           p_miss = PKatt - pG,
           A_perf = A - xAG,
           npG_A = npG.x + A, npxG_xAG, npG_A_perf = npG_A - npxG_xAG,
           npG_perf_p90 = npG_p90 - npxG_p90, A_perf_p90 = A_p90 - xAG_p90,
           npG_A_perf_p90 = npG_A_p90 - npxG_xAG_p90) %>% 
  select(player, team, type = Position, MW,
           nation = Nation, position = Pos, age = Age, born = Born,
           starts, starts_graded, subs, subs_graded, benchs, mins, mins_mean, nineties = Nineties,
           grade_mean, tG, tGA,
           npG = npG.x, npxG, npG_perf, p_att = PKatt, pG, p_miss,
           npA, pA, xAG, A_perf, 
           npG_A, npG_A_perf,
           npG_p90, npxG_p90, npG_perf_p90, 
           A_p90, xAG_p90, A_perf_p90,
           npG_A_p90, npxG_xAG_p90, npG_A_perf_p90,
           ownG,
           ylw, ylwred, red, sds,
           status_pts, grade_pts, npG_pts, pG_pts, npA_pts, pA_pts, ylwred_pts, red_pts, sds_pts,
           clean_sheet_pts, points,
           name_fbref = Player, url_fbref = Url)

saveRDS(players_ssn, "data/2425/players_ssn_fbref.RDS")
write.xlsx(players_ssn, "data/2425/players_ssn_fbref.xlsx")
write.csv(players_ssn, "data/2425/players_ssn_fbref.csv")
