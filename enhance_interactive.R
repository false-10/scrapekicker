
library(tidyverse)
library(xlsx)
library(worldfootballR)
library(fuzzyjoin)
library(xlsx)


### fehlerhafte SDS korrigieren

players_full <- readRDS("data/2425/players_full.RDS")

players_full <- players_full %>% mutate(sds = case_when(spt == 5 & player == "Kimmich" ~ FALSE,
                                                        spt == 16 & player == "Xavi" ~ TRUE,
                                                        spt == 26 & player == "Kaua Santos" ~ TRUE,
                                                        spt == 27 & player == "Aleix Garcia" ~ TRUE,
                                                        spt == 29 & player == "Xavi" ~ TRUE,
                                                        .default = sds))

### Datensatz mit Interactive-Punkten ###

interactive <- read.csv("data/2425/34_interactive.csv", sep = ";")

names(interactive) <- c("kID", "Vorname", "Nachname", "Name_kurz", "Name_lang", "Verein", 
                        "Position", "MW", "Punkte", "Note")

interactive <- interactive %>% mutate(MW = MW/10e5)

interactive <- interactive %>% filter(MW != 999)

### Interactive-Infos ergänzen ###

players_names <- players_full %>% select(player, team) %>% unique()

players_interactive <- players_names %>% full_join(interactive, by = join_by(player == Name_kurz), 
                                                   relationship = "many-to-many") %>% arrange(player)

# write.xlsx(players_interactive, "data/2425/players_interactive.xlsx")

players_interactive_edit <- read.xlsx("data/2425/players_interactive_edit.xlsx", sheetIndex = 1)

players_interactive_edit <- players_interactive_edit %>% 
  select(player, team, Position, kID, Vorname, Nachname, Name_lang, MW, Punkte, Note)


players <- players_full %>% left_join(players_interactive_edit, by = c("player", "team"))

players <- players %>% mutate(G_coef = case_when(Position == "GOALKEEPER" ~ 6,
                                     Position == "DEFENDER" ~ 5,
                                     Position == "MIDFIELDER" ~ 4,
                                     Position == "FORWARD" ~ 3,
                                     .default = 3))


players <- players %>% 
  mutate(status_pts = ifelse(status == "start", 4, ifelse(status == "sub", 2, 0)),
         grade_pts = (3.5-grade)*4,
         npG_pts = npG*G_coef, pG_pts = pG*G_coef, npA_pts = npA*2, pA_pts = pA*2,
         ylwred_pts = ylwred*(-3), red_pts = red*(-6),
         sds_pts = sds*3,
         clean_sheet_pts = case_when(Position == "GOALKEEPER" & tGA == 0 & end == 90 ~ 2, 
                                     .default = 0),
         points = status_pts + ifelse(is.na(grade_pts), 0, grade_pts) + 
           npG_pts + pG_pts + npA_pts + pA_pts + ylwred_pts + red_pts + sds_pts + clean_sheet_pts)

players$Position <- fct_recode(players$Position, Sturm = "FORWARD", Mittelfeld = "MIDFIELDER", 
                               Abwehr = "DEFENDER", Tor = "GOALKEEPER")

players$Position <- factor(players$Position, levels = c("Sturm", "Mittelfeld", "Abwehr", "Tor"))

players <- players %>% mutate(name_long = ifelse(is.na(Name_lang), player, Name_lang))

### Überprüfen auf Fehler ###

points_check <- players %>% group_by(player, team) %>% reframe(sum(points), mean(Punkte))

points_check <- points_check %>% drop_na()

identical(points_check$`sum(points)`, points_check$`mean(Punkte)`)

points_check[points_check$`sum(points)` != points_check$`mean(Punkte)`,] %>% view()

### Datensatz speichern ###

write.xlsx(players, "data/2425/players.xlsx")
write.csv(players, "data/2425/players.csv")
saveRDS(players, "data/2425/players.RDS")

#### Spielerwerte für die komplette Saison ####

players <- readRDS("data/2425/players.RDS")

players_ssn <- players %>% group_by(player, team, Position, MW, name_long) %>% 
    summarise(starts = sum(status == "start"), starts_graded = sum(status == "start" & !is.na(grade)),
              subs = sum(status == "sub"), subs_graded = sum(status == "sub" & !is.na(grade)),
              benchs = sum(status == "bench"),
              mins = sum(end - begin, na.rm = TRUE), 
              mins_mean = round(mean(end - begin, na.rm = TRUE)), 
              grade_mean = round(mean(grade, na.rm = TRUE), 2),
              tG = sum(tG), tGA = sum(tGA), 
              npG = sum(npG), pG = sum(pG), npA = sum(npA), pA = sum(pA),
              ownG = sum(ownG), ylw = sum(ylw), ylwred = sum(ylwred), red = sum(red),
              sds = sum(sds),
              status_pts = sum(status_pts), grade_pts = sum(grade_pts, na.rm = TRUE),
              npG_pts = sum(npG_pts), pG_pts = sum(pG_pts), 
              npA_pts = sum(npA_pts), pA_pts = sum(pA_pts),
              ylwred_pts = sum(ylwred_pts), red_pts = sum(red_pts),
              sds_pts = sum(sds_pts), clean_sheet_pts = sum(clean_sheet_pts), 
              points = sum(points)) %>% 
  ungroup()


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


###### Werte von kicker & fbref zusammenführen ######

players_ssn <- players_ssn %>% 
  full_join(players_jointable_edit, by = c(name_long = "Name_lang", team = "team"), keep = FALSE)

players_ssn <- players_ssn %>% 
  full_join(fbref_players, by = c(Player_latin = "Player_latin", team = "Squad"), keep = FALSE)

players_ssn <- players_ssn %>% 
  mutate(npG_perf = npG.x - npxG, npG_perf_rel = round(npG.x/npxG, 2),
         p_miss = PKatt - pG,
         A_perf = A - xAG, A_perf_rel = round(A/xAG, 2),
         npG_A = npG.x + A, npxG_xAG, npG_A_perf = npG_A - npxG_xAG, 
         npG_A_perf_rel = round(npG_A/npxG_xAG, 2),
         npG_perf_p90 = npG_p90 - npxG_p90, npG_perf_rel_p90 = round(npG_p90/npxG_p90, 2),
         A_perf_p90 = A_p90 - xAG_p90, A_perf_rel_p90 = round(A_p90/xAG_p90, 2),
         npG_A_perf_p90 = npG_A_p90 - npxG_xAG_p90, 
         npG_A_perf_rel_p90 = round(npG_A_p90/npxG_xAG_p90, 2)) %>% 
  select(player, team, type = Position, MW,
         nation = Nation, position = Pos, age = Age, born = Born,
         starts, starts_graded, subs, subs_graded, benchs, mins, mins_mean, nineties = Nineties,
         grade_mean, tG, tGA,
         npG = npG.x, npxG, npG_perf, npG_perf_rel, p_att = PKatt, pG, p_miss,
         npA, pA, xAG, A_perf, A_perf_rel,
         npG_A, npG_A_perf, npG_A_perf_rel,
         npG_p90, npxG_p90, npG_perf_p90, npG_perf_rel_p90,
         A_p90, xAG_p90, A_perf_p90, A_perf_rel_p90,
         npG_A_p90, npxG_xAG_p90, npG_A_perf_p90, npG_A_perf_rel_p90,
         ownG,
         ylw, ylwred, red, sds,
         status_pts, grade_pts, npG_pts, pG_pts, npA_pts, pA_pts, ylwred_pts, red_pts, sds_pts,
         clean_sheet_pts, points,
         name_fbref = Player, url_fbref = Url)

players_ssn <- players_ssn %>% 
  mutate_all(~ifelse(is.nan(.) | grepl("NaN", .) | grepl("NA", .), NA, .))

players_ssn$type <- fct_recode(factor(players_ssn$type), 
                               "Sturm" = "1", "Mittelfeld" = "2", "Abwehr" = "3", "Tor" = "4")

players_ssn <- players_ssn %>% 
  mutate(age = as.numeric(age)) %>% 
  as.data.frame()

saveRDS(players_ssn, "data/2425/players_ssn.RDS")
write.xlsx(players_ssn, "data/2425/players_ssn.xlsx",
           row.names = FALSE, showNA = FALSE)
write.csv(players_ssn, "data/2425/players_ssn.csv")


###### Saisonwerte verfeinern ######


players_start_ssn <- players %>% filter(status == "start" & !is.na(grade)) %>% 
  group_by(player, team, Position, MW) %>% 
  summarise(starts = sum(status == "start"), starts_graded = sum(status == "start" & !is.na(grade)),
            subs = sum(status == "sub"), subs_graded = sum(status == "sub" & !is.na(grade)),
            benchs = sum(status == "bench"),
            mins = sum(end - begin, na.rm = TRUE), 
            mins_mean = round(mean(end - begin, na.rm = TRUE)), 
            grade_mean = round(mean(grade, na.rm = TRUE), 2),
            tG = sum(tG), tGA = sum(tGA), 
            npG = sum(npG), pG = sum(pG), npA = sum(npA), pA = sum(pA),
            ownG = sum(ownG), ylw = sum(ylw), ylwred = sum(ylwred), red = sum(red),
            sds = sum(sds),
            status_pts = sum(status_pts), grade_pts = sum(grade_pts),
            npG_pts = sum(npG_pts), pG_pts = sum(pG_pts), 
            npA_pts = sum(npA_pts), pA_pts = sum(pA_pts),
            ylwred_pts = sum(ylwred_pts), red_pts = sum(red_pts),
            sds_pts = sum(sds_pts), clean_sheet_pts = sum(clean_sheet_pts), points = sum(points))

players_sub_ssn <- players %>% filter(status == "sub") %>% 
  group_by(player, team, Position, MW) %>% 
  summarise(starts = sum(status == "start"), starts_graded = sum(status == "start" & !is.na(grade)),
            subs = sum(status == "sub"), subs_graded = sum(status == "sub" & !is.na(grade)),
            benchs = sum(status == "bench"),
            mins = sum(end - begin, na.rm = TRUE), 
            mins_mean = round(mean(end - begin, na.rm = TRUE)), 
            grade_mean = round(mean(grade, na.rm = TRUE), 2),
            tG = sum(tG), tGA = sum(tGA), 
            npG = sum(npG), pG = sum(pG), npA = sum(npA), pA = sum(pA),
            ownG = sum(ownG), ylw = sum(ylw), ylwred = sum(ylwred), red = sum(red),
            sds = sum(sds),
            status_pts = sum(status_pts), grade_pts = sum(grade_pts),
            npG_pts = sum(npG_pts), pG_pts = sum(pG_pts), 
            npA_pts = sum(npA_pts), pA_pts = sum(pA_pts),
            ylwred_pts = sum(ylwred_pts), red_pts = sum(red_pts),
            sds_pts = sum(sds_pts), clean_sheet_pts = sum(clean_sheet_pts), points = sum(points))



####################################################################################################
########################### Teamwerte ##############################################################
####################################################################################################


###### Teamdaten aus fbref importieren ######

fbref_teams <- 
  fb_big5_advanced_season_stats(season_end_year = 2025, stat_type = "standard", 
                                team_or_player = "team") %>%
  filter(Comp == "Bundesliga")

fbref_teams <- fbref_teams %>% 
  select(team = Squad, type = Team_or_Opponent, num_players = Num_Players, age = Age, poss = Poss, 
         G = Gls, A = Ast, npG = G_minus_PK, pG = PK, PKatt, CrdY, CrdR, 
         xG = xG_Expected, npxG = npxG_Expected, xAG = xAG_Expected, G_p90 = Gls_Per, 
         A_p90 = Ast_Per, npG_p90 = G_minus_PK_Per, xG_p90 = xG_Per, xAG_p90 = xAG_Per,
         npxG_p90 = npxG_Per, url_fbref = Url)

fbref_teams <- fbref_teams %>% 
  pivot_wider(names_from = type, values_from = -c(team, type, num_players))

fbref_teams <- fbref_teams %>%
  mutate(npG_diff = npG_team - npG_opponent, npxG_diff = npxG_team - npxG_opponent,
         npG_diff_p90 = npG_p90_team - npG_p90_opponent, 
         npxG_diff_p90 = npxG_p90_team - npxG_p90_opponent)

fbref_teams <- fbref_teams %>% 
  mutate(team = fct_recode(team, "FC Augsburg" = "Augsburg", "Bayern München" = "Bayern Munich",
                           "VfL Bochum" = "Bochum", "Borussia Dortmund" = "Dortmund",
                           "Bor. Mönchengladbach" = "Gladbach", "1. FC Heidenheim" = "Heidenheim",
                           "TSG Hoffenheim" = "Hoffenheim", "Bayer 04 Leverkusen" = "Leverkusen",
                           "1. FSV Mainz 05" = "Mainz 05", "FC St. Pauli" = "St. Pauli",
                           "VfB Stuttgart" = "Stuttgart", "1. FC Union Berlin" = "Union Berlin",
                           "VfL Wolfsburg" = "Wolfsburg"))

fbref_teams <- fbref_teams %>%
  arrange(desc(npxG_diff), desc(npG_diff)) %>% as.data.frame()


saveRDS(fbref_teams, "data/2425/fbref_teams.RDS")

