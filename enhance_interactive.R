
library(tidyverse)
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


### Überprüfen auf Fehler ###

points_check <- players %>% group_by(player, team) %>% reframe(sum(points), mean(Punkte))

points_check <- points_check %>% drop_na()

identical(points_check$`sum(points)`, points_check$`mean(Punkte)`)

points_check[points_check$`sum(points)` != points_check$`mean(Punkte)`,] %>% view()

### Datensatz speichern ###

write.xlsx(players, "data/2425/players.xlsx")
write.csv(players, "data/2425/players.csv")
saveRDS(players, "data/2425/players.RDS")


#### Spieler nach Saison ####

players %>% group_by(player, team) %>% 
  summarise(starts = sum(status == "start"), starts_graded = sum(status == "start" & !is.na(grade)),
            subs = sum(status == "sub"), subs_graded = sum(status == "sub" & !is.na(grade)),
            benchs = sum(status == "bench"),
            mins = sum(end - begin, na.rm = TRUE), mins_mean = mean(end - begin, na.rm = TRUE), 
            mins_mean_start = mean((end - begin)[status == "start"], na.rm = TRUE))
