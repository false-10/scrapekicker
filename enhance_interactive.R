
library(tidyverse)
library(xlsx)


### Datensatz mit Interactive-Punkten ###

interactive <- read.csv("data/2425/15_interactive.csv", sep = ";")

names(interactive) <- c("kID", "Vorname", "Nachname", "Name_kurz", "Name_lang", "Verein", 
                        "Position", "MW", "Punkte", "Note")

interactive <- interactive %>% mutate(MW = MW/10e5)

### Interactive-Infos ergänzen ###

players_full <- readRDS("data/2425/players_full.RDS")

players_names <- players_full %>% select(player, team) %>% unique()

players_interactive <- players_names %>% full_join(interactive, by = join_by(player == Name_kurz), 
                                                   relationship = "many-to-many") %>% arrange(player)

write.xlsx(players_interactive, "data/2425/players_interactive.xlsx")

players_interactive_edit <- read.xlsx("data/2425/players_interactive_edit.xlsx", sheetIndex = 1)

players_interactive_edit <- players_interactive_edit %>% 
  select(player, team, Position, kID, Position, MW, Punkte, Note)


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

season <- players %>% group_by(player, team) %>% reframe(sum(points), mean(Punkte))

### Überprüfen auf Fehler ###

# season <- season %>% drop_na()
# 
# identical(season$`sum(points)`, season$`mean(Punkte)`)
# 
# season[season$`sum(points)` != season$`mean(Punkte)`,]

### Fehler bei Kimmich bereinigen ###

players <- players %>% mutate(sds_pts = case_when(player == "Kimmich" & spt == 5 ~ 0,
                                                  .default = sds_pts),
                              points = case_when(player == "Kimmich" & spt == 5 ~ 10,
                                                  .default = points))

### Datensatz speicher ###

write.xlsx(players, "data/2425/players.xlsx")
write.csv(players, "data/2425/players.csv")
saveRDS(players, "data/2425/players.RDS")
