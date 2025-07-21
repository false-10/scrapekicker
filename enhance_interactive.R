
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
              grade_mean = sprintf("%.2f", mean(grade, na.rm = TRUE)),
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

saveRDS(players_ssn, "data/2425/players_ssn.RDS")
write.xlsx(players_ssn, "data/2425/players_ssn.xlsx")
write.csv(players_ssn, "data/2425/players_ssn.csv")


players_start_ssn <- players %>% filter(status == "start" & !is.na(grade)) %>% 
  group_by(player, team, Position, MW) %>% 
  summarise(starts = sum(status == "start"), starts_graded = sum(status == "start" & !is.na(grade)),
            subs = sum(status == "sub"), subs_graded = sum(status == "sub" & !is.na(grade)),
            benchs = sum(status == "bench"),
            mins = sum(end - begin, na.rm = TRUE), 
            mins_mean = round(mean(end - begin, na.rm = TRUE)), 
            grade_mean = sprintf("%.2f", mean(grade, na.rm = TRUE)),
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
            grade_mean = sprintf("%.2f", mean(grade, na.rm = TRUE)),
            tG = sum(tG), tGA = sum(tGA), 
            npG = sum(npG), pG = sum(pG), npA = sum(npA), pA = sum(pA),
            ownG = sum(ownG), ylw = sum(ylw), ylwred = sum(ylwred), red = sum(red),
            sds = sum(sds),
            status_pts = sum(status_pts), grade_pts = sum(grade_pts),
            npG_pts = sum(npG_pts), pG_pts = sum(pG_pts), 
            npA_pts = sum(npA_pts), pA_pts = sum(pA_pts),
            ylwred_pts = sum(ylwred_pts), red_pts = sum(red_pts),
            sds_pts = sum(sds_pts), clean_sheet_pts = sum(clean_sheet_pts), points = sum(points))
