
library(tidyverse)
library(worldfootballR)


#### Spieler ####


fbref_players <- load_fb_big5_advanced_season_stats(season_end_year = 2025, stat_type = "standard", 
                                                    team_or_player = "player") %>%
  filter(Comp == "Bundesliga")
  
fbref_23 <- fbref_23 %>% rename(Matches = MP_Playing, 
                                Starts = Starts_Playing, Minutes = Min_Playing, Nineties = Mins_Per_90_Playing,
                                npG = G_minus_PK, PrgC = PrgC_Progression, PrgP = PrgP_Progression, 
                                PrgR = PrgR_Progression, npg_Per = G_minus_PK_Per, xG = xG_Expected,
                                npxG = npxG_Expected, xAG = xAG_Expected)

fbref_23 <- fbref_23 %>% mutate(Player_latin = stri_trans_general(Player, id = "Latin-ASCII"))





all_season_team <- load_fb_big5_advanced_season_stats(stat_type = "defense", team_or_player = "team")
current_season_team <- load_fb_big5_advanced_season_stats(season_end_year = 2022, stat_type = "defense", team_or_player = "team")

