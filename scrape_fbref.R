
library(tidyverse)
library(worldfootballR)


#### Spieler ####


fbref_players <- 
  load_fb_big5_advanced_season_stats(season_end_year = 2025, stat_type = "standard", 
                                                    team_or_player = "player") %>%
  filter(Comp == "Bundesliga")
  
fbref_players <- fbref_players %>% 
  rename(Matches = MP_Playing, 
         Starts = Starts_Playing, Minutes = Min_Playing, Nineties = Mins_Per_90_Playing,
         npG = G_minus_PK, PrgC = PrgC_Progression, PrgP = PrgP_Progression, 
         PrgR = PrgR_Progression, npg_Per = G_minus_PK_Per, xG = xG_Expected,
         npxG = npxG_Expected, xAG = xAG_Expected)

fbref_players <- fbref_players %>% 
  mutate(Player_latin = stri_trans_general(Player, id = "Latin-ASCII"))


names(fbref_24)[c(8:11, 14,23,27:36)] <-
  c("MP", "Starts", "Mins", "Nineties", "G_A", "npxG_xAG", "Gls_p90", "Ast_p90", "G_A_p90",
    "npG_p90", "npG_A_p90", "xG_p90", "xAG_p90",
    "xG_xAG_p90", "npxG_p90", "npxG_xAG_p90")

fbref_24 <-
  fbref_24 %>%
  select(Player, Pos, Team = Squad, Age, MP, Starts, Mins, Nineties, G = Gls, A = Ast, G_A, npG = G_minus_PK,
         Pens = PK, Pens_att = PKatt, CrdY, CrdR, xG = xG_Expected, npxG = npxG_Expected, xAG = xAG_Expected,
         npxG_xAG, G_p90 = Gls_p90, A_p90 = Ast_p90, npG_p90, npG_A_p90, xG_p90, xAG_p90, npxG_p90, npxG_xAG_p90)



