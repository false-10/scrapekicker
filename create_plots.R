
library(tidyverse)
library(ggdark)
library(extrafont)
library(ggrepel)
library(ggpattern)

"#ffa600"
"#bc5090"

theme_set(dark_theme_classic(base_family = "Barlow Condensed"))
theme_update(panel.border = element_rect(color = "white", fill = NA),
             plot.background = element_rect(fill = "#102a43"),
             panel.background = element_rect(fill = "#102a43"),
             legend.background = element_rect(fill = "#102a43"))


players <- readRDS("data/2425/players.RDS")
players_ssn <- readRDS("data/2425/players_ssn.RDS")

teams_ssn <- readRDS("data/2425/teams_ssn.RDS")


####################################################################################################
########################### Spieler ###############################################################
####################################################################################################


###### Spieler nach Gesamtpunkten Barplots #######


players_ssn %>% filter(points >= 200 && Position != "Tor") %>%
ggplot(aes(x = points, y = fct_reorder(player, points), fill = Position)) +
  geom_vline(xintercept = seq(0, 300, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +10, fontface = "bold") +
  labs(x = "", y = "", title = "Spieler mit mind. 200 Punkten",
       subtitle = "ohne Torhüter | Saison 24/25") +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 370), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


players_ssn %>% filter(points >= 150 && points < 200 && Position != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = Position)) +
  geom_vline(xintercept = seq(0, 230, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +10, fontface = "bold") +
  labs(x = "", y = "", title = "Spieler zwischen 150 & 200 Punkten",
       subtitle = "ohne Torhüter | Saison 24/25") +
  scale_x_continuous(breaks = seq(0, 230, 50), limits = c(0, 230), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


players_ssn %>% filter(points >= 100 && points < 150 && Position != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = Position)) +
  geom_vline(xintercept = seq(0, 230, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +10, fontface = "bold") +
  labs(x = "", y = "", title = "Spieler mit mind. 200 Punkten",
       subtitle = "ohne Torhüter | Saison 24/25") +
  scale_x_continuous(breaks = seq(0, 230, 50), limits = c(0, 170), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


###### Spieler nach Gesamtpunkten & Marktwert #######

players_ssn %>% filter(Position != "Tor") %>%
  ggplot(aes(x = points, y = MW, col = Position)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0.001*(2^(3:8)), color="red", 
              linetype="dashed") +
  scale_x_continuous(expand = c(0,0)) +
  ggrepel::geom_text_repel(aes(label = player))


players_ssn %>% filter(Position != "Tor") %>%
  ggplot(aes(x = points, y = MW, col = Position)) +
  geom_point() +
  gghighlight::gghighlight() + 
  facet_wrap(vars(Position))


players_ssn %>% filter(Position != "Tor") %>%
  mutate(PpM = points/MW) %>% 
  ggplot(aes(x = points, y = PpM, col = Position)) +
  geom_point() +
  geom_vline(xintercept = mean(points), linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = player))

players_ssn %>% filter(Position != "Tor") %>%
  mutate(PpM = points/MW) %>% 
  ggplot(aes(x = MW, y = PpM, col = Position)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = player))
  



####### das Ganze nochmal nur mit benoteten Startelfeinsätzen ######


players_start_ssn %>% filter(points >= 200 && Position != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = Position)) +
  geom_vline(xintercept = seq(0, 300, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +10, fontface = "bold") +
  labs(x = "", y = "", title = "Spieler mit mind. 200 Punkten",
       subtitle = "ohne Torhüter | Saison 24/25") +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 370), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


players_start_ssn %>% filter(points >= 150 && points < 200 && Position != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = Position)) +
  geom_vline(xintercept = seq(0, 230, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +10, fontface = "bold") +
  labs(x = "", y = "", title = "Spieler zwischen 150 & 200 Punkten",
       subtitle = "ohne Torhüter | Saison 24/25") +
  scale_x_continuous(breaks = seq(0, 230, 50), limits = c(0, 230), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


players_start_ssn %>% filter(points >= 100 && points < 150 && Position != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = Position)) +
  geom_vline(xintercept = seq(0, 230, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +10, fontface = "bold") +
  labs(x = "", y = "", title = "Spieler mit mind. 200 Punkten",
       subtitle = "ohne Torhüter | Saison 24/25") +
  scale_x_continuous(breaks = seq(0, 230, 50), limits = c(0, 170), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


###### Spieler nach Gesamtpunkten & Marktwert #######

players_start_ssn %>% filter(Position != "Tor") %>%
  ggplot(aes(x = points, y = MW, col = Position)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0.001*(2^(3:8)), color="red", 
              linetype="dashed") +
  scale_x_continuous(expand = c(0,0)) +
  ggrepel::geom_text_repel(aes(label = player))


players_start_ssn %>% filter(Position != "Tor") %>%
  ggplot(aes(x = points, y = MW, col = Position)) +
  geom_point() +
  gghighlight::gghighlight() + 
  facet_wrap(vars(Position))


players_start_ssn %>% filter(Position != "Tor") %>%
  mutate(PpM = points/MW) %>% 
  ggplot(aes(x = points, y = PpM, col = Position)) +
  geom_point() +
  geom_vline(xintercept = mean(points), linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = player))

players_start_ssn %>% filter(Position != "Tor") %>%
  mutate(PpM = points/MW) %>% 
  ggplot(aes(x = MW, y = PpM, col = Position)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = player))



######### Nur Einwechslungen über die Saison #######


players_sub_ssn %>% filter(Position != "Tor") %>% 
  ggplot(aes(x = subs, y = points, col = Position)) +
  geom_abline(slope = 2, linetype = "dotted", col = "grey") +
  geom_hline(yintercept = 0, linetype = "dotted", col = "grey") +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = player)) +
  labs(x = "", y = "")



################ xG-Performance von Spielern ####################


players_ssn %>% filter(type == "Sturm" & mins > 899) %>%
  ggplot(aes(y = fct_reorder(player, npxG_p90))) +
  geom_vline(xintercept = seq(0, 1.2, 0.1), linetype = "dotted", col = "grey") +
  geom_col(aes(x = npxG_p90, fill = "xG")) +
  geom_col_pattern(aes(x = npG_p90, col = "Tore"), 
                   fill = NA,
                   pattern = "stripe", pattern_fill = "#bc5090", 
                   pattern_spacing = 0.04, pattern_size = 0.2, pattern_alpha = 0.3) +
  geom_text(aes(label = player, x = npxG_p90), hjust = 1.05, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(x = npxG_p90, label = sprintf("%.1f", nineties)), 
            hjust = 1.05, nudge_x = 0.045, fontface = "bold") +
  labs(x = "", y = "", title = "Torgefahr Stürmer", 
       subtitle = "Bundesliga 24/25 | pro 90 Min | ohne Elfmeter | mind. 900 Min") +
  scale_x_continuous(breaks = seq(0, 2, 0.1), limits = c(0, 1.2), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  scale_fill_manual(values = c("xG" = "#ffa600")) +
  scale_color_manual(values = c("Tore" = "#bc5090")) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

players_ssn %>% filter(type == "Mittelfeld" & mins > 899 & npxG_p90 >= 0.2) %>%
  ggplot(aes(y = fct_reorder(player, npxG_p90))) +
  geom_vline(xintercept = seq(0, 1.2, 0.1), linetype = "dotted", col = "grey") +
  geom_col(aes(x = npxG_p90, fill = "xG")) +
  geom_col_pattern(aes(x = npG_p90, col = "Tore"), 
                   fill = NA,
                   pattern = "stripe", pattern_fill = "#bc5090", 
                   pattern_spacing = 0.04, pattern_size = 0.2, pattern_alpha = 0.3) +
  geom_text(aes(label = player, x = npxG_p90), hjust = 1.05, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(x = npxG_p90, label = sprintf("%.1f", nineties)), 
            hjust = 1.05, nudge_x = 0.045, fontface = "bold") +
  labs(x = "", y = "", title = "Torgefahr Mittelfeldspieler", 
       subtitle = "Bundesliga 24/25 | pro 90 Min | ohne Elfmeter | mind. 900 Min") +
  scale_x_continuous(breaks = seq(0, 2, 0.1), limits = c(0, 1.2), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  scale_fill_manual(values = c("xG" = "#ffa600")) +
  scale_color_manual(values = c("Tore" = "#bc5090")) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

players_ssn %>% filter(type == "Abwehr" & mins > 899 & (npxG_p90 >= 0.1 | npG_p90 >= 0.1)) %>%
  ggplot(aes(y = fct_reorder(player, npxG_p90))) +
  geom_vline(xintercept = seq(0, 0.3, 0.1), linetype = "dotted", col = "grey") +
  geom_col(aes(x = npxG_p90, fill = "xG")) +
  geom_col_pattern(aes(x = npG_p90, col = "Tore"), 
                   fill = NA,
                   pattern = "stripe", pattern_fill = "#bc5090", 
                   pattern_spacing = 0.04, pattern_size = 0.2, pattern_alpha = 0.3) +
  geom_text(aes(label = player, x = npxG_p90), hjust = 1.05, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(x = npxG_p90, label = sprintf("%.1f", nineties)), 
            hjust = 1.05, nudge_x = 0.045, fontface = "bold") +
  labs(x = "", y = "", title = "Torgefahr Abwehrspieler", 
       subtitle = "Bundesliga 24/25 | pro 90 Min | ohne Elfmeter | mind. 900 Min") +
  scale_x_continuous(breaks = seq(0, 2, 0.1), limits = c(0, 0.3), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  scale_fill_manual(values = c("xG" = "#ffa600")) +
  scale_color_manual(values = c("Tore" = "#bc5090")) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


players_ssn %>% filter(mins < 900 & mins > 399 & npxG_p90 >= 0.3) %>%
  ggplot(aes(y = fct_reorder(player, npxG_p90))) +
  geom_vline(xintercept = seq(0, 1.2, 0.1), linetype = "dotted", col = "grey") +
  geom_col(aes(x = npxG_p90, fill = "xG")) +
  geom_col_pattern(aes(x = npG_p90, col = "Tore"), 
                   fill = NA,
                   pattern = "stripe", pattern_fill = "#bc5090", 
                   pattern_spacing = 0.04, pattern_size = 0.2, pattern_alpha = 0.3) +
  geom_text(aes(label = player, x = npxG_p90), hjust = 1.05, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(x = npxG_p90, label = sprintf("%.1f", nineties)), 
            hjust = 1.05, nudge_x = 0.045, fontface = "bold") +
  labs(x = "", y = "", title = "Torgefahr Spieler mit wenig Einsatzzeit", 
       subtitle = "Bundesliga 24/25 | pro 90 Min | ohne Elfmeter | zwischen 400 & 900 Min") +
  scale_x_continuous(breaks = seq(0, 2, 0.1), limits = c(0, 1.2), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  scale_fill_manual(values = c("xG" = "#ffa600")) +
  scale_color_manual(values = c("Tore" = "#bc5090")) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))



####################################################################################################
########################### Teams ##################################################################
####################################################################################################


###### Tordiffferenz ######

teams_ssn %>% 
ggplot(aes(y = fct_reorder(team, npxG_diff_p90))) +
  geom_vline(xintercept = seq(-1.2, 1.7, 0.3), linetype = "dotted", col = "grey") +
  geom_col(aes(x = npxG_diff_p90, fill = "xG")) +
  geom_col_pattern(aes(x = npG_diff_p90, col = "Tore"), 
                   fill = NA,
                   pattern = "stripe", pattern_fill = "#bc5090", 
                   pattern_spacing = 0.04, pattern_size = 0.2, pattern_alpha = 0.3) +
  geom_text(aes(label = team, x = npxG_diff_p90, hjust = ifelse(npxG_diff_p90 >= 0, 1.05, -0.05)), 
            fontface = "bold") +
  labs(x = "", y = "", title = "Tordifferenz pro Spiel", 
       subtitle = "Bundesliga 24/25 | ohne Elfmeter") +
  scale_x_continuous(breaks = seq(-1.2, 1.7, 0.3), limits = c(-1.2, 1.7), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  scale_fill_manual(values = c("xG" = "#ffa600")) +
  scale_color_manual(values = c("Tore" = "#bc5090")) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))










