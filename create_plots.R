
library(tidyverse)
library(ggdark)
library(extrafont)
library(ggrepel)
library(ggpattern)
library(gtExtras)
library(plotly)

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

players_start <- readRDS("data/2425/players_start.RDS")
players_start_ssn <- readRDS("data/2425/players_start_ssn.RDS")



####################################################################################################
########################### Teams ##################################################################
####################################################################################################

###### Gesamtpunkte nach Position #######

players %>% filter(!is.na(type) & type != "Tor") %>% 
  group_by(team, type) %>% 
  summarise(points = sum(points)) %>% 
  group_by(team) %>% mutate(points_all = sum(points)) %>%
  ggplot(aes(x = points, y = fct_reorder(team, points_all), fill = type)) +
  geom_vline(xintercept = seq(0,3000,500), col = "grey", alpha = 0.3) +
  geom_col(position = "stack") +
  labs(x = "", y = "", title = "Gesamtpunkte der Vereine",
       subtitle = "Saison 24/25 | ohne Torhüter") +
  scale_x_continuous(breaks = seq(0, 3000, 500), limits = c(0, 3000), expand = c(0,0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Teams_Punkte_Position.png", dpi = 400)


###### Gesamtpunkte nach Punkten pro 90 Min ######


players %>% filter(!is.na(type) & type != "Tor") %>% 
  group_by(team, type) %>% 
  summarise(nineties = sum(mins, na.rm = TRUE)/90,
            points = sum(points)) %>% 
  group_by(team) %>% mutate(points_all = sum(points)) %>%
  ggplot(aes(x = points/nineties, y = fct_reorder(team, points_all), fill = type)) +
  geom_vline(xintercept = seq(0, 30, 5), col = "grey", alpha = 0.3) +
  geom_col(position = "stack") +
  labs(x = "Punkte / 90 Minuten", y = "", 
       title = "Wie haben die Mannschaftsteile nach gespielten Minuten gepunktet?",
       subtitle = "Saison 24/25 | ohne Torhüter") +
  scale_x_continuous(breaks = seq(0, 30, 5), limits = c(0,30), expand = c(0,0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 10, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        axis.title.x = element_text(size = 13),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Teams_Punkte_p90.png", dpi = 400)

players %>% filter(!is.na(type) & type != "Tor") %>% 
  group_by(team, type) %>% 
  summarise(nineties = sum(mins, na.rm = TRUE)/90,
            points = sum(points)) %>% 
  group_by(team) %>% mutate(points_all = sum(points)) %>%
  ggplot(aes(x = points/nineties, y = fct_reorder(team, points_all), fill = type)) +
  geom_col(position = "fill") +
  geom_vline(xintercept = c(1/3, 2/3), col = "grey") +
  labs(x = "", y = "", title = "Vergleich der Mannschaftsteile innerhalb der Vereine",
       subtitle = "Saison 24/25 | ohne Torhüter") +
  scale_x_continuous(breaks = seq(0, 1, 1/3), expand = c(0,0), labels = c("0", "1/3", "2/3", "1")) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Teams_Punkte_p90_fill.png", dpi = 400)


####################################################################################################
########################### Spieler ###############################################################
####################################################################################################


###### Spieler nach Gesamtpunkten Barplots #######


players_ssn %>% filter(points >= 200 & type != "Tor") %>%
ggplot(aes(x = points, y = fct_reorder(player, points), fill = type)) +
  geom_vline(xintercept = seq(0, 350, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +13, fontface = "bold") +
  labs(x = "", y = "", title = "Gesamtpunkte der Topspieler im Vergleich",
       subtitle = "Saison 24/25 | ohne Torhüter | mind. 200 Punkte") +
  scale_x_continuous(breaks = seq(0, 300, 50), limits = c(0, 370), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Spieler_Punkte_200.png", dpi = 400)


players_ssn %>% filter(points >= 150 & points < 200 & type != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = type)) +
  geom_vline(xintercept = seq(0, 230, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold", size = 3) +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +6, fontface = "bold", 
            size = 3) +
  labs(x = "", y = "", title = "Gesamtpunkte der zweiten Riege im Vergleich",
       subtitle = "Saison 24/25 | ohne Torhüter | zwischen 150 & 200 Punkten") +
  scale_x_continuous(breaks = seq(0, 230, 50), limits = c(0, 230), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Spieler_Punkte_150.png", dpi = 400)

players_ssn %>% filter(points >= 125 & points < 150 & type != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = type)) +
  geom_vline(xintercept = seq(0, 230, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold", size = 2) +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +4, fontface = "bold",
            size = 2) +
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

###### Gesamtpunkte nach Position Boxplot ######

players_ssn %>% filter(!is.na(type)) %>% 
  ggplot(aes(x = type, y = points, fill = type)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(-10, 350))

players_ssn %>% filter(!is.na(type) & points >= 10) %>% 
  ggplot(aes(x = type, y = points, fill = type)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(-10, 350))

players_ssn %>% filter(!is.na(type) & starts > 0) %>% 
  ggplot(aes(x = type, y = points, fill = type)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(-10, 350))

players_ssn %>% filter(!is.na(type) & starts > 0) %>% 
  ggplot(aes(x = type, y = points, fill = type)) +
  geom_violin(draw_quantiles = c(0.1, 0.5, 0.9)) +
  scale_y_continuous(limits = c(-10, 350))

players_ssn %>% filter(!is.na(type) & starts > 1) %>% 
  ggplot(aes(x = type, y = points, fill = type)) +
  geom_hline(yintercept = seq(0, 350, 50), linetype = "dotted", col = "grey", alpha = 0.3) +
  geom_violin(draw_quantiles = c(0.1, 0.5, 0.9), scale = "count") +
  geom_jitter(width = 0.1, col = "white") +
  scale_y_continuous(breaks = seq(0, 350, 50)) +
  scale_fill_discrete(guide = "none") +
  labs(x = "", y = "", title = "Wie verteilen sich die Spieler nach Gesamtpunkten?",
       subtitle = "Saison 24/25 | mind. 1 Startelfeinsatz") +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"))

ggsave("plots/2425/Spieler_Punkte_violin.png", dpi = 400)


###### Spieler nach Punkten pro Marktwert #####

players_ssn %>% filter(points/MW >= 80 & type != "Tor") %>%
  ggplot(aes(x = points/MW, y = fct_reorder(player, points/MW), fill = type)) +
  geom_vline(xintercept = seq(0,220,50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold", size = 3) +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +6, fontface = "bold",
            size = 3) +
  labs(x = "Punkte pro Mio", y = "", 
       title = "Welche Spieler waren am besten nach Punkten pro Mio?",
       subtitle = "Saison 24/25 | ohne Torhüter | mind. 80 Punkte pro Mio") +
  scale_x_continuous(breaks = seq(0,220,50), limits = c(0, 220), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 10, color = "white"),
        axis.title.x = element_text(size = 13),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Spieler_PpM_80.png", dpi = 400)


players_ssn %>% filter(points/MW < 80 & points/MW >= 60 & type != "Tor") %>%
  ggplot(aes(x = points/MW, y = fct_reorder(player, points/MW), fill = type)) +
  geom_vline(xintercept = seq(0,90,50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold", size = 3) +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +6, fontface = "bold",
            size = 3) +
  labs(x = "Punkte pro Mio", y = "", 
       title = "Welche Spieler waren am besten nach Punkten pro Mio?",
       subtitle = "Saison 24/25 | ohne Torhüter | zw. 60 & 80 Punkte pro Mio") +
  scale_x_continuous(breaks = seq(0,90,50), limits = c(0, 90), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 10, color = "white"),
        axis.title.x = element_text(size = 13),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


players_ssn %>% filter(!is.na(type) & starts > 0) %>% 
  ggplot(aes(x = type, y = points/MW, fill = type)) +
  geom_hline(yintercept = seq(0, 200, 50), linetype = "dotted", col = "grey", alpha = 0.3) +
  geom_violin(draw_quantiles = c(0.1, 0.5, 0.9), scale = "count") +
  geom_jitter(width = 0.05, col = "white") +
  scale_y_continuous(breaks = seq(0, 200, 50)) +
  scale_fill_discrete(guide = "none") +
  labs(x = "", y = "", title = "Wie verteilen sich die Spieler nach Punkte pro Mio?",
       subtitle = "Saison 24/25 | mind. 1 Startelfeinsatz") +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"))

ggsave("plots/2425/Spieler_PpM_violin.png", dpi = 400)

###### Spieler nach Punkten pro 90 Min ######

players_ssn %>% filter(points/nineties >= 8 & type != "Tor" & starts > 4) %>%
  ggplot(aes(x = points/nineties, y = fct_reorder(player, points/nineties), fill = type)) +
  geom_vline(xintercept = seq(0,16,2), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold", size = 3) +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +0.4, fontface = "bold",
            size = 3) +
  labs(x = "Punkte / 90 Minuten", y = "", 
       title = "Welche Spieler waren am besten nach Punkten pro Spielzeit?",
       subtitle = "Saison 24/25 | ohne Torhüter | mind. 5 Startelfeinsätze | mind. 8 Punkte p90") +
  scale_x_continuous(breaks = seq(0,16,2), limits = c(0,16), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 10, color = "white"),
        axis.title.x = element_text(size = 13),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Spieler_Pp90_8.png", dpi = 400)


players_ssn %>% filter(points/nineties >= 7 & points/nineties < 8 & type != "Tor" & starts > 4) %>%
  ggplot(aes(x = points/nineties, y = fct_reorder(player, points/nineties), fill = type)) +
  geom_vline(xintercept = seq(0,9,2), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold", size = 2) +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +0.2, fontface = "bold",
            size = 2) +
  labs(x = "Punkte / 90 Minuten", y = "", 
       title = "Welche Spieler waren am besten nach Punkten pro Spielzeit?",
       subtitle = "Saison 24/25 | ohne Torhüter | mind. 5 Startelfeinsätze | Punkte p90 zw. 7 & 8") +
  scale_x_continuous(breaks = seq(0,9,2), limits = c(0,9), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 10, color = "white"),
        axis.title.x = element_text(size = 13),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


players_ssn %>% filter(!is.na(type) & starts > 4) %>% 
  ggplot(aes(x = type, y = points/nineties, fill = type)) +
  geom_hline(yintercept = seq(0, 16, 2), linetype = "dotted", col = "grey", alpha = 0.3) +
  geom_violin(draw_quantiles = c(0.1, 0.5, 0.9), scale = "count") +
  geom_jitter(width = 0.05, col = "white") +
  scale_y_continuous(breaks = seq(0, 16, 2)) +
  scale_fill_discrete(guide = "none") +
  labs(x = "", y = "", title = "Wie verteilen sich die Spieler nach Punkte pro 90 Min?",
       subtitle = "Saison 24/25 | mind. 5 Startelfeinsätze") +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"))

ggsave("plots/2425/Spieler_Pp90_violin.png", dpi = 400)

###### Spieler nach Art der Punkte ######

players_ssn %>% filter(type != "Tor" & points >= 200) %>% 
  pivot_longer(cols = status_pts:sds_pts, names_to = "pts_type", values_to = "pts") %>%
  ggplot(aes(y = fct_reorder(player, points), x = pts, fill = pts_type)) +
  geom_col()
  
players_ssn %>% filter(type != "Tor" & points >= 150 & points < 200) %>% 
  pivot_longer(cols = status_pts:sds_pts, names_to = "pts_type", values_to = "pts") %>%
  ggplot(aes(y = fct_reorder(player, points), x = pts, fill = pts_type)) +
  geom_col()


###### Spieler nach Gesamtpunkten & Marktwert #######




players_ssn %>% filter(type != "Tor") %>%
  ggplot(aes(x = points, y = points/MW, col = type)) +
  geom_point() +
  geom_text_repel(aes(label = player)) +
  labs(x = "Punkte", y = "Punkte pro Mio", title = "Effizienz nach Punkte pro Mio",
       subtitle = "Saison 24/25 | ohne Torhüter") +
  scale_x_continuous(breaks = seq(0, 380, 50), limits = c(0, 380), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,220,50), limits= c(0,220), expand = c(0,0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 10, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.position = c(0.9, 0.8),
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Spieler_Punkte_PpM.png", dpi = 400)

players_ssn %>% filter(type != "Tor" & starts > 4) %>%
  ggplot(aes(x = points, y = points/nineties, col = type)) +
  geom_point() +
  geom_vline(xintercept = mean(points), linetype = "dashed") +
  geom_text_repel(aes(label = player)) +
  labs(x = "Punkte", y = "Punkte pro Mio", title = "Effizienz nach Punkte pro 90 Min",
       subtitle = "Saison 24/25 | ohne Torhüter | mind. 5 Startelfeinsätze") +
  scale_x_continuous(breaks = seq(0, 380, 50), limits = c(0, 380), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,16,2), limits= c(0,16), expand = c(0,0)) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 10, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.position = c(0.9, 0.2),
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Spieler_Punkte_Pp90.png", dpi = 400)

players_ssn %>% filter(type != "Tor") %>%
  ggplot(aes(x = MW, y = points/MW, col = type)) +
  geom_point() +
  geom_text_repel(aes(label = player))


players_ssn %>% filter(type != "Tor") %>%
  ggplot(aes(x = points, y = MW, col = type)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0.001*(2^(3:8)), color="grey", 
              linetype="dashed") +
  scale_x_continuous(expand = c(0,0)) +
  geom_text_repel(aes(label = player))
  




###### Spieler nach Noten & Verein ######

players_start_ssn %>% filter(!is.na(type) & type != "Tor" & starts > 9) %>% 
  ggplot(aes(x = grade_mean, y = team, col = type, 
             text = paste(player, "\nNote: ", grade_mean))) +
  geom_point() +
  labs(x = "", y = "", title = "Wie wurden Spieler der Vereine benotet?",
       subtitle = "Saison 24/25 | ohne Torhüter | mind. 10 Startelfeinsätze") +
  scale_x_continuous(breaks = seq(2.5, 4.5, 0.5), limits = c(2.3, 4.7), expand = c(0,0)) +
  scale_y_discrete(limits = rev) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Teams_Spieler_Note.png", dpi = 400)


players_start_ssn %>% filter(!is.na(type) & type != "Tor" & starts > 9) %>% 
  ggplot(aes(x = grade_mean, y = team, col = type, 
             text = paste(player, "\nNote: ", grade_mean))) +
  geom_point() +
  geom_text_repel(aes(label = player), max.overlaps = 6, vjust = 0.5, size = 2.5) +
  labs(x = "", y = "", title = "Welche Spieler sind durch ihre Noten herausgestochen?",
       subtitle = "Saison 24/25 | ohne Torhüter | mind. 10 Startelfeinsätze") +
  scale_x_continuous(breaks = seq(2.5, 4.5, 0.5), limits = c(2.3, 4.7), expand = c(0,0)) +
  scale_y_discrete(limits = rev) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))

ggsave("plots/2425/Teams_Spieler_Note_labeled.png", dpi = 400)

ggplotly(tooltip = "text")

###### das Ganze nochmal nur mit benoteten Startelfeinsätzen ######


players_start_ssn %>% filter(points >= 200 & type != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = type)) +
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


players_start_ssn %>% filter(points >= 150 && points < 200 && type != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = type)) +
  geom_vline(xintercept = seq(0, 230, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +10, fontface = "bold") +
  labs(x = "", y = "", title = "Spieler zwischen 150 & 200 Punkten",
       subtitle = "Saison 24/25 | ohne Torhüter") +
  scale_x_continuous(breaks = seq(0, 230, 50), limits = c(0, 230), expand = c(0,0)) +
  scale_y_discrete(breaks = NULL) +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        plot.subtitle = element_text(size = 15, hjust = 0.5),
        axis.text.x = element_text(size = 13, color = "white"), 
        axis.text.y = element_text(size = 10, color = "white"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 11),
        legend.spacing = unit(0, "cm"))


players_start_ssn %>% filter(points >= 100 && points < 150 && type != "Tor") %>%
  ggplot(aes(x = points, y = fct_reorder(player, points), fill = type)) +
  geom_vline(xintercept = seq(0, 160, 50), linetype = "dotted", col = "grey") +
  geom_col() +
  geom_text(aes(label = player), hjust = 1.1, nudge_y = -0.005, fontface = "bold") +
  geom_text(aes(label = sprintf("%.1f", MW)), hjust = 1.05, nudge_x = +10, fontface = "bold") +
  labs(x = "", y = "", title = "Spieler zwischen 100 & 150 Punkten",
       subtitle = "Saison 24/25 | ohne Torhüter") +
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

players_start_ssn %>% filter(type != "Tor") %>%
  ggplot(aes(x = points, y = MW, col = type)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0.001*(2^(3:8)), color="grey", 
              linetype="dashed") +
  scale_x_continuous(expand = c(0,0)) +
  ggrepel::geom_text_repel(aes(label = player))


players_start_ssn %>% filter(type != "Tor") %>%
  ggplot(aes(x = points, y = MW, col = type)) +
  geom_point() +
  gghighlight::gghighlight() + 
  facet_wrap(vars(type))


players_start_ssn %>% filter(type != "Tor") %>%
  mutate(PpM = points/MW) %>% 
  ggplot(aes(x = points, y = PpM, col = type)) +
  geom_point() +
  geom_vline(xintercept = mean(points), linetype = "dashed") +
  ggrepel::geom_text_repel(aes(label = player))

players_start_ssn %>% filter(type != "Tor") %>%
  mutate(PpM = points/MW) %>% 
  ggplot(aes(x = MW, y = PpM, col = type)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = player))



###### Nur Einwechslungen über die Saison #######


players_sub_ssn %>% filter(type != "Tor") %>% 
  ggplot(aes(x = subs, y = points, col = type)) +
  geom_abline(slope = 2, linetype = "dotted", col = "grey") +
  geom_hline(yintercept = 0, linetype = "dotted", col = "grey") +
  geom_point() +
  geom_text_repel(aes(label = player)) +
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






###### Vergleich Noten Heim vs Auswärts ######

players_start %>% 
  mutate(g_diff = tG-tGA) %>% 
  group_by(team, location) %>% 
  summarise(grade_mean = mean(grade), g_diff = sum(g_diff)) %>%
  ggplot(aes(x = team, y = grade_mean, fill = location)) +
  geom_col(position = "dodge") +
  scale_y_continuous() +
  coord_cartesian(ylim = c(2.8, 4))


players_start %>% 
  group_by(spt, team, location) %>% 
  summarise(g_diff = mean(tG) - mean(tGA), grade_mean = mean(grade)) %>% 
  group_by(team, location) %>% 
  summarise(grade_mean = mean(grade_mean), g_diff = sum(g_diff)) %>% 
  pivot_wider(names_from = location, values_from = c(grade_mean, g_diff)) %>% 
  mutate(grade_diff = grade_mean_away - grade_mean_home,
         g_diff_diff = g_diff_home - g_diff_away) %>% 
  ggplot(aes(y = grade_diff, x = g_diff_diff)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text_repel(aes(label = team))


players_start %>% 
  group_by(spt, team, location) %>% 
  summarise(tpoints = case_when(mean(tG) - mean(tGA) > 0 ~ 3,
                                mean(tG) - mean(tGA) == 0 ~1,
                                mean(tG) - mean(tGA) < 0 ~ 0), 
            grade_mean = mean(grade)) %>% 
  group_by(team, location) %>% 
  summarise(grade_mean = mean(grade_mean), tpoints = sum(tpoints)) %>% 
  pivot_wider(names_from = location, values_from = c(grade_mean, tpoints)) %>% 
  mutate(grade_diff = grade_mean_away - grade_mean_home,
         tpoints_diff = tpoints_home - tpoints_away) %>% 
  ggplot(aes(y = grade_diff, x = tpoints_diff)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text_repel(aes(label = team))

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




