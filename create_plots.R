
library(tidyverse)
library(ggdark)
library(extrafont)
library(ggrepel)

"#ffa600"
"#bc5090"

theme_set(dark_theme_classic(base_family = "Barlow Condensed"))
theme_update(panel.border = element_rect(color = "white", fill = NA),
             plot.background = element_rect(fill = "#102a43"),
             panel.background = element_rect(fill = "#102a43"),
             legend.background = element_rect(fill = "#102a43"))

players_ssn <- readRDS("data/2425/players_ssn.RDS")



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





