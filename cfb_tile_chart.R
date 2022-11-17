library(cfbfastR)
library(tidyverse)
library(ggtext)
library(magick)
library(cowplot)
library(ggrepel)
library(ggimage)
library(gridExtra)
library(grid)
library(RCurl)
library(png)
library(jpeg)
library(ggridges)
library(ggtext)
library(RColorBrewer)

Sys.setenv(CFBD_API_KEY = "mtU85YjFNgHylNwj/RowQ+Tffwg65fhiZ7S7yhjb5z2bGb+5RqlnljDONQQ0VV3I")


adv<-cfbd_stats_season_advanced(
  year = 2022,
  excl_garbage_time = TRUE
)

df<- cfbd_team_info()

adv<-adv %>%
  dplyr::select(-season,-conference) %>%
  pivot_longer(!team,
               names_to = "metrics",
               values_to = "values") %>%
  dplyr::left_join(df,by=c("team" = "school"))



adv1<-cfbd_stats_season_advanced(
  year = 2022,
  excl_garbage_time = TRUE
) %>%
  dplyr::select(team,off_total_ppa,def_total_ppa) %>%
  dplyr::mutate(net_total_ppa = off_total_ppa - def_total_ppa)


adv2<-cfbd_stats_season_advanced(
  year = 2022,
  excl_garbage_time = TRUE
)

o<-adv2 %>%
  dplyr::filter(team == "Oregon")


theme_set(theme_minimal())
theme_update(
  text = element_text(family = "mono",size=14),
  plot.title = element_text('Courier', face = 'bold', size = 12, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('mono', face = 'bold', size = 10, color = 'gray50'),
  #axis.text = element_text(size = 8),
  #axis.ticks.x = element_blank(),
  #axis.text.x = element_markdown(margin = margin(t = -25,unit = "pt")),
  # axis.title = element_text(size = 24, face = 'bold'),
  #axis.title.y = element_text(size = 14, face = 'bold', hjust = 0.5),
  #axis.title.x = element_blank(),
  plot.background = element_rect(fill = '#FFFEF2', color = NA),
  #panel.background = element_rect(fill = '#FFFEF2', color = NA),
  plot.tag = element_text('mono', size = 5, color = 'gray20', hjust = 0), 
  plot.caption = element_text('mono',size = 7,color = "gray50"),
  plot.tag.position = c(.01, 0.02),
  legend.position = "none"
)

link_to_img <- function(x, width = 50) {
  #glue::glue("<img src='{x}' width='{width}'/>")
  glue::glue("<img src='{x}' width='20'/>")
}

adv<-adv %>%
  mutate(label = link_to_img(logo)) %>%
  dplyr::left_join(adv1,by=c("team" = "team"))


a<-adv %>%
  group_by(metrics) %>%
  dplyr::summarise(n = n())



adv %>%
  dplyr::filter(conference %in% c("ACC","Big 12","Big Ten",
                                  "Pac-12","SEC"),
                metrics %in% c("def_passing_plays_ppa",
                               "def_rushing_plays_ppa",
                               "off_passing_plays_ppa",
                               "off_rushing_plays_ppa")) %>%
  mutate(values = round(values,3),
         metrics = case_when(
           metrics == "def_passing_plays_ppa" ~ "def pass",
           metrics == "def_rushing_plays_ppa" ~ "def rush",
           metrics == "off_passing_plays_ppa" ~ "off pass",
           metrics == "off_rushing_plays_ppa" ~ "off rush",
           
           TRUE ~ as.character(NA) 
         ),
         values = ifelse(metrics %in% c("def rush","def pass"),
                        values * -1,values)
        ) %>%
  ggplot() +
  aes(x = metrics, y = fct_reorder(label,net_total_ppa,.desc=FALSE),fill = values) +
  geom_tile(
            color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = values),color = "black",size = 2) +
  #scale_fill_gradientn(colors = c("#FFFFFF","#bbeeff","#99ccff","#5588ff","#3366ff")) + 
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000") +
  scale_x_discrete(limits=rev) + 
  #scale_fill_manual(values=rev(brewer.pal(7, "YlGnBu")), na.value="grey90") +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_markdown(margin = margin(t = -10, unit = "pt")),
    axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5,angle = 90),
  ) +
  facet_wrap(conference ~ .,scales = "free_y",ncol = 5)  +
  labs(
    x = "",
    y = "",
    title = "Power 5 Predicted Points Added (PPA) Per Play Comparison",
    subtitle = "@QuinnsWisdom | PPA: pos (+): gaining points/play, neg (-): losing points/play",
    caption = "Sorted by Top Net PPA (Off. PPA - Def. PPA)",
    tag = "Data: cfbfastR | Excluding Garbage Time | Off/Def PPA on same scale"
  )
 

###### G5 ######

  

adv %>%
  dplyr::filter(conference %in% c("American Athletic","Conference USA",
                                  "Mid-American","Mountain West","Sun Belt"),
                metrics %in% c("def_passing_plays_ppa",
                               "def_rushing_plays_ppa",
                               "off_passing_plays_ppa",
                               "off_rushing_plays_ppa")) %>%
  mutate(values = round(values,3),
         metrics = case_when(
           metrics == "def_passing_plays_ppa" ~ "def pass",
           metrics == "def_rushing_plays_ppa" ~ "def rush",
           metrics == "off_passing_plays_ppa" ~ "off pass",
           metrics == "off_rushing_plays_ppa" ~ "off rush",
           
           TRUE ~ as.character(NA) 
         ),
         values = ifelse(metrics %in% c("def rush","def pass"),
                         values * -1,values)
  ) %>%
  ggplot() +
  aes(x = metrics, y = fct_reorder(label,net_total_ppa,.desc=FALSE),fill = values) +
  geom_tile(
    color = "white",
    lwd = 1.5,
    linetype = 1) +
  geom_text(aes(label = values),color = "black",size = 2) +
  #scale_fill_gradientn(colors = c("#FFFFFF","#bbeeff","#99ccff","#5588ff","#3366ff")) + 
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000") +
  scale_x_discrete(limits=rev) + 
  #scale_fill_manual(values=rev(brewer.pal(7, "YlGnBu")), na.value="grey90") +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_markdown(margin = margin(t = -10, unit = "pt")),
    axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5,angle = 90),
  ) +
  facet_wrap(conference ~ .,scales = "free_y",ncol = 5)  +
  labs(
    x = "",
    y = "",
    title = "Group of 5 Predicted Points Added (PPA) Per Play Comparison",
    subtitle = "@QuinnsWisdom | PPA: pos (+): gaining points/play, neg (-): losing points/play",
    caption = "Sorted by Top Net PPA (Off. PPA - Def. PPA)",
    tag = "Data: cfbfastR | Excluding Garbage Time | Off/Def PPA on same scale"
  )






