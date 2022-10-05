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

Sys.setenv(CFBD_API_KEY = "mtU85YjFNgHylNwj/RowQ+Tffwg65fhiZ7S7yhjb5z2bGb+5RqlnljDONQQ0VV3I")

pbp<-data.frame()


for(i in 2022) {
  re<-cfbfastR::load_cfb_pbp(seasons = i) 
  
  pbp<-bind_rows(pbp,re)
  
}

rm(re)


pwr<-pbp %>%
  dplyr::filter(offense_conference %in% c("SEC","Pac-12","ACC","Big 12","Big Ten"))




top_qb<-pwr %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  group_by(passer_player_name,pos_team) %>%
  summarize(attempts = n()) %>%
  arrange(desc(attempts)) %>%
  ungroup() %>%
  group_by(pos_team) %>%
  top_n(n = 1)

qb_epa<-pwr %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  group_by(passer_player_name,pos_team) %>%
  summarize(total_EPA = mean(EPA,na.rm = T),
            var_EPA = var(EPA,na.rm = T)) 

top_qb<-top_qb %>%
  dplyr::inner_join(qb_epa,by = "passer_player_name") %>%
  dplyr::arrange(desc(total_EPA))


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
  plot.tag = element_text('mono', size = 10, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  legend.position = "none"
)

df<- cfbd_team_info()

link_to_img <- function(x, width = 50) {
  #glue::glue("<img src='{x}' width='{width}'/>")
  glue::glue("<img src='{x}' width='20'/>")
}





  
  plot_df<-pwr %>%
  dplyr::filter(complete.cases(passer_player_name)) %>%
  left_join(top_qb,by="passer_player_name") %>%
  left_join(df,pwr, by = c("pos_team.x" = "school")) %>%
  #left_join(qb_epa,pwr,by=c("pos_team.x" = "pos_team")) %>%
  dplyr::filter(attempts > 0) %>%
  mutate(label = link_to_img(logo)
  )
  
  
  plot_df %>%
  ggplot() + 
  aes(x = EPA, y = fct_reorder(label,total_EPA,.desc=FALSE), fill = color,
      color = alt_color, alpha = 0.5) + 
    geom_density_ridges(
      quantile_lines=TRUE,
      quantile_fun=function(x,...)mean(x),
      scale = 1,size=0.25,
      ) +
    scale_fill_identity() +
    scale_color_identity() +
    facet_wrap(offense_conference ~ .,scales = "free_y",ncol = 5) +
    scale_x_continuous(limits = c(-5,5)) +
    #scale_y_discrete(labels = labels) +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_markdown(margin = margin(t = -20, unit = "pt")),
      axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5),
    ) +
    labs(
      y = NULL,
      title = "Power 5 Passing EPA Distribution (Thru Week 5)",
      caption = "Viz: @QuinnsWisdom | Data: cfbfastR",
      subtitle = "Ordered by Starting QB w/ Top Mean Passing EPA per Conference"
    )
  

    



