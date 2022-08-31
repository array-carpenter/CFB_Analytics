library(cfbfastR)
library(tidyverse)
library(ggtext)
library(magick)
library(cowplot)


####################3 RETURNING PRODUCTION ##############

#### Question: How unknown is this Oregon offense ahead of their 2022 opener vs Georgia

Sys.setenv(CFBD_API_KEY = "mtU85YjFNgHylNwj/RowQ+Tffwg65fhiZ7S7yhjb5z2bGb+5RqlnljDONQQ0VV3I")

pbp<-data.frame()

for(i in 2014:2022) {
  re<-cfbfastR::load_cfb_pbp(seasons = i) %>%
    dplyr::filter(passer_player_name %in% c("Bo Nix","Anthony Brown","Marcus Mariota","Justin Herbert",
                                            "Vernon Adams","Dakota Prukop","Tyler Shough","Vernon Adams Jr.",
                                            "Braxton Burmeister"))
  
  pbp<-bind_rows(pbp,re)
  
  
}

theme_set(theme_minimal())
theme_update(
  text = element_text(family = "mono",size=14),
  plot.title = element_text('Courier', face = 'bold', size = 12, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('mono', face = 'bold', size = 10, color = 'gray50'),
  axis.text = element_text(size = 12),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.5),
  #plot.background = element_rect(fill = '#FFFEF2', color = NA),
  #panel.background = element_rect(fill = '#FFFEF2', color = NA),
  plot.tag = element_text('mono', size = 10, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  legend.position = "none"
)



pbp %>%
  ggplot() + 
  aes(y = total_away_EPA_pass,x= total_home_EPA_pass) +
  geom_point(color = '#154733') +
  facet_wrap(~passer_player_name) +
  labs(x = "Home EPA",
       y = "Away EPA")

logo <- image_read("https://seeklogo.com/images/O/oregon-ducks-logo-541BCE5FDA-seeklogo.com.png") %>% 
  image_resize(300)

mypngfile <- download.file('https://seeklogo.com/images/O/oregon-ducks-logo-541BCE5FDA-seeklogo.com.png', destfile = 'ducks.png', mode = 'wb')
library(png)
mypng <- readPNG('ducks.png')



plot<-pbp %>%
  dplyr::filter(pos_team == "Oregon" |
                  pos_team == "Auburn"
  ) %>%
  dplyr::filter(passer_player_name != "Braxton Burmeister") %>%
  dplyr::group_by(passer_player_name) %>%
  dplyr::summarize(EPA = mean(EPA,na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(EPA)) %>%
  ggplot() +
  aes(x = fct_reorder(passer_player_name,EPA),y = EPA,position = "dodge",
      fill = passer_player_name) +
  geom_col() +
  #annotation_raster(mypng, ymin = 0.1,ymax= 0.2,xmin = 0.2,xmax = 0.3) +
  coord_flip() +
  geom_hline(yintercept = 0.001,color = "black",size = 2) +
  scale_fill_manual(values = 
                      c('#154733','#F26522','#154733','#154733',
                        '#154733','#154733','#154733','#154733')) +
  labs(
    x = "",
    y = "mean EPA",
    title = "Bo Nix comparison to previous Duck QBs",
    subtitle = "Career EPA (Mariota: since 2014 season | Transfer QBs only including UO stats)",
    caption = "Data: @cfbfastR",
    tag = "@QuinnsWisdom"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "white", size = 0.5),
    panel.ontop = TRUE#,
    #plot.background = element_rect(fill = '#FFFEF2'),
  ) 
  

ggdraw() +
  draw_image("https://seeklogo.com/images/O/oregon-ducks-logo-541BCE5FDA-seeklogo.com.png",
             x = 0.38, y = -0.3, scale = .1) +
  draw_plot(plot)  +
  theme(
    plot.background = element_rect(fill = '#FFFEF2')
  )

ggsave("Bo_nix_ducks.png")

  


pbp %>%
  dplyr::filter(pos_team == "Oregon" |
                  pos_team == "Auburn"
  ) %>%
  dplyr::filter(passer_player_name != 'Ty Thompson') %>%
  dplyr::group_by(passer_player_name,year) %>%
  dplyr::summarize(EPA = mean(EPA,na.rm = T),
                   n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(EPA)) 





