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


####################3 PLAYMAKER VIZ ##############

#### Question: How unknown is this Oregon offense ahead of their 2022 opener vs Georgia

Sys.setenv(CFBD_API_KEY = Sys.getenv("CFBD_API_KEY"))



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
  legend.position = "bottom"
)



ppa<-data.frame()

for(i in 2010:2021) {
  re<-cfbd_metrics_ppa_players_games(year = i,team = "Oregon")
  
  ppa<-bind_rows(ppa,re)
  
  rm(re)
  
  
}

use<-data.frame()

for(i in 2010:2021) {
 re<-cfbd_player_usage(year = i,team="Oregon")
  
  use<-bind_rows(use,re)
  
  rm(re)
  
  
}






img_2013 <- readJPEG(getURLContent("https://www.decalsextremeonline.com/assets/images/oregonduckssilverhelmet.jpg"))
img_2014 <- readPNG(getURLContent("https://www.riddell.com/medias/sys_master/images/images/h64/h2d/8803035349022/-1200Wx1200H-000000000008036681-1.png"))
img_2015 <- readJPEG(getURLContent("https://i.ebayimg.com/images/g/ddIAAOSwKNhaX5GU/s-l500.jpg"))
img_2016 <- readJPEG(getURLContent("https://i.ebayimg.com/images/g/wZkAAOSwOX9eu-Um/s-l500.jpg"))
#img_2017 <- readJPEG(getURLContent("https://cdn.vox-cdn.com/thumbor/BKJv51VAKVuywNqkZ4A9Niaqxmw=/1400x0/filters:no_upscale()/cdn.vox-cdn.com/uploads/chorus_asset/file/9109749/Screen_Shot_2017_08_24_at_3.15.04_PM.png"))
img_2017 <- readPNG(getURLContent("https://www.helmethistory.com/uploads/8/3/1/5/8315957/published/2017-6-oregon-vs-wsu.png"))
#img_2018 <- readJPEG(getURLContent("https://i.ebayimg.com/images/g/1z8AAOSwmBthN0ip/s-l500.jpg"))
img_2018 <-readPNG(getURLContent("https://www.helmethistory.com/uploads/8/3/1/5/8315957/published/2018-2-oregon-vs-portland-state-posting-side.png"))
img_2019 <- readJPEG(getURLContent("https://i.ebayimg.com/images/g/xWEAAOSwDv5ghxsU/s-l500.jpg"))
img_2020 <- readPNG(getURLContent("https://www.helmethistory.com/uploads/8/3/1/5/8315957/published/2020-5-oregon-vs-cal.png"))
img_2021 <- readPNG(getURLContent("https://www.helmethistory.com/uploads/8/3/1/5/8315957/published/2021-7-oregon.png"))







plot<-ppa %>%
  dplyr::filter(complete.cases(avg_PPA_pass),
                complete.cases(avg_PPA_rush)) %>%
  dplyr::group_by(name,position,season) %>%
  dplyr::summarise(ppa_pass_var = var(avg_PPA_pass,na.rm = T),
                   ppa_rush_var = var(avg_PPA_rush,na.rm = T),
                   total_ppa_var = var(avg_PPA_all,na.rm = T),
                   total_ppa_mean = mean(avg_PPA_all,na.rm = T)) %>%
  dplyr::filter(complete.cases(ppa_pass_var),
                complete.cases(ppa_rush_var),
                position %in% c('RB','WR','TE')) %>%
  dplyr::left_join(use,by=c('season','name')) %>%
  dplyr::select(-position.y) %>%
  dplyr::rename(position = position.x) %>%
  dplyr::filter(usg_overall >= 0.1) %>%
  dplyr::arrange(desc(total_ppa_mean)) %>%
  dplyr::mutate(img = case_when(
    season == "2013" ~ "https://www.decalsextremeonline.com/assets/images/oregonduckssilverhelmet.jpg",
    season == "2014" ~ "https://www.riddell.com/medias/sys_master/images/images/h64/h2d/8803035349022/-1200Wx1200H-000000000008036681-1.png",
    season == "2015" ~ "https://i.ebayimg.com/images/g/ddIAAOSwKNhaX5GU/s-l500.jpg",
    season == "2016" ~ "https://i.ebayimg.com/images/g/wZkAAOSwOX9eu-Um/s-l500.jpg",
    season == "2017" ~ "https://www.helmethistory.com/uploads/8/3/1/5/8315957/published/2017-6-oregon-vs-wsu.png",
    season == "2018" ~ "https://www.helmethistory.com/uploads/8/3/1/5/8315957/published/2018-2-oregon-vs-portland-state-posting-side.png",
    season == "2019" ~ "https://i.ebayimg.com/images/g/xWEAAOSwDv5ghxsU/s-l500.jpg",
    season == "2020" ~ "https://www.helmethistory.com/uploads/8/3/1/5/8315957/published/2020-5-oregon-vs-cal.png",
    season == "2021" ~ "https://www.helmethistory.com/uploads/8/3/1/5/8315957/published/2021-7-oregon.png",
    TRUE ~ as.character(NA)
  )) %>%
  ggplot() +
  aes(x=total_ppa_mean,y=total_ppa_var,size=usg_overall) +
  geom_point(color= '#154733') +
  geom_image(aes(image=img), size=.04) +
  geom_smooth(method="lm",color="grey",linetype="dashed",se=FALSE) +
  geom_text_repel(aes(label = name), 
                  size = 2, 
                  colour = "black",
                  position = position_nudge(y=-0.02,x=0.01)) +
  labs(
    x= "Mean Total PPA",
    y = "Weekly Variance of Total PPA",
    title = "Oregon Playmakers Weekly Consistency",
    subtitle = "@QuinnsWisdom | Seasons: 2013-2021",
    tag = "Data: cfbfastR",
    caption = "Utilized in both passing/rushing game each week"
  ) +
  theme(
    legend.position = "none"
  ) + 
  ### 2013
  annotate("text", x = 0.03, y = 0.5, label = "UO 2013") +
  annotation_raster(img_2013, xmin = 0, xmax = 0.05, 
                    ymin = 0.52, 
                    ymax = 0.55) +
  ## 2014
  annotate("text", x = 0.03, y = 0.43, label = "UO 2014") +
  annotation_raster(img_2014, xmin = 0, xmax = 0.05, 
                    ymin = 0.44, 
                    ymax = 0.47) +
  ## 2015
annotate("text", x = 0.13, y = 0.5, label = "UO 2015") +
  annotation_raster(img_2015, xmin = 0.1, xmax = 0.15, 
                    ymin = 0.52, 
                    ymax = 0.55) +
  ## 2016
  annotate("text", x = 0.13, y = 0.43, label = "UO 2016") +
  annotation_raster(img_2016, xmin = 0.1, xmax = 0.15, 
                    ymin = 0.44, 
                    ymax = 0.47)   +
  ## 2017 
  annotate("text", x = 0.23, y = 0.5, label = "UO 2017") +
  annotation_raster(img_2017, xmin = 0.2, xmax = 0.25, 
                    ymin = 0.52, 
                    ymax = 0.55) +  
  ## 2018 
  annotate("text", x = 0.23, y = 0.43, label = "UO 2018") +
  annotation_raster(img_2018, xmin = 0.2, xmax = 0.25, 
                    ymin = 0.44, 
                    ymax = 0.47) +  
  ## 2019
  annotate("text", x = 0.33, y = 0.5, label = "UO 2019") +
  annotation_raster(img_2019, xmin = 0.3, xmax = 0.35, 
                    ymin = 0.52, 
                    ymax = 0.55) + 
  ## 2020
  annotate("text", x = 0.33, y = 0.43, label = "UO 2020") +
  annotation_raster(img_2020, xmin = 0.3, xmax = 0.35, 
                    ymin = 0.44, 
                    ymax = 0.47) + 
  ## 2021
  annotate("text", x = 0.43, y = 0.5, label = "UO 2021") +
  annotation_raster(img_2021, xmin = 0.4, xmax = 0.45, 
                    ymin = 0.52, 
                    ymax = 0.55) 
  
plot

ggsave("Oregon_playmaker.png")
  
  
#ggdraw() +
##  draw_plot(plot)  +
#  draw_image("https://www.decalsextremeonline.com/assets/images/oregonduckssilverhelmet.jpg",
#             x = 0, y = 0.4, scale = .1) +
#  draw_image("https://www.riddell.com/medias/sys_master/images/images/h64/h2d/8803035349022/-1200Wx1200H-000000000008036681-1.png",
#             x = 5, y = 0.3, scale = .1) +
#  theme(
#    plot.background = element_rect(fill = '#FFFEF2')
#  )








