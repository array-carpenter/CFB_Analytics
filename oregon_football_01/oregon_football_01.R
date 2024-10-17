library(cfbfastR)
library(ggtext)


install.packages("cfbfastR")

####################3 RETURNING PRODUCTION ##############

#### Question: How unknown is this Oregon offense ahead of their 2022 opener vs Georgia

Sys.setenv(CFBD_API_KEY = Sys.getenv("CFBD_API_KEY"))

team_return<-data.frame()
talent_mix<-data.frame()

for(i in 2014:2022) {
  re<-cfbd_player_returning(year = i,team = "Oregon")
  
  team_return<-bind_rows(team_return,re)
  
  
}

### bind w/ Georgia
for(i in 2014:2022) {
  re<-cfbd_player_returning(year = i,team = "Georgia")
  
  team_return<-bind_rows(team_return,re)
  
  
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
  plot.background = element_rect(fill = '#FFFEF2', color = NA),
  panel.background = element_rect(fill = '#FFFEF2', color = NA),
  plot.tag = element_text('mono', size = 10, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  legend.position = "none"
)



return %>%
  ggplot() +
  aes(x= season,y=usage,color=team) +
  geom_point(alpha = 0.5,size = 6) +
  geom_line(aes(group = season),color="grey") +
  scale_color_manual(
    name = NULL,
  values = c(Oregon = "#154733", Georgia = "#BA0C2F"),
) +
  labs(
    title = "Biggest UO Offensive Mystery",
    tag = "Data is from @cfbfastR",
    subtitle = "@QuinnsWisdom",
    x = "Season", y = "Returning Usage Pct") +
  annotation_custom(textGrob('Oregon', gp = gpar(col = '#154733')), 
                    xmin = 2014, xmax = 2015, ymin = 0.75, ymax = 0.95) +
  annotation_custom(textGrob('Georgia', gp = gpar(col = '#BA0C2F')), 
                    xmin = 2014, xmax = 2015, ymin = 0.45, ymax = 0.50) +
  scale_x_continuous(breaks=seq(2014, 2022, 1)) +
  scale_y_continuous(breaks=seq(0, 1, .1),labels = scales::percent_format(accuracy = 1))
    
  ggsave("UO Offense.png")  
    





