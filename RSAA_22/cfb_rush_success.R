library(tidyverse)
library(cfbfastR)
library(lme4)
library(rstanarm)
library(gt)
library(mgcv)

pbp<-load_cfb_pbp(seasons = c(2016:2022)) |>
  dplyr::filter(rush == 1)

roster<-load_cfb_rosters(seasons = c(2016:2022))


glimpse(pbp)

rush_df<-pbp |>
  dplyr::select(success,score_diff_start,yards_to_goal,
                down,rush_player_id,pos_team,year,def_pos_team,drive_event_number,
                drive_time_seconds_start,drive_time_minutes_start,drive_number,TimeSecsRem,
                adj_TimeSecsRem,distance,drive_play_number,game_play_number) |>
  dplyr::mutate(rush_player_id = as.character(rush_player_id)) |>
  dplyr::left_join(roster,by=c("rush_player_id" = "athlete_id","pos_team" = "team","year" = 
                                 "season")) |>
  dplyr::select(-year.y) %>%
  dplyr::mutate(def_pos_team = as.factor(def_pos_team),
                pos_team = as.factor(pos_team))

rush_df<-na.omit(rush_df)




rush_mle<-lme4::glmer(factor(success) ~
                       score_diff_start +
                        yards_to_goal +
                       down + drive_event_number +
                        drive_number + adj_TimeSecsRem + distance + drive_play_number + 
                        game_play_number + 
                        (1|rush_player_id) +
                        (1|pos_team) +
                        (1|def_pos_team),
                      data = rush_df,
                      family = binomial(link = "probit"),
                      control = glmerControl(optimizer="Nelder_Mead"))



summary(rush_mle)



rush.success.mod.mcmc<-stan_glmer(success ~ score_diff_start +
                                    yards_to_goal + down +
                                    (1|rush_player_id) +
                                    (1|pos_team),
                                  data = rush_df,
                                  family=binomial(link="logit"),
                                  chains = 4,
                                  prior_intercept = normal(0,10),
                                  prior = normal(0,1),
                                  prior_aux = exponential(1),
                                  prior_covariance = decov(1,1,1,1),
                                  adapt_delta = .8,
                                  iter = 1000,
                                  QR = FALSE)


#### simulate
RE.sims<-merTools::REsim(rush_mle,n.sims=10000,seed=1234)

rusher.sims<-RE.sims %>%
  dplyr::filter(groupFctr == "rush_player_id")

ros<-roster %>%
  dplyr::select(athlete_id,first_name,last_name,position,headshot_url) %>%
  dplyr::distinct()

rusher.sims<- rusher.sims %>%
  dplyr::left_join(ros,by=c("groupID" = "athlete_id"))


rb_sims<-rusher.sims %>%
  dplyr::filter(position == "RB")



rush_stats<-pbp %>%
  dplyr::group_by(rush_player_id) %>%
  dplyr::summarise(n = n(),
                   success = sum(success),
                   school = last(pos_team),
                   year = last(year),
                   yds = sum(yards_gained)) %>%
  dplyr::mutate(rush_player_id = as.character(rush_player_id))

rb_sims<-rb_sims %>%
  dplyr::left_join(rush_stats,by=c("groupID" = "rush_player_id"))

yrs<-roster %>%
  dplyr::group_by(athlete_id) %>%
  dplyr::summarise(years = n_distinct(season))

rb_sims<-rb_sims %>%
  dplyr::left_join(yrs,by=c("groupID" = "athlete_id"))


rb_sims %>%
  ggplot() + 
  aes(x = n) +
  geom_density()



rb_sims %>%
  dplyr::filter(year == "2022",
                n > 175) %>%
  dplyr::arrange(desc(median)) %>%
  head(10L) %>%
  mutate(floor = round(mean - sd,3),
         ceiling = round(mean + sd,3),
         median = round(median,3),
         name = paste0(first_name," ",last_name),
         success_rt = round(success/n,3)) %>%
  dplyr::select(name,headshot_url,school,median,floor,ceiling,success_rt,n,yds) %>%
  dplyr::filter(!name %in% c("Jakobi Buchanan","Lew Nichols")) %>%
  gt() %>%
  text_transform(
    locations = cells_body(c(headshot_url)),
    fn = function(x){
      web_image(
        url = x,
        height = px(30)
      )
    }
  ) %>% 
  cols_label(
    headshot_url = "",
    name = "Name",
    school = "Team",
    floor = "Floor RSAA",
    median = "RSAA",
    ceiling = "Ceiling RSAA",
    n = "Rushes",
    success_rt = "Success Rate",
    yds = "Yards"
  ) %>% 
  data_color(
    columns = c(`median`),
    colors = scales::col_numeric(
      palette = c("#af8dc3", "#f7f7f7", "#7fbf7b"),
      domain = NULL
    )
  )  %>% 
  #tab_style(
  #  style = cell_text(weight = "bold"),
  #  locations = cells_body(
  #    columns = c(name,school)
  #  )
  #) %>% 
  tab_options(
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 12,
    heading.align = "left"
  ) %>%
  opt_table_font(
    font = list(
      google_font("Mono"),
      default_fonts()
    )
  )  %>%
  tab_spanner(
    label = "Rushing Success Above Average",
    columns = 4:6
  ) %>%
  tab_spanner(
    label = "Career Stats",
    columns = 7:9
  ) %>%
  tab_footnote(
    footnote = "Bold Names: PFF Early 2023 NFL Draft Top 10 RBs according to Mike Renner",
    locations = cells_column_labels(
      columns = 1
    )
  ) %>%
  tab_footnote(
    footnote = "Measures Median Rushing Success above Relative Performance",
    locations = cells_column_labels(
      columns = 4
    )
  ) %>%
  tab_footnote(
    footnote = "Floor: Lower Quartile RSAA range",
    locations = cells_column_labels(
      columns = 5
    )
  ) %>%
  tab_footnote(
    footnote = "Ceiling: Higher Quartile RSAA range",
    locations = cells_column_labels(
      columns = 6
    )
  ) %>%
  tab_footnote(
    footnote = "Rushing Success Rate: % of Carries where rusher gains 40% of needed yards on 1st down, 60% on 2nd, 100% on later downs",
    locations = cells_column_labels(
      columns = 7
    )
  ) %>%
  tab_source_note(
    source_note = "Data: cfbfastR"
  ) %>%
  tab_header(
    title = md("**Top 2022 Career Rushers with Highest Relative Rushing Success**"),
    subtitle = md("Viz: @QuinnsWisdom | Career Rushes: 175+")
  ) %>%
  fmt_percent(
    decimals = 1,
      columns =  c(median,floor,ceiling,success_rt)
    ) %>%
  fmt_number(
    sep_mark = ",",
    decimals = 0,
    columns = c(yds)
  ) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = list(
              cells_body(
                columns = 1,
                rows = c(1,5,7)
              )
              )
            ) 
  
  

  








  



  
