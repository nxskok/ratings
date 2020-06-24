## ------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
source("recency_functions.R")

## ------------------------------------------------------------------------
games=readRDS("~/teaching/scoresway/rds/games.rds")
(league_ids=readRDS("league_ids.rds"))

## ------------------------------------------------------------------------
league_ids %>% arrange(the_country) %>% mutate(row=row_number()) %>% select(the_country, row, id) -> li
tz="America/Toronto"
last_posterior_date() %>% separate(fname,into=c("league","rest"),extra="merge",sep=".(?=[^_]+$)") %>% 
  mutate(league=str_to_title(league)) %>% 
  left_join(li,by=c("league"="the_country")) %>% 
  mutate(iso_week_lr=iso_week_half(mtime)) %>% 
  mutate(last_game=map_dbl(id,~last_game_date(games,.))) %>% 
  mutate(last_game=as_datetime(last_game,tz=tz)) %>% 
  mutate(iso_week_last=iso_week_half(last_game)) %>% 
  mutate(next_ungotten=map_dbl(id, ~last_ungotten(games, .))) %>% 
  mutate(next_ungotten=as_datetime(next_ungotten,tz=tz)) %>% 
  mutate(iso_week_next=iso_week_half(next_ungotten)) %>% 
  mutate(since_rating=(last_game-mtime)/dhours(1)) %>% 
  mutate(ug_from_now=(next_ungotten-Sys.time())/dhours(1)) %>% 
  mutate(action=case_when(
    ug_from_now< (-3) ~ "get games",
    ug_from_now<0 ~ "more",
    since_rating<0 ~ "z",
    iso_week_next>iso_week_last ~ "a new rating",
    TRUE ~ "more coming later"
  )) %>% 
  arrange(action, next_ungotten) %>% 
  drop_na() %>% 
  select(league, last_rating=mtime, iso_week_lr, last_game, iso_week_last, next_ungotten, iso_week_next, row, action) %>% View("action")

