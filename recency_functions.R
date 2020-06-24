# functions for recency

# return data frame of modification times for posterior distributions

last_posterior_date=function() {
  dir="."
  pat="*._post.rds"
  v=list.files(path=dir,pattern=pat,full.names = T)
  v_short=list.files(path=dir,pattern=pat)
  file.info(v) %>%  
    mutate(fname=v_short) %>% arrange(mtime) %>% 
    select(fname,mtime)
}

# when last game with score was played

last_game_date=function(games, comp_no) {
  games %>% filter(comp %in% comp_no) %>% 
    filter(str_detect(score," - ")) -> d
  if (nrow(d)==0) return(0)
  d %>% 
    arrange(desc(time_stamp)) %>% 
    pluck("time_stamp",1)
}

# when first game without score is played

last_ungotten=function(games, comp_no, after_now=F) {
  games %>% filter(comp %in% comp_no) %>% 
    filter(!str_detect(score," - ")) %>% 
    filter(!str_detect(score,"Postponed")) %>% 
    filter(!str_detect(score,"Suspended")) %>% 
    filter(!str_detect(score,"Cancelled")) %>% 
    filter((time_stamp-Sys.time())/ddays(1)>-7) %>% 
    arrange(time_stamp) -> ug
  if (after_now) {
    now=Sys.time()
    ug %>% filter(time_stamp+hours(3)>now) -> ug
  }
  if (nrow(ug)==0) return(Sys.time()+ddays(200))
  ug %>% pluck("time_stamp", 1)
}

# iso week with half for midweek

iso_week_half=function(date) {
  iw=isoweek(date-days(1))
  dow=wday(date, label=T)
  ym=year(date)-2019
  iw=ifelse(dow %in% c("Tue", "Wed", "Thu"), iw-0.5, iw) 
  iw+ym*53
}
