## ------------------------------------------------------------------------
library(tidyverse)
library(rstan)
library(ggrepel)
source("functions.R")

## ------------------------------------------------------------------------
games=readRDS("~/teaching/scoresway/rds/games.rds")
league_ids=readRDS("league_ids.rds")

## ------------------------------------------------------------------------
league_ids %>% arrange(the_country) %>% mutate(row=row_number()) %>% select(the_country, row, id) -> li
# View(li,"The leagues")

## ----league-no-----------------------------------------------------------
cno=3
(li %>% slice(cno) -> v)
v %>% pull(id) -> lnos
v %>% pull(the_country) -> country
v %>% pull(id) -> id

## ------------------------------------------------------------------------
games %>% filter(comp %in% lnos) %>% 
  arrange(desc(time_stamp)) %>% View("sched")

## ------------------------------------------------------------------------
country_lower=tolower(country)
prior_year=2019 # change only for Sweden/Nordic at end of year
priorname=str_c(country_lower,prior_year,"_prior.csv")
prepostname=str_c(country_lower,"_pre_post.rds")
postname=str_c(country_lower,"_post.rds")
prior=get_prior(priorname)
pre_post=readRDS(postname) # comment out these lines for a first time rating (ideally I want to use prior as pre_post)
saveRDS(pre_post, prepostname) # this one too
post=make_posterior(prior,lnos)
# display(post,prior) %>% ggplot(aes(x=diff,y=sum, label=team))+geom_point()+geom_text_repel()+xlab("Openness")+ylab("Quality")
saveRDS(post,postname)
beepr::beep(2)

## ------------------------------------------------------------------------
# namelc="netherlands"
namelc=tolower(country)
prior_year=2019
priorname=str_c(namelc, prior_year, "_prior.csv")
prior=get_prior(priorname)
postname=str_c(namelc, "_post.rds")
prepostname=str_c(namelc, "_pre_post.rds") # for a first-timer this won't work
post=readRDS(postname)
pre_post=readRDS(prepostname)

## ------------------------------------------------------------------------
games %>%  filter(comp==id) %>% filter(str_detect(score, " - ")) %>% 
  mutate(iso_week=iso_week_half(time_stamp)) %>% 
  filter(iso_week==max(iso_week)) %>% 
  select(t1_name, t2_name, score, time_stamp) %>% 
  separate(score, into=c("s1", "s2"), remove=F) %>% 
  mutate(score_rev=str_c(s2, s1, sep=" - ")) %>% 
  select(-s1, -s2) -> results
results %>% select(team=t1_name, opp=t2_name, score=score, time_stamp) %>% 
  mutate(venue="h") -> r1
results %>% select(team=t2_name, opp=t1_name, score=score_rev, time_stamp) %>% 
  mutate(venue="a") -> r2
bind_rows(r1, r2) -> all_results
all_results

## ------------------------------------------------------------------------
df1=post_df(prior,post)
df0=post_df(prior,pre_post)
df1 %>% left_join(df0, by=c("team"="team")) %>% 
  mutate(sum.x=sum.x-mean(sum.x),
         sum.y=sum.y-mean(sum.y)) %>% 
  mutate(delta_sum=sum.x-sum.y,
         delta_diff=diff.x-diff.y) %>% 
  mutate(pr_sum=percent_rank(sum.x),
         pr_diff=percent_rank(diff.x)) %>% 
  select(team, pr_sum, pr_diff, delta_sum, delta_diff) %>% 
  mutate_at(vars(starts_with("delta")), ~round(., 3)) %>% 
  left_join(all_results) -> z
# z %>% arrange(desc(abs(delta_diff)))
z %>% arrange(desc(delta_sum)) %>% View("new ratings")

## ------------------------------------------------------------------------
# namelc="ireland"
namelc=tolower(country)
prior_year=2019
priorname=str_c(namelc, prior_year, "_prior.csv")
priorname
prior=get_prior(priorname)
postname=str_c(namelc, "_post.rds")
# prepostname=str_c(namelc, "_pre_post.rds")
post=readRDS(postname)
# pre_post=readRDS(prepostname)
display
display(post, prior) %>% ggplot(aes(x=diff,y=sum, label=team))+geom_point()+geom_text_repel()+xlab("Openness")+ylab("Quality")+ggtitle(str_to_title(namelc))

## ------------------------------------------------------------------------
graph_it(prior, pre_post, post) +  coord_fixed() + coord_flip()

## ------------------------------------------------------------------------
# namelc="ireland"
namelc=tolower(country)
prior_year=2019
priorname=str_c(namelc, prior_year, "_prior.csv")
priorname
prior=get_prior(priorname)
postname=str_c(namelc, "_post.rds")
# prepostname=str_c(namelc, "_pre_post.rds")
post=readRDS(postname)
# pre_post=readRDS(prepostname)
display
display(post, prior) %>% ggplot(aes(x=diff,y=sum, label=team))+geom_point()+geom_text_repel()+xlab("Openness")+ylab("Quality")+ggtitle(str_to_title(namelc))

## ------------------------------------------------------------------------
  games %>% filter(comp %in% lnos) -> current_games
  current_games %>% 
    filter(str_detect(score," - ")) %>% 
    separate(score,c("s1","s2"),sep=" - ",convert=T) %>% 
    select(t1_name,t2_name,s1,s2) -> gg
gg

## ------------------------------------------------------------------------
country="Iceland"
(games %>% country_to_leagues(country) -> ll)
league_name=ll[6]
country_to_league_number(games,country,league_name)
lnos=47479

## ------------------------------------------------------------------------
post_df=function(prior,post_thing) {
  rat=extract(post_thing)
  tibble(id=1:nrow(prior),
         o=apply(rat$o,2,mean),
         d=apply(rat$d,2,mean),
         h=mean(rat$h)) %>% 
    left_join(prior,by=c("id")) %>% 
    select(team,id,o=o.x,d=d.x) %>% 
    mutate(sum=o+d,diff=o-d) %>% 
    arrange(desc(sum))
}

## ------------------------------------------------------------------------
graph_it=function(prior, pre_post, post) {
  df1=post_df(prior, post)
  df0=post_df(prior, pre_post)
  bind_rows(df1, df0, .id="which") -> d # 1 is new, 2 is old
  d %>% group_by(which) %>% summarize(mm=mean(sum)) -> means
  d %>% left_join(means) %>% # look up right mean for "which"
    mutate(sum=sum-mm) %>% 
    mutate(which=ifelse(which==1, "now", "previous")) %>% 
    mutate(mylab=ifelse(which=="now", team, "")) -> d2
  ggplot(d2,aes(x=diff,y=sum,colour=which,label=mylab))+
    geom_point()+
    geom_path(aes(group=team),colour="blue", arrow=arrow(type="closed", ends="last", length=unit(0.1, "inches")))+
    geom_text_repel(colour="black")  
}

## ------------------------------------------------------------------------
graph_it(prior, pre_post, post)

## ------------------------------------------------------------------------
df1=post_df(prior, post)
df0=post_df(prior, pre_post)
bind_rows(df1, df0, .id="which") -> d # 1 is new, 2 is old
d

## ------------------------------------------------------------------------
 ggplot(d,aes(x=diff,y=sum,colour=which,
                      label=team))+geom_point()+
    geom_line(aes(group=team),colour="green")+
    geom_text_repel()

## ------------------------------------------------------------------------
d
(d %>% group_by(which) %>% summarize(mm=mean(sum)) -> means)

## ------------------------------------------------------------------------
(d %>% left_join(means) %>% mutate(sum=sum-mm) %>% 
   mutate(which=ifelse(which==1, "present", "past")) %>% 
   mutate(mylab=ifelse(which=="present", team, "")) -> d2)

## ------------------------------------------------------------------------
 ggplot(d2,aes(x=diff,y=sum,colour=which,
                      label=mylab))+geom_point()+
    geom_path(aes(group=team),colour="blue", arrow=arrow(type="closed", ends="first", length=unit(0.1, "inches")))+
    geom_text_repel(colour="black")

## ------------------------------------------------------------------------
display=function(post,pre_post,prior) {
  rat=extract(post)
  tibble(id=1:nrow(prior),
         o=apply(rat$o,2,mean),
         d=apply(rat$d,2,mean),
         h=mean(rat$h)
  ) %>% 
    left_join(prior,by=c("id")) %>% 
    select(team,id,o=o.x,d=d.x) %>% 
    mutate(sum=o+d,diff=o-d) %>% 
    arrange(desc(sum))
}

