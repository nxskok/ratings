## functions.R

# functions for making prior and posterior

# given country name, return all leagues in that country

country_to_leagues=function(games,country) {
  games %>% select(comp, comp_name, time_stamp, country) %>% 
    group_by(comp) %>% 
    summarize(nm=min(comp_name),nt=min(time_stamp),nc=min(country)) %>% 
    filter(str_detect(tolower(nc), tolower(country))) %>% pull(nm) %>% unique() %>% sort()
}

# given country and league name, return all seasons of that with numbers
# nt is the *first* game date

country_to_league_number=function(games,ctry,league_name) {
  games %>% filter(comp_name == league_name) %>% 
    filter(str_detect(country, ctry)) %>% 
    group_by(comp) %>% 
    summarize(nm=min(comp_name),nt=min(time_stamp),nc=max(country),nm=n()) %>% 
    arrange(desc(nt)) %>% 
    filter(nc == country)
}

# return all the games in given comp number (which can be a vector)

comp_games=function(games,comp_number) {
  games %>% filter(comp %in% comp_number)
}

# make a lookup table of all the teams in the league

team_lookup_table=function(the_games) { # the_games is output from comp_games 
  team_names=with(the_games,c(t1_name,t2_name))
  team_names=sort(unique(team_names))
  tibble(team=team_names) %>% mutate(id=row_number())
}

# create input to use for Stan

make_stan=function(the_games,lookup_table) {
  the_games %>% 
    left_join(lookup_table,by=c("t1_name"="team")) %>% 
    left_join(lookup_table,by=c("t2_name"="team")) %>% 
    separate(score,c("s1","s2"),convert=T) %>% 
    mutate(s1=as.numeric(s1), s2=as.numeric(s2)) %>% 
    select(t1=id.x, t2=id.y, s1, s2) %>% 
    drop_na(s2)
}

# run stan to create prior for future seasons

make_stan_prior=function(games,comp_id,save_name,iterations=10000) {
  games %>% comp_games(comp_id) -> gg
  gg %>% team_lookup_table() -> lookup_table
  gg_stan=make_stan(gg, lookup_table)
  nt=nrow(lookup_table)
  with(gg_stan,list(
    nt=nt,
    ng=nrow(gg_stan),
    x=cbind(t1,t2),
    y=cbind(s1,s2),
    prior_o_mean=rep(0,nt),
    prior_o_sd=rep(1,nt),
    prior_d_mean=rep(0,nt),
    prior_d_sd=rep(1,nt),
    prior_h_mean=0,
    prior_h_sd=1
  )) -> 
    stan_prior_data
  p.sc=readRDS("psc.rds")
  ans=sampling(p.sc,stan_prior_data,iter=iterations)
  rat=extract(ans)
  tibble(id=1:nt,
         o=apply(rat$o,2,mean),
         os=apply(rat$o,2,sd),
         d=apply(rat$d,2,mean),
         ds=apply(rat$d,2,sd),
         h=mean(rat$h),
         hs=sd(rat$h)
  ) %>% 
    left_join(lookup_table)   %>%
    select(team,id,everything())  %>%
    write_csv(save_name)
}

# league table

table_of=function(games,lg_number) {
  games %>% 
    filter(comp %in% lg_number) %>% 
    filter(str_detect(score," - "))%>% 
    separate(score,c("s1","s2"),sep=" - ",convert=T) %>%
    select(t1_name,t2_name,s1,s2) %>% 
    mutate(result=case_when(
      s1>s2 ~ "W",
      s1==s2 ~ "D",
      s1<s2 ~ "L",
      TRUE ~ "X"
    )) %>% 
    mutate(pt1=case_when(
      result=="W" ~ 3,
      result=="D" ~ 1,
      result=="L" ~ 0
    )) %>% 
    mutate(pt2=case_when(
      result=="W" ~ 0,
      result=="D" ~ 1,
      result=="L" ~ 3
    )) -> 
    d
  d %>% group_by(t1_name) %>% 
    summarize(P=n(),
              GD=sum(s1)-sum(s2),
              Pt=sum(pt1)
    ) -> 
    tab1
  d %>% group_by(t2_name) %>% 
    summarize(P=n(),
              GD=sum(s2)-sum(s1),
              Pt=sum(pt2)
    ) -> 
    tab2
  tab1 %>% left_join(tab2,by=c("t1_name"="t2_name")) %>% 
    mutate(P=P.x+P.y,GD=GD.x+GD.y,Pt=Pt.x+Pt.y) %>% 
    select(t1_name,P,GD,Pt) %>% 
    arrange(desc(Pt),desc(GD))
}

get_prior=function(priorname) {
  read_csv(priorname)
}

make_posterior=function(prior,league_number,iterations=10000,multiplier=1.5) {
  games %>% filter(comp %in% league_number) -> current_games
  current_games %>% 
    filter(str_detect(score," - ")) %>% 
    separate(score,c("s1","s2"),sep=" - ",convert=T) %>% 
    select(t1_name,t2_name,s1,s2) -> gg
  if (nrow(gg)==0) { # no games yet
    # pull off first row from current_games and fill in 2-1 score for it
    current_games %>% slice(1) %>% 
      mutate(s1=2, s2=1) %>% 
      select(t1_name,t2_name,s1,s2) ->
      gg
  }
  # get rid of any spaces at front or back of team names
  gg %>% mutate_at(vars(ends_with("name")), ~trimws(.)) -> gg
  # the below should squeal if I got any team names wrong in prior
  gg %>% left_join(prior,by=c("t1_name"="team")) %>% 
    left_join(prior,by=c("t2_name"="team")) %>%
    select(t1=id.x,t2=id.y,s1,s2) ->
    post_stan
  # print(post_stan) # any NA in here indicate problems in prior
  post_data=with(post_stan,list(
    nt=nrow(prior),
    ng=nrow(post_stan),
    x=cbind(t1,t2),
    y=cbind(s1,s2),
    prior_o_mean=prior$o,
    prior_o_sd=multiplier*prior$os,
    prior_d_mean=prior$d,
    prior_d_sd=multiplier*prior$ds,
    prior_h_mean=prior$h[1],
    prior_h_sd=multiplier*prior$hs[1]
  ))
  X=with(post_stan, cbind(t1,t2, s1, s2))
  if (any(is.na(X))) {
    print("NAs in x")
    return(X)
  }
  p.sc=readRDS("psc.rds")
  post_now=sampling(p.sc,data=post_data,iter=iterations)
}

display=function(post,prior) {
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

post_df=function(prior,post_thing) { # needs fixing for RL. Does it work for soccer? Am I using it? I suspect I need to redo priors like for soccer. Or something.
  rat=extract(post_thing)
  tibble(id=1:nrow(prior),
         o=apply(rat$o,2,mean),
         d=apply(rat$d,2,mean),
         h=mean(rat$h)) %>% 
    left_join(prior,by=c("id")) %>% 
    select(name,id,o=o.x,d=d.x) %>% 
    mutate(sum=o+d,diff=o-d) %>% 
    arrange(desc(sum))
}
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
    geom_path(aes(group=team),colour="darkgreen", arrow=arrow(type="closed", ends="first", length=unit(0.06, "inches")))+
    geom_text_repel(colour="black") +
    xlab("Openness") + ylab("Quality") + ggtitle(str_to_title(namelc))
}


###################################################################
# added 2019-09-02


name_lookup_table=function(games, lno) {
  games %>% comp_games(lno) -> d
  d %>% select(sw_id=t1, name=t1_name) -> h
  d %>% select(sw_id=t2, name=t2_name) -> a
  h %>% bind_rows(a) %>% 
    group_by(sw_id) %>% 
    count(name) %>% 
    top_n(n=1, wt=n) %>% 
    ungroup() %>% 
    mutate(stan_id=row_number()) %>% 
    select(-n)
}

team_changes=function(games, lno_old, lno_new) {
  names_old=name_lookup_table(games, lno_old)
  names_new=name_lookup_table(games, lno_new)
  names_old
  names_old %>% anti_join(names_new,by="sw_id") -> removed # removed from
  names_new %>% anti_join(names_old,by="sw_id") -> added # added to
  list(removed=removed, added=added)
}

make_stan_prior_2=function(games,comp_id,save_name,iterations=10000) {
  games %>% comp_games(comp_id) -> gg
  games %>% name_lookup_table(comp_id) -> lookup_table
  # gg_stan=make_stan(gg, lookup_table)
  gg %>% select(t1, t2, score) %>% filter(str_detect(score, " - ")) %>% 
    left_join(name_lookups, by=c("t1"="sw_id")) %>% 
    left_join(name_lookups, by=c("t2"="sw_id")) -> looked_up
  looked_up %>%
    select(starts_with("stan")) %>%  as.matrix() -> stan_X
  looked_up %>% select(score) %>% 
    separate(score, into=c("z1", "z2", "z3", "z4"), fill = "right") %>% 
    mutate(s1=ifelse(is.na(z4), as.numeric(z1), as.numeric(z2)),
           s2=ifelse(is.na(z4), as.numeric(z2), as.numeric(z3))) %>% 
    select(s1, s2) %>% as.matrix() -> stan_y
  nt=nrow(lookup_table)
  stan_prior_data=list(
    nt=nt,
    ng=nrow(stan_X),
    x=stan_X,
    y=stan_y,
    prior_o_mean=rep(0,nt),
    prior_o_sd=rep(1,nt),
    prior_d_mean=rep(0,nt),
    prior_d_sd=rep(1,nt),
    prior_h_mean=0,
    prior_h_sd=1
  )
  p.sc=readRDS("psc.rds")
  ans=sampling(p.sc,stan_prior_data,iter=iterations)
  rat=extract(ans)
  tibble(id=1:nt,
         o=apply(rat$o,2,mean),
         os=apply(rat$o,2,sd),
         d=apply(rat$d,2,mean),
         ds=apply(rat$d,2,sd),
         h=mean(rat$h),
         hs=sd(rat$h)
  ) %>% 
    left_join(lookup_table, by=c("id"="stan_id")) %>%
    select(everything())  %>%
    write_csv(save_name)
}


make_posterior_2=function(prior,league_number,iterations=10000,multiplier=1.5) {
  games %>% filter(comp %in% league_number) -> current_games
  current_games %>% 
    filter(str_detect(score," - ")) %>% 
    separate(score, into=c("z1", "z2", "z3", "z4"), fill = "right") %>% 
    mutate(s1=ifelse(is.na(z4), as.numeric(z1), as.numeric(z2)),
           s2=ifelse(is.na(z4), as.numeric(z2), as.numeric(z3))) %>% 
    select(t1, t2, s1, s2) -> gg
  if (nrow(gg)==0) { # no games yet
    # pull off first row from current_games and fill in 2-1 score for it; this fails if there are no games at all
    current_games %>% slice(1) %>% 
      mutate(s1=2, s2=1) %>% 
      select(t1,t2,s1,s2) ->
      gg
  }
  # look up stan team ids from sw ids
  gg %>% left_join(prior, by=c("t1"="sw_id")) %>% 
    left_join(prior, by=c("t2"="sw_id")) %>% 
    select(t1, t2, id.x, id.y, s1, s2)  -> post_stan
  # print(post_stan) # any NA in here indicate problems in prior
  post_data=with(post_stan,list(
    nt=nrow(prior),
    ng=nrow(post_stan),
    x=cbind(id.x, id.y),
    y=cbind(s1, s2),
    prior_o_mean=prior$o,
    prior_o_sd=multiplier*prior$os,
    prior_d_mean=prior$d,
    prior_d_sd=multiplier*prior$ds,
    prior_h_mean=prior$h[1],
    prior_h_sd=multiplier*prior$hs[1]
  ))
  print(post_data)
  X=with(post_stan, cbind(t1,t2, s1, s2))
  print(X)
  if (any(is.na(X))) {
    print("NAs in x")
    return(X)
  }
  p.sc=readRDS("psc.rds")
  # post_now=sampling(p.sc,data=post_data,iter=iterations, refresh=-1)
  post_now=sampling(p.sc,data=post_data,iter=iterations)
}


display_2=function(post,team_names) {
  rat=extract(post)
  tibble(id=1:nrow(prior),
         o=apply(rat$o,2,mean),
         d=apply(rat$d,2,mean),
         h=mean(rat$h)
  ) %>% 
    left_join(team_names,by=c("id"="stan_id")) %>% 
    select(name, everything()) %>%
    mutate(sum=o+d,diff=o-d) %>% 
    arrange(desc(sum))
}
