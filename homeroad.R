options(stringsAsFactors = F)
library(tidyverse)
library(rvest)

#scraping standings from 2016-2021 seasons.

stdf = NULL
for (s in 2016:2021){
  if (s == 2021){
    html = read_html(paste0('https://www.basketball-reference.com/leagues/NBA_',s,'_standings.html'))
    dat = html %>% html_table()
    east = dat[[1]] %>% as.matrix();west = dat[[2]] %>% as.matrix();colnames(east) = NULL;colnames(west) = NULL
    st = rbind.data.frame(east, west) %>% select(V1, V4)
    for (i in 1:nrow(st)){
      sp = strsplit(st$V1[i], '')[[1]]
      st$V1[i] = paste0(sp[1:(which(sp == '(')-2)], collapse = '')
    }
    st$Yr = s
  } else{
    html = read_html(paste0('https://www.basketball-reference.com/leagues/NBA_',s,'_standings.html'))
    dat = html %>% html_table()
    east = dat[[1]] %>% as.matrix();west = dat[[2]] %>% as.matrix();colnames(east) = NULL;colnames(west) = NULL
    st = rbind.data.frame(east, west) %>% select(V1, V4)
    for (i in 1:nrow(st)){
      sp = strsplit(st$V1[i], '')[[1]]
      if (sp[length(sp)] == '*'){
        st$V1[i] = paste0(sp[1:(length(sp)-1)], collapse = '')
      } else{
        st$V1[i] = st$V1[i]
      }
    }
    st$Yr = s
  }
  stdf = rbind.data.frame(stdf, st)
}
for (i in 1:nrow(stdf)){if (grepl("\\*", stdf$V1[i])){stdf$V1[i] = strsplit(stdf$V1[i], "\\*")[[1]]} else{stdf$V1[i] = stdf$V1[i]}}
stdf %>% write.csv('standingsFull.csv')

#scraping results from 2016-2021 seasons.
months = c('january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december', 'october-2019')
monthswn = cbind.data.frame(num = 1:12, mon = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
fdf = NULL
for (s in 2016:2021){
  seasondf = NULL
  if (s == 2020){op = c(13, 11, 12, 1, 2, 3)}else{op = 1:12}
  for (m in op){
    html = try(read_html(paste0('https://www.basketball-reference.com/leagues/NBA_',s,'_games-',months[m],'.html')), TRUE)
    if (class(html[1]) == 'list'){
      dat = html %>% html_table()
      keep = dat[[1]]
      keep = keep[-c(which(names(keep)==''))]
      names(keep) = c('Date','StartTime','Away','AwayPts','Home','HomePts','Attend','Notes')
      keep = keep %>% as_tibble() %>% select(-Notes)
      keep$season = s
    } else{
      keep = NULL
    }
    seasondf = rbind.data.frame(seasondf, keep)
    print(paste(m, s))
  }
  fdf = rbind.data.frame(fdf, seasondf)
  print(paste(s))
}
fdf %>% write.csv('inArenaResults.csv')

df = read.csv('inArenaResults.csv')[,-1] %>% as_tibble()
df = df[-which(is.na(df$AwayPts)),]
df = df[-which(df$Away=="Playoffs"),]
df$Attend = ifelse(df$Attend == "", "No", "Yes")
df[,c("AwayPts", "HomePts")] = sapply(df[,c("AwayPts", "HomePts")], as.numeric)
df = df %>% mutate(pointDiff = HomePts-AwayPts)
sta = read.csv('standingsFull.csv')[,-1] %>% as_tibble()

df$AwayWP = 0; df$HomeWP = 0
for (i in 1:nrow(df)){
  df$AwayWP[i] = sta$V4[which(sta$V1 == df$Away[i] & sta$Yr == df$season[i])]
  df$HomeWP[i] = sta$V4[which(sta$V1 == df$Home[i] & sta$Yr == df$season[i])]
}

df = df %>% mutate(WPDiff = HomeWP - AwayWP)
#df %>% ggplot(aes(x = WPDiff, y = pointDiff)) + geom_point(color = "steelblue", alpha = I(1/2)) + theme_bw()

df$Winner = ifelse(df$pointDiff > 0, "Home", "Away")
df %>% ggplot(aes(x = pointDiff, fill = Winner)) + geom_histogram(alpha = I(.90), bins = 60) + geom_vline(xintercept = 0, linetype = "dashed") + facet_grid(season ~ ., scales = "free_y") + scale_fill_manual("", values = c("#66C2A5","#5E4FA2")) + theme(legend.position = "top") + scale_x_continuous("Point Differential", breaks = seq(-60, 60, 10)) + scale_y_continuous("Games")
df %>% group_by(season) %>% summarise(.groups = "drop", pointDiff = mean(pointDiff))
summary(df$WPDiff)

df %>% ggplot(aes(x = WPDiff)) + geom_density(fill = "grey95") + scale_x_continuous("Win Percentage Difference", breaks = seq(-1, 1, 0.2)) + geom_vline(xintercept = c(-0.146, 0, 0.146), alpha = I(1/2)) + theme_bw() + annotate(geom = 'text', x = -0.22, y = 0.5, label = 'Away Favored', angle = 270) + annotate(geom = 'text', x = -0.07, y = 0.70, label = 'Away Slightly Favored', angle = 270)+ annotate(geom = 'text', x = 0.07, y = 0.71, label = 'Home Slightly Favored', angle = 270)+ annotate(geom = 'text', x = 0.22, y = 0.5, label = 'Home Favored', angle = 270)
df$WPDiffCat = ifelse(df$WPDiff <= -0.146, "Away Favored", ifelse(df$WPDiff < 0, "Away Slightly Favored", ifelse(df$WPDiff <= 0.146, "Home Slightly Favored", "Home Favored")))
df$WPDiffCat = factor(df$WPDiffCat, levels = c("Away Favored", "Away Slightly Favored", "Home Slightly Favored", "Home Favored"))
df %>% ggplot(aes(x = pointDiff, fill = Winner)) + geom_histogram(alpha = I(.90), bins = 60) + geom_vline(xintercept = 0, linetype = "dashed") + facet_grid(season ~ WPDiffCat, scales = "free_y") + scale_fill_manual("", values = c("#66C2A5","#5E4FA2")) + theme(legend.position = "none") + scale_x_continuous("Point Differential", breaks = seq(-40, 40, 20)) + scale_y_continuous("Games")

catmeans = df %>% group_by(season, WPDiffCat) %>% summarise(.groups = "drop", pointDiff = mean(pointDiff)) %>% as.data.frame() %>% arrange(WPDiffCat)
catmeans$WPDiffCat = factor(catmeans$WPDiffCat, levels = rev(unique(catmeans$WPDiffCat)))
catmeans %>% ggplot(aes(x = season, y = pointDiff, color = WPDiffCat)) + geom_hline(yintercept = 0, color = "grey50") + geom_line(size = 1) + geom_point() + scale_color_manual("", values = c("#67001F","#B2182B","#D6604D","#F4A582")) + theme_bw() + scale_x_continuous("") + scale_y_continuous("Average Point Differential")

cattab = df %>% group_by(season, WPDiffCat, Winner) %>% summarize(.groups = "drop", Games = n())
ndf = cattab %>% group_by(season, WPDiffCat) %>% summarise(.groups = "drop", n = sum(Games))
cattab$n = 0
for (i in 1:nrow(cattab)){
  if (i %% 2 == 0){
    cattab$n[i] = cattab$Games[i-1] + cattab$Games[i]
  } else{
    cattab$n[i] = cattab$Games[i+1] + cattab$Games[i]
  }
}

cattab %>% ggplot(aes(x = season, y = Games, fill = Winner)) + geom_bar(color = "grey5", stat = "identity", position = "fill", alpha = I(0.90)) + facet_grid(. ~ WPDiffCat, scales = "free_y") + scale_x_continuous("", breaks = 2016:2021) + scale_y_continuous("Proportion of Games Won") + scale_fill_manual("", values = c("#66C2A5","#5E4FA2")) + theme_bw() + geom_hline(yintercept = 0.50, linetype = "dotted", color = "grey15") +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(y = .05, label = n), angle = 90, hjust = 0, check_overlap = T)



