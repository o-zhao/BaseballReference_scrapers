# Baseball-Reference.com scraper for minor/foreign league stats
# PITCHERS
# last edited 17 July 24 (ozhao)

setwd()

library('rvest')
library('dplyr')
library('gsubfn')

# set up and CCS selectors
allteams <- c('ARI','ATL','BAL','BOS','CHC','CHW','CIN','CLE','COL','DET','HOU','KCR','ANA','LAD','FLA','MIL','MIN','NYM','NYY','OAK','PHI','PIT','SDP','SFG','SEA','STL','TBD','TEX','TOR','WSN')
scrapeteams <- c('KCR')

scrapeseasons <- seq(2017, 2017, 1)

## part 1: pull bb-r ID
url <- 'http://www.baseball-reference.com/minors/'
pitch_table <- 'table#team_pitching'
pitch_id <- paste0(pitch_table, ' a')

pitcherlist <- c()

## part 2: pull stats, roster history, personal info
url2 <- 'http://www.baseball-reference.com/register/player.fcgi?id='
stats_table <- 'table#standard_pitching'
roster_table <- 'table#standard_roster'
name <- 'h1[itemprop="name"]'
position <- '#meta > div > p:nth-child(2)'
hand <- '#meta > div > p:nth-child(3)'
birthdate <- 'span#necro-birth'
birthplace <- 'span[itemprop="birthPlace"]'

pitchers_stats <- data.frame()
pitchers_roster <- data.frame()
pitchers_info <- data.frame(name=character(), position=character(), bat=character(), throw=character(), birthdate=character(), birthplace=character(), bbrid=character())


# scrape functions
pullname_nocomment <- function(url,selector){
  read_html(url) %>%
    html_nodes(selector) %>%
    html_attr(name = 'href') %>% unlist %>% as.character
}

pullname_comment <- function(url,selector){
  read_html(url) %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse = '') %>%
    read_html() %>%
    html_nodes(selector) %>%
    html_attr(name = 'href') %>% unlist %>% as.character
}

pulltable_nocomment <- function(url,selector){
  read_html(url) %>%
    html_node(selector) %>%
    html_table() %>%
    data.frame() %>%
    tbl_df()
}

pulltable_comment <- function(url,selector){
  read_html(url) %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse = '') %>%
    read_html() %>%
    html_node(selector) %>%
    html_table() %>%
    data.frame() %>%
    tbl_df()
}

pullinfo <- function(url, selector){
  read_html(url) %>%
    html_nodes(selector) %>%
    html_text(trim=T)
}

# part 1: loop over season years and teams to get players' bb-r ID
system.time( # timer to infer average scrape times
  
  for (team in scrapeteams){ 
    for (season in scrapeseasons) {
      url_full <- paste(url,"affiliate.cgi?id=",team,"&year=",season,sep="")
      
      pitcherlist_temp <- try(pullname_comment(url_full, pitch_id), silent=F)
      if (inherits(pitcherlist_temp, "try-error")) pitcherlist_temp <- pullname_nocomment(url_full, pitch_id)
      
      pitcherlist_temp <- gsub('/register/player.fcgi?id=', '', pitcherlist_temp, fixed=T)
      
      pitcherlist <- union(pitcherlist, pitcherlist_temp)
      
    }
  }
)

# part 2: loop over bb-r ID to get players' stats, roster history, info
system.time(
  
  for (pitcher in pitcherlist){ 
    url_full <- paste(url2,pitcher,sep="")
    download.file(url_full, destfile='tempfile.html')
    url_local <- 'tempfile.html'
    
    ## pull stats
    df_pitch <- try(pulltable_nocomment(url_local, stats_table), silent=F)
    if (inherits(df_pitch, "try-error")) df_pitch <- pulltable_comment(url_local, stats_table)
    df_pitch$bbrid <- pitcher
    
    ## pull roster history
    df_roster <- try(pulltable_comment(url_local, roster_table), silent=F)
    if (inherits(df_roster, "try-error")) df_roster <- pulltable_nocomment(url_local, roster_table)
    df_roster$bbrid <- pitcher
    
    ## pull info
    name_temp <- pullinfo(url_local, name)
    position_temp <- pullinfo(url_local, position)
    hand_temp <- pullinfo(url_local, hand)
    birthdate_temp <- pullinfo(url_local, birthdate)
    birthplace_temp <- pullinfo(url_local, birthplace)
    
    ### regex cleaning
    position_temp <- gsub('\n', '', position_temp, fixed=T)
    position_temp <- gsub('\r', '', position_temp, fixed=T)
    position_temp <- sub('Position:', '', position_temp, fixed=T)
    position_temp <- sub('Positions:', '', position_temp, fixed=T)
    
    bat_temp <- strapplyc(hand_temp, "Bats: (\\w+)")[[1]]
    throw_temp <- strapplyc(hand_temp, "Throws: (\\w+)")[[1]]
    
    birthdate_temp <- gsub('\n', '', birthdate_temp, fixed=T)
    birthdate_temp <- gsub('\r', '', birthdate_temp, fixed=T)
    
    birthplace_temp <- gsub('\n', '', birthplace_temp, fixed=T)
    birthplace_temp <- gsub('\r', '', birthplace_temp, fixed=T)
    birthplace_temp <- gsub('in', '', birthplace_temp, fixed=T)
    
    if (length(birthdate_temp)==0) birthdate_temp <- ''
    if (length(birthplace_temp)==0) birthplace_temp <- ''
    
    df_info <- data.frame(Name=name_temp, Position=position_temp, Bat=bat_temp, Throw=throw_temp, Birthdate=birthdate_temp, Birthplace=birthplace_temp, bbrid=pitcher)
    
    ## appending to master results
    pitchers_stats <- rbind(pitchers_stats, df_pitch)
    pitchers_roster <- rbind(pitchers_roster, df_roster)
    pitchers_info <- rbind(pitchers_info, df_info)
    
    print(pitcher)
    
  }
)

# part 3: cleaning
## removing total or header rows from the stats tables
leagueremove <- c('', '2 Lgs', '3 Lgs', '4 Lgs', '5 Lgs', '6 Lgs', 'Lg')
teamremove <- c('2 Teams', '3 Teams', '4 Teams')

pitchers_stats <- subset(pitchers_stats, (!(Lg %in% leagueremove) & !(Tm %in% teamremove)))


# save to files
write.csv(pitchers_stats, file='pitchers_stats.csv')
write.csv(pitchers_roster, file='pitchers_roster.csv')
write.csv(pitchers_info, file='pitchers_info.csv')
write.csv(pitcherlist, file = 'pitcherlist.csv', row.names=F)


###################### END ########################



