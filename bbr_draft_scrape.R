# Baseball-Reference.com scraper for draft results
# last edited 17 July 24 (ozhao)

setwd()

library('rvest')
library('dplyr')
library('gsubfn')

# set up and CCS selectors
url1 <- 'http://www.baseball-reference.com/draft/?year_ID='
url2 <- '&draft_round='
url3 <- '&draft_type=junreg&query_type=year_round&from_type_jc=0&from_type_hs=0&from_type_4y=0&from_type_unk=0'

draft_table <- 'table#draft_stats'
draft_id <- 'td:nth-child(9) > a'

seasons <- (1984:2016)
## it's easier to manually set the number of rounds in each draft year
rounds <- c(51, 39, 49, 74, 75, 87, 99, 96, 50, 91, 98, 87, 100, 92, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 40, 40, 40, 40, 40)

drafts <- data.frame()

# scrape functions
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


# loop over season year and round number
system.time(
  
  for (i in 1:length(seasons)){ 
    year <- seasons[i]
    
    for (round in 1:rounds[i]) {
      url_full <- paste(url1, year, url2, round, url3, sep="")
      
      download.file(url_full, destfile='tempfile.html')
      url_local <- 'tempfile.html'
      
      ## pull table
      df_draft <- try(pulltable_nocomment(url_local, draft_table), silent=F)
      if (inherits(df_draft, "try-error")) df_draft <- pulltable_comment(url_local, draft_table)
      
      ## delete voided selections so that dimensions align
      if (year==1989 & round==33) {df_draft <- subset(df_draft, df_draft$RdPck!=4)}
      if (year==1989 & round==58) {df_draft <- subset(df_draft, df_draft$RdPck!=12)}
      if (year==1993 & round==47) {df_draft <- subset(df_draft, df_draft$RdPck!=23)}
      if (year==1993 & round==51) {df_draft <- subset(df_draft, df_draft$RdPck!=18)}
      
      ## pull bb-r ID's
      df_id <- try(pullname_comment(url_local, draft_id), silent=F)
      if (inherits(df_id, "try-error") | length(df_id)==0) df_id <- pullname_nocomment(url_local, draft_id)
      
      df_id <- gsub('/register/player.fcgi?id=', '', df_id, fixed=T)
      df_id <- df_id[grep('shtml', df_id, invert = T)]
      
      df_draft$bbrid <- df_id
      
      ## append to master draft table
      drafts <- rbind(drafts, df_draft)
      
      print(paste0(year, '-', round))
      
    }
  }
)

# differentiate between prospects that did and did NOT sign
drafts_signed <- subset(drafts, drafts$Signed=='Y')

write.csv(drafts, 'draft.csv')
write.csv(drafts_signed, 'draft_signed.csv')


############# END ############################