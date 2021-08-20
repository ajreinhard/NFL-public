library(rvest)

### big sloppy function that seems to be able 
ref_tbl <- function(URL) {
  #URL  <- 'https://www.pro-football-reference.com/boxscores/201909190jax.htm#player_offense::none'
  webpage <- read_html(strsplit(URL,'#')[[1]][1])
  ref_html <- webpage %>%
    html_nodes(xpath = '//comment()') %>%    # select comment nodes
    html_text() %>%    # extract comment text
    paste(collapse = '') %>%    # collapse to a single string
    read_html() %>%    # reparse to HTML
    html_node(paste0('table#',strsplit(strsplit(URL,'#')[[1]][2],'::')[[1]][1]))
  
  ##if there is only a single table that is not commented out
  ##this will remove header rows that appear in the middle of the table
  if (length(ref_html)==0) {
    ref_html <- html_node(webpage,'table')
    data_rows <- ref_html %>% html_nodes('tbody') %>% html_nodes('tr')
    rm_data_rows <- html_nodes(webpage ,xpath = '//table//tr[@class="thead"]')
    data_rows <- data_rows[-match(rm_data_rows,data_rows)]
  } else {
    data_rows <- ref_html %>% html_nodes('tbody') %>% html_nodes('tr')
    # look at scope attr in first column. if it is "row" keep it.
    rw_scope <- sapply(sapply(data_rows, function(x) html_nodes(x,'th') %>% html_attr('scope')), function(y) y[1])
    data_rows <- data_rows[which(rw_scope=='row')]
  }
  if (length(data_rows)==0) {return(NULL)}
  
  
  #ref_html %>% html_nodes('tbody') %>% html_nodes('tr') %>% 
  
  #%>% html_attr('scope')
  
  
  #non-header cells
  main_data <- matrix(html_nodes(data_rows,'td') %>% html_text(),length(data_rows),byrow=T)
  #header cols for each row
  lead_data <- matrix(html_nodes(data_rows,'th') %>% html_text(),length(data_rows),byrow=T)
  #create column headings for data
  data_names <- ref_html %>% html_nodes('thead') %>% html_nodes('tr') %>% .[length(.)] %>% html_nodes('th') %>% html_nodes(xpath= '@data-stat') %>% html_text()
  ref_df <- data.frame(cbind(lead_data,main_data),stringsAsFactors=F)
  names(ref_df) <- data_names
  
  
  
  
  #pull in csv playerID if available
  #cell_ID <- sapply(c(html_nodes(data_rows[1],'th') %>% xml_attrs(),html_nodes(data_rows[1],'td') %>% xml_attrs()), function(x) max(grepl('data-append-csv',names(x))))
  
  ## figure out how often each attribute appears in each column
  attr_mx <- sapply(1:nrow(ref_df), function(x) c(html_nodes(data_rows[x],'th') %>% xml_attrs(),html_nodes(data_rows[x],'td') %>% xml_attrs()))
  attr_av <- sapply(1:nrow(attr_mx), function(x) table(names(unlist(attr_mx[x,])))/ncol(attr_mx))
  cell_ID <- sapply(attr_av, function(x) max(grepl('data-append-csv',names(x))))
  ID_names <- paste0(data_names[which(cell_ID==1)],'ID')
  #id_data <- data.frame(matrix(html_nodes(data_rows,xpath= '//@data-append-csv') %>% html_text(),length(data_rows),byrow=T),stringsAsFactors=F)
  id_data <- data.frame(matrix(html_nodes(data_rows,'th') %>% xml_attr('data-append-csv'),length(data_rows),byrow=T),stringsAsFactors=F)
  names(id_data) <- ID_names
  if (length(id_data)!=0) {ref_df <- cbind(id_data,ref_df)}
  
  ##if there are more than 30% of numeric values then return numeric col type
  options(warn=-1)
  num_test <- sapply(ref_df, function(x) table(factor(is.na(as.numeric(x)),c('TRUE','FALSE'))))
  for (col in which(num_test['FALSE',]/nrow(ref_df) >= .3)) ref_df[,col] <- as.numeric(ref_df[,col])
  #for (col in which(!is.na(as.numeric(head(ref_df,1))))) ref_df[,col] <- as.numeric(ref_df[,col])
  options(warn=0)
  
  return(ref_df)
}
###

### get the list of coaches head from PFR
webpage <- read_html('https://www.pro-football-reference.com/coaches/#coaches::none')
coach_names <- html_nodes(webpage, xpath = '//td/@csk') %>% html_text()
coach_link <- html_nodes(webpage, xpath = '//td//a/@href') %>% html_text()
coach_yrs <- html_nodes(webpage, xpath = '//td[@data-stat="coach_career_span"]') %>% html_text()
coach_first <- sapply(strsplit(coach_yrs, '-'), function(x) x[1])
coach_last <- sapply(strsplit(coach_yrs, '-'), function(x) rev(x)[1])
coach_df <- data.frame(coach_names,coach_link,coach_first,coach_last, stringsAsFactors = F)

### only recent coaches
coach_df <- coach_df[which(coach_df$coach_last >= 1990),]

### loop through every coach's PFR history
coach_list <- lapply(coach_df$coach_link, function(coach_id) {

job_hist <- ref_tbl(paste0('https://www.pro-football-reference.com/',coach_id,'#coaching_history::none'))
job_hist$full <- paste(job_hist$coach_level, job_hist$coach_employer, job_hist$coach_role, sep = ' - ')
job_hist$new <- c(TRUE, sapply(2:nrow(job_hist), function(x) job_hist$full[x - 1] != job_hist$full[x]))

job_hist$isHC <- 0
job_hist$isHC[which(grepl('Head Coach', job_hist$coach_role) & job_hist$coach_level=='NFL')] <- 1
job_hist$isHC[grep('Assistant Head Coach', job_hist$coach_role)] <- 0

yrs_in_job <- diff(c(which(job_hist$new),nrow(job_hist)+1))
job_hist$job_inx <- unlist(sapply(1:length(yrs_in_job), function(x) rep(x, yrs_in_job[x])))
job_hist$HC_inx <- findInterval(job_hist$job_inx,job_hist$job_inx[which(job_hist$new & job_hist$isHC==1)])
job_hist$HC_inx[which(job_hist$isHC==0 & job_hist$HC_inx > 0)] <- NA
job_hist$last_job <- paste0(job_hist$coach_level,' - ',job_hist$coach_role)[match(job_hist$job_inx-1,job_hist$job_inx)]

job_res <- read_html(paste0('https://www.pro-football-reference.com/',coach_id,'#coaching_results::none'))
job_yr <- html_nodes(job_res, xpath = '//tbody/tr/th[@data-stat="year_id"]') %>% html_text()
tm_abbr <- html_nodes(job_res, xpath = '//tbody/tr/td[@data-stat="team"]/a/@href') %>% html_text()
tm_abbr <- sapply(strsplit(tm_abbr,'/'), function(x) x[3])

col_nms <- c('team','league_id','wins', 'losses', 'ties', 'g_playoffs','wins_playoffs','coach_remarks')
hc_rec <- sapply(col_nms, function(x) {
  html_nodes(job_res, xpath = paste0('//tbody/tr/td[@data-stat="',x,'"]')) %>% html_text()
})

if (length(job_yr)==1) {hc_rec <- t(hc_rec)}

res_df <- data.frame(job_yr, tm_abbr, hc_rec, stringsAsFactors = F)
full_df <- merge(job_hist, res_df, by.x = c('year_id'), by.y = c('job_yr'), all.x=T)
full_df$ID <- coach_id
return(full_df)
})

### bind list as data frame and join to original data
full_coach_df <- data.frame(do.call(rbind, coach_list), stringsAsFactors = F)
full_coach_df <- merge(full_coach_df,coach_df, by.x = c('ID'), by.y = c('coach_link'), all.x=T)

### save it
write.csv(full_coach_df, 'coach_hist_1990.csv', row.names = F)
