# Scrape data from web (tide predictions and moon phases)
# for a little tutorial on web scraping see https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/

### TIDES
# Corrected 14.2.2018

library(rvest)
library(plyr)
Sys.setenv(TZ="America/Mazatlan")

# the years have to be scraped seperatly, otherwise the dates are a mess!
# 2006
tides_06 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2006&m=4&d=1")
tides_06_tbls <- html_nodes(tides_06, "table")
tides_06_list <- html_table(tides_06_tbls) # list of the tables
tides_06_df <- ldply(tides_06_list, data.frame) # convert this into a big dataframe

# 2007
tides_07 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2007&m=4&d=1")
tides_07_tbls <- html_nodes(tides_07, "table")
tides_07_list <- html_table(tides_07_tbls) # list of the tables
tides_07_df <- ldply(tides_07_list, data.frame) # convert this into a big dataframe

# 2008
tides_08 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2008&m=4&d=1")
tides_08_tbls <- html_nodes(tides_08, "table")
tides_08_list <- html_table(tides_08_tbls) # list of the tables
tides_08_df <- ldply(tides_08_list, data.frame) # convert this into a big dataframe
tides_08_df$Low.2 <- NULL
tides_08_df$High.3 <- NULL # these two extra coloums were the reason I had to scrape 08 seperatly
colnames(tides_08_df) <- colnames(tides_07_df) # some of the colnames didn't match (needed for rbind)

# 2009
tides_09 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2009&m=4&d=1")
tides_09_tbls <- html_nodes(tides_09, "table")
tides_09_list <- html_table(tides_09_tbls) # list of the tables
tides_09_df <- ldply(tides_09_list, data.frame) # convert this into a big dataframe

#  2010
tides_10 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2010&m=4&d=1")
tides_10_tbls <- html_nodes(tides_10, "table")
tides_10_list <- html_table(tides_10_tbls) # list of the tables
tides_10_df <- ldply(tides_10_list, data.frame) # convert this into a big dataframe

#  2011
tides_11 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2011&m=4&d=1")
tides_11_tbls <- html_nodes(tides_11, "table")
tides_11_list <- html_table(tides_11_tbls) # list of the tables
tides_11_df <- ldply(tides_11_list, data.frame) # convert this into a big dataframe

#  2012
tides_12 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2012&m=4&d=1")
tides_12_tbls <- html_nodes(tides_12, "table")
tides_12_list <- html_table(tides_12_tbls) # list of the tables
tides_12_df <- ldply(tides_12_list, data.frame) # convert this into a big dataframe

#  2013
tides_13 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2013&m=4&d=1")
tides_13_tbls <- html_nodes(tides_13, "table")
tides_13_list <- html_table(tides_13_tbls) # list of the tables
tides_13_df <- ldply(tides_13_list, data.frame) # convert this into a big dataframe

#  2014
tides_14 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2014&m=4&d=1")
tides_14_tbls <- html_nodes(tides_14, "table")
tides_14_list <- html_table(tides_14_tbls) # list of the tables
tides_14_df <- ldply(tides_14_list, data.frame) # convert this into a big dataframe

#  2015
tides_15 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2015&m=4&d=1")
tides_15_tbls <- html_nodes(tides_15, "table")
tides_15_list <- html_table(tides_15_tbls) # list of the tables
tides_15_df <- ldply(tides_15_list, data.frame) # convert this into a big dataframe

#  2016
tides_16 <- read_html("http://tides.mobilegeographics.com/calendar/year/3689.html?y=2016&m=4&d=1")
tides_16_tbls <- html_nodes(tides_16, "table")
tides_16_list <- html_table(tides_16_tbls) # list of the tables
tides_16_df <- ldply(tides_16_list, data.frame) # convert this into a big dataframe


# 2006-2016 merged together

tides_all <- rbind(tides_06_df,tides_07_df,tides_08_df,tides_09_df,tides_10_df,tides_11_df,tides_12_df,tides_13_df,tides_14_df,tides_15_df,tides_16_df)

tides_all$pk <- seq(1:nrow(tides_all))
#which(is.na(tides_all))

tides_all$date <- seq(as.Date("2006/1/1"), as.Date("2016/12/31"), "days")
tides_all$date <- format(tides_all$date, "%Y-%d-%m")
tides_all$date <- as.Date(tides_all$date, "%Y-%d-%m" )

tides_all$year <- format(tides_all$date, "%Y")

tides_all <- subset(tides_all, select = -c(Day,Sunrise,Sunset) )
tides_all <- tides_all[,c(7,9,8,6,1,2,3,4,5)]
colnames(tides_all) <- c("pk","year","date","event","high_1","low_1","high_2","low_2","high_3")
tides_all$high_1 <- substring(tides_all$high_1,15,19)
tides_all$low_1 <- substring(tides_all$low_1,15,19)
tides_all$high_2 <- substring(tides_all$high_2,15,19)
tides_all$low_2 <- substring(tides_all$low_2,15,19)
tides_all$high_3 <- substring(tides_all$high_3,15,19)
tides_all$high_1 <- as.numeric(tides_all$high_1)*100
tides_all$high_2 <- as.numeric(tides_all$high_2)*100
tides_all$high_3 <- as.numeric(tides_all$high_3)*100
tides_all$low_1 <- as.numeric(tides_all$low_1)*100
tides_all$low_2 <- as.numeric(tides_all$low_2)*100

tides_all$event <- gsub('First Quarter', 'fq', tides_all$event)
tides_all$event <- gsub('Full Moon', 'fm', tides_all$event)
tides_all$event <- gsub('Last Quarter', 'lq', tides_all$event)
tides_all$event <- gsub('New Moon', 'nm', tides_all$event)

# I want to have the highest high tides on a given day
ht <- tides_all[,c(5,6,7,8,9)]
ht$m= pmax(ht$high_1,ht$high_2,ht$high_3, na.rm=TRUE) #compare those three and give me the max
# add a coloum with the max tide height to tides_all 
tides_all$max_tide_height <- ht$m

write.csv(tides_all, "/Users/Silvia/Dropbox/SnowyPlover_cycles (1)/tides_all.csv")





## Scrape the lunar data

### MOONPHASES (site last checked on 7.1.2019)
# Get moonphases from timeanddate.com


# set time local
# set it back afterwards to en_US.UTF-8
Sys.setlocale(locale="de_DE.UTF-8") # have to set my locale to the one right for the data (my months from timeanddate.com are in german and I haven't figured out a way to change this)
library(rvest)
library(xml2)
library(magrittr)

# 2006
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2006"
ph_06 <- url %>%
  read_html() %>%
  #html_nodes(xpath='/html/body/div[1]/div[8]/section[2]/div[2]/div/table') %>% # the appearance of the site lightly changed since the last access
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_06 <- ph_06[[1]]
ph_06 <- ph_06[c(1:13),c(2:9)]
colnames(ph_06) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_06$year <- 2006

# 2007
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2007"
ph_07 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_07 <- ph_07[[1]]
ph_07 <- ph_07[c(1:13),c(2:9)]
colnames(ph_07) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_07$year <- 2007

# 2008
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2008"
ph_08 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_08 <- ph_08[[1]]
ph_08 <- ph_08[c(1:13),c(2:9)]
colnames(ph_08) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_08$year <- 2008


# 2009
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2009"
ph_09 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_09 <- ph_09[[1]]
ph_09 <- ph_09[c(1:13),c(2:9)]
colnames(ph_09) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_09$year <- 2009



# 2010
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2010"
ph_10 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_10 <- ph_10[[1]]
ph_10 <- ph_10[c(1:13),c(2:9)]
colnames(ph_10) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_10$year <- 2010


# 2011
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2011"
ph_11 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_11 <- ph_11[[1]]
ph_11 <- ph_11[c(1:13),c(2:9)]
colnames(ph_11) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_11$year <- 2011


# 2012
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2012"
ph_12 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_12 <- ph_12[[1]]
ph_12 <- ph_12[c(1:13),c(2:9)]
colnames(ph_12) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_12$year <- 2012



# 2013
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2013"
ph_13 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_13 <- ph_13[[1]]
ph_13 <- ph_13[c(1:13),c(2:9)]
colnames(ph_13) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_13$year <- 2013


# 2014
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2014"
ph_14 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_14 <- ph_14[[1]]
ph_14 <- ph_14[c(1:13),c(2:9)]
colnames(ph_14) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_14$year <- 2014


# 2015
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2015"
ph_15 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_15 <- ph_15[[1]]
ph_15 <- ph_15[c(1:13),c(2:9)]
colnames(ph_15) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_15$year <- 2015


# 2016
url <- "https://www.timeanddate.com/moon/phases/mexico/mazatlan?year=2016"
ph_16 <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/div[1]/div[7]/section[2]/div[2]/div/table') %>%
  html_table()
ph_16 <- ph_16[[1]]
ph_16 <- ph_16[c(1:13),c(2:9)]
colnames(ph_16) <- c("nm_d","nm_t","fq_d","fq_t","fm_d","fm_t","lq_d","lq_t")
ph_16$year <- 2016


# bind all years together
mp_all <- rbind (ph_06, ph_07, ph_08,ph_09,ph_10,ph_11,ph_12,ph_13,ph_14,ph_15,ph_16)
# reformate, create datetime object
# new moon
mp_all$nm_d <- ifelse(mp_all$nm_d != "" ,paste(mp_all$nm_d,mp_all$year ),NA) # merge year and date if there is a date, otherwise write NA
mp_all$nm_dt <- strptime(as.character(mp_all$nm_d), "%d. %B %Y") # here is, why it's important to change the local to german
mp_all$nm_dt <- format(mp_all$nm_dt, "%Y-%m-%d")
mp_all$nm_dt <- paste(mp_all$nm_dt, mp_all$nm_t)
# first quater
mp_all$fq_d <- ifelse(mp_all$fq_d != "" ,paste(mp_all$fq_d,mp_all$year ),NA)
mp_all$fq_dt <- strptime(as.character(mp_all$fq_d), "%d. %B %Y") 
mp_all$fq_dt <- format(mp_all$fq_dt, "%Y-%m-%d")
mp_all$fq_dt <- paste(mp_all$fq_dt, mp_all$fq_t)
# fullmoon
mp_all$fm_d <- ifelse(mp_all$fm_d != "" ,paste(mp_all$fm_d,mp_all$year ),NA)
mp_all$fm_dt <- strptime(as.character(mp_all$fm_d), "%d. %B %Y") 
mp_all$fm_dt <- format(mp_all$fm_dt, "%Y-%m-%d")
mp_all$fm_dt <- paste(mp_all$fm_dt, mp_all$fm_t)
# last quater
mp_all$lq_d <- ifelse(mp_all$lq_d != "" ,paste(mp_all$lq_d,mp_all$year ),NA)
mp_all$lq_dt <- strptime(as.character(mp_all$lq_d), "%d. %B %Y") 
mp_all$lq_dt <- format(mp_all$lq_dt, "%Y-%m-%d")
mp_all$lq_dt <- paste(mp_all$lq_dt, mp_all$lq_t)
# reformate the dataframe ( I want the corresponding moon event to each datetime)
# new moon
mp_all$event <- 1 # dummy
mp_nm <- data.frame(mp_all$nm_dt,mp_all$event)
colnames(mp_nm) <- c("datetime","event")
mp_nm$event <- "nm"
# first quater
mp_fq <- data.frame(mp_all$fq_dt,mp_all$event)
colnames(mp_fq) <- c("datetime","event")
mp_fq$event <- "fq"
# full moon
mp_fm <- data.frame(mp_all$fm_dt,mp_all$event)
colnames(mp_fm) <- c("datetime","event")
mp_fm$event <- "fm"
# last quater
mp_lq <- data.frame(mp_all$lq_dt,mp_all$event)
colnames(mp_lq) <- c("datetime","event")
mp_lq$event <- "lq"

# bind them together
moonsequ <- rbind (mp_nm,mp_fq,mp_fm,mp_lq)
# reorder according to date
moonsequ <- moonsequ[order(as.Date(moonsequ$datetime, format="%Y-%m-%d")),]
moonsequ$pk <- 1:nrow(moonsequ)
# get rid of NAs
moonsequ <- moonsequ[(moonsequ$pk < 544),]
moonsequ$year <- format(as.Date(moonsequ$datetime, format="%Y-%m-%d"),"%Y")
moonsequ <- moonsequ[,c(4,1,2)]
colnames(moonsequ)[2] <- "datetime_"
rownames(moonsequ) <- c()
write.csv2(moonsequ, "/Users/Silvia/SnowyPlover/moonphases_test.csv")





### ILLUMINATION (not used during final analysis)
# 2006-2016

moon_06 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2006"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_06_list <- do.call(rbind, moon_06) 
moon_06_df <- do.call(rbind, moon_06_list)
moon_06_df <- moon_06_df[,c(1,8,11)]
colnames(moon_06_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_06_df=moon_06_df[moon_06_df$date %in% dig,] 
moon_06_df$date <- seq(as.Date("2006/1/1"), as.Date("2006/12/31"), "days")


moon_07 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2007"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_07_list <- do.call(rbind, moon_07) 
moon_07_df <- do.call(rbind, moon_07_list)
moon_07_df <- moon_07_df[,c(1,8,11)]
colnames(moon_07_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_07_df=moon_07_df[moon_07_df$date %in% dig,] # works, nice!
moon_07_df$date <- seq(as.Date("2007/1/1"), as.Date("2007/12/31"), "days")


moon_08 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2008"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_08_list <- do.call(rbind, moon_08) 
moon_08_df <- do.call(rbind, moon_08_list)
moon_08_df <- moon_08_df[,c(1,8,11)]
colnames(moon_08_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_08_df=moon_08_df[moon_08_df$date %in% dig,] # works, nice!
moon_08_df$date <- seq(as.Date("2008/1/1"), as.Date("2008/12/31"), "days")


moon_09 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2009"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_09_list <- do.call(rbind, moon_09) 
moon_09_df <- do.call(rbind, moon_09_list)
moon_09_df <- moon_09_df[,c(1,8,11)]
colnames(moon_09_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_09_df=moon_09_df[moon_09_df$date %in% dig,] # works, nice!
moon_09_df$date <- seq(as.Date("2009/1/1"), as.Date("2009/12/31"), "days")
View(moon_09_df)


moon_10 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2010"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_10_list <- do.call(rbind, moon_10) 
moon_10_df <- do.call(rbind, moon_10_list)
moon_10_df <- moon_10_df[,c(1,8,11)]
colnames(moon_10_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_10_df=moon_10_df[moon_10_df$date %in% dig,] # works, nice!
moon_10_df$date <- seq(as.Date("2010/1/1"), as.Date("2010/12/31"), "days")
View(moon_10_df)


moon_11 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2011"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_11_list <- do.call(rbind, moon_11) 
moon_11_df <- do.call(rbind, moon_11_list)
moon_11_df <- moon_11_df[,c(1,8,11)]
colnames(moon_11_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_11_df=moon_11_df[moon_11_df$date %in% dig,] # works, nice!
moon_11_df$date <- seq(as.Date("2011/1/1"), as.Date("2011/12/31"), "days")
View(moon_11_df)


moon_12 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2012"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_12_list <- do.call(rbind, moon_12) 
moon_12_df <- do.call(rbind, moon_12_list)
moon_12_df <- moon_12_df[,c(1,8,11)]
colnames(moon_12_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_12_df=moon_12_df[moon_12_df$date %in% dig,] # works, nice!
moon_12_df$date <- seq(as.Date("2012/1/1"), as.Date("2012/12/31"), "days")
View(moon_12_df)



moon_13 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2013"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_13_list <- do.call(rbind, moon_13) 
moon_13_df <- do.call(rbind, moon_13_list)
moon_13_df <- moon_13_df[,c(1,8,11)]
colnames(moon_13_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_13_df=moon_13_df[moon_13_df$date %in% dig,] # works, nice!
moon_13_df$date <- seq(as.Date("2013/1/1"), as.Date("2013/12/31"), "days")
View(moon_13_df)


moon_14 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2014"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_14_list <- do.call(rbind, moon_14) 
moon_14_df <- do.call(rbind, moon_14_list)
moon_14_df <- moon_14_df[,c(1,8,11)]
colnames(moon_14_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_14_df=moon_14_df[moon_14_df$date %in% dig,] # works, nice!
moon_14_df$date <- seq(as.Date("2014/1/1"), as.Date("2014/12/31"), "days")
View(moon_14_df)


moon_15 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2015"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_15_list <- do.call(rbind, moon_15) 
moon_15_df <- do.call(rbind, moon_15_list)
moon_15_df <- moon_15_df[,c(1,8,11)]
colnames(moon_15_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_15_df=moon_15_df[moon_15_df$date %in% dig,] # works, nice!
moon_15_df$date <- seq(as.Date("2015/1/1"), as.Date("2015/12/31"), "days")
View(moon_15_df)


moon_16 <- lapply(paste0("https://www.timeanddate.com/moon/mexico/mazatlan?month=",1:12,"&year=2016"),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("table") %>% 
                      html_table()
                  })  
moon_16_list <- do.call(rbind, moon_16) 
moon_16_df <- do.call(rbind, moon_16_list)
moon_16_df <- moon_16_df[,c(1,8,11)]
colnames(moon_16_df) <- c("date","time","illumination")
dig <- c(1:31)
moon_16_df=moon_16_df[moon_16_df$date %in% dig,] 
moon_16_df$date <- seq(as.Date("2016/1/1"), as.Date("2016/12/31"), "days")
View(moon_16_df)

#2006-2016 merged

moon_all <- rbind (moon_06_df, moon_07_df, moon_08_df , moon_09_df, moon_10_df, moon_11_df, moon_12_df, moon_13_df,  moon_14_df, moon_15_df, moon_16_df)

moon_all$time <- gsub("Moon does not pass the meridian on this day.","NA",moon_all$time )
moon_all$illumination <- gsub("Moon does not pass the meridian on this day.","NA",moon_all$illumination )

## Interpolation for illumination at 12 pm
Sys.setenv(TZ="UTC")
moon_all$datetime<- as.POSIXct(paste(moon_all$date, moon_all$time), format="%Y-%m-%d %H:%M")
moon_all$illumination <- gsub("%","",moon_all$illumination)
moon_all$illumination <- gsub(",",".",moon_all$illumination)
moon_all$illumination <- as.numeric(moon_all$illumination)/100

f <- approxfun(moon_all$datetime,moon_all$illumination)

start <- as.POSIXct("2006-1-01 12:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")
end   <- as.POSIXct("2016-12-31 12:00:00", "%Y-%m-%d %H:%M:%S", tz="UTC")

x <- seq(start, end, "days")
moon_all$noon <- x
moon_all$interpolated <- f(x)
moon_all$year <- format(moon_all$date, "%Y")
moon_all <- moon_all[,c(7,4,3,5,6)]
colnames(moon_all) <- c("year","meridian_passing","illumination_mp","noon","illumination_noon")








