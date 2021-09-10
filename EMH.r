#1. Import Packages
library(jsonlite)
library(httr)
library(emh)
library(devtools)
library(zoo)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ade4)
library(openxlsx)
library(readxl)

##2. Data Lisiting
#clean up envion
rm(list=ls())

#2.1make dic object
coin_dir <- c('C:/Users/system999/Documents/crypto/rawdata')

#2.2listing up
coin_dir_file<-list.files(coin_dir)
coin_dir_file2<-order(coin_dir_file)
coin_split<-strsplit(coin_dir_file, split="-")

#2.3counting
coin_dir_file_cnt<-length(coin_dir_file)

##3Data Collect & Analysis
crypto<-read_excel(path = '###/xxx.xlsx',
                   sheet='Weak Summary',
                   col_names = TRUE)



#3.1 Make Data Frame
resu1 <- data.frame(Test_Name=NA,Frequency=NA,Sample_Size=NA,Statistic=NA,Two_sided_p=NA,Z_Score=NA,Non_Random=NA)
resu2 <- data.frame(Test_Name=NA,Frequency=NA,Sample_Size=NA,Statistic=NA,Two_sided_p=NA,Z_Score=NA,Non_Random=NA)

#3.2 Data Collect & Analysis

#3.2.1 Make Time Interval
for (i in 1:59) {
  inter <- interval(ymd(as.Date(16040))+months(i), ymd('2019-11-22'))
  #3.2.2 Data Collect
  for (j in 1:1600) {
    #3.2.3 Decide which cryptocurrencies can be used 
    if (as.numeric(length(unique(fromJSON(paste(coin_dir, "/", coin_dir_file[j], sep=""))$date))<(as.numeric(inter)/(3600*24)))) {
      next
    }
    
    #3.2.4 Data Collect
    assign(coin_dir_file[j], unique(fromJSON(paste(coin_dir, "/", coin_dir_file[j], sep=""))%>% tbl_df))
    #3.2.5 Data Filtering
    assign(coin_dir_file[j], get(coin_dir_file[j]) %>% filter(as.Date(get(coin_dir_file[j])$date) %within% inter))
    #3.2.6 Make Timeseries Data
    zoofile <- zoo(unique(get(coin_dir_file[j])$price), unique(as.Date(get(coin_dir_file[j])$date)))
    #3.2.7 Data Analysis
    result <- tryCatch(is_random(S=zoofile, a=0.95),
                       error = function(e) e
    )
    if(inherits(result, "error")) next
    
    #3.2.8 Make Cryptocurrencis Name
    name<-t(as.data.frame(strsplit(coin_dir_file[j], split="-")))
    #3.2.9 Make Data Frame 
    resu1 <- cbind(result, name, coin_dir_file[j], as.Date(get(coin_dir_file[j])$date)[1])
    #3.2.10 Merge Final Data
    resu2 <- bind_rows(resu2, resu1)
    
    
  }
  
}
as.Date(get(coin_dir_file[1])$'1')[1,]

dd<-as.data.frame(get(coin_dir_file[1]))
names(dd)<-c('date', 'price', 'volume', 'marketcap')
head(dd)


#4Data Clening
#4.1 Check Data
resu3<-crypto

dim(resu2)
head(resu3)
resu3<-resu2[-1,]

#4.2 Summary Data Process

head(resu3)
result5<-resu3
#4.3 Choose 1time period result
result_ALL_t1<-subset(result5, Frequency=='(t-1 to t)')
head(result_ALL_t1)
dim(result5)

#4.4 Choose Test Method & Frequency
result_RUNS<-subset(result5, Test_Name=="Independent Runs" & Frequency=='(t-1 to t)')
result_DW<-subset(result5, Test_Name=="Durbin-Watson" & Frequency=='(t-1 to t)')
result_LB<-subset(result5, Test_Name=="Ljung-Box" & Frequency=='(t-1 to t)')
result_BG<-subset(result5, Test_Name=="Breusch-Godfrey" & Frequency=='(t-1 to t)')
result_BART<-subset(result5, Test_Name=="Bartell Rank" & Frequency=='(t-1 to t)')
result_VRLM<-subset(result5, Test_Name=="Variance-Ratio LoMac" & Frequency=='(t-1 to t)')

#4.5 Make Final Data Frame
#4.5.1 row-bind data
result_ALL_ALL_ALL<-bind_rows(result_RUNS, result_DW, result_LB, result_BG, result_BART, result_VRLM)
#4.5.2 ALL column-bind data
result_ALL_ALL_ALL_C<-bind_cols(result_RUNS, result_DW, result_LB, result_BG, result_BART, result_VRLM)
#4.5.3 Choose column-bind data
result_ALL_ALL_ALL_CC<-result_ALL_ALL_ALL_C[,c(10,11,3,5,16,27,38,49,60,7,18,29,40,51,62)]
head(result_ALL_ALL_ALL_CC)
names(result_ALL_ALL_ALL_CC)<-c('name','date', 'Sample Size', "Runs Test", "Durbin Watson", "LjungBox", 'Breush-Godfrey', 'Bartell Rank', 'Variance-Ratio Lomac', 
                                "RTNR", "DWNR", "LJBNR", 'BGNR', 'BRNR', 'VRLNR')


##4.6 Make Monthly Data
sub_data_table <- data.frame(Test_Name=NA,Frequency=NA,Sample_Size=NA,Statistic=NA)
sub_data_rbind <- data.frame(all=NA,weak=NA,ratio=NA,date=NA)

#4.6.1 Merge Monthly Data
for (i in 1:59) {
  #4.6.2Declaration of Date Data
  start_date <- ymd(as.Date(16040))+months(i)
  #4.6.3 Choose Monthly Data
  sub_data<-subset(result_ALL_ALL_ALL_CC, date==start_date)
  #4.6.4 Choose All-Weak time success data
  sub_data_final<-subset(sub_data, RTNR==FALSE & DWNR==FALSE & LJBNR==FALSE& BRNR==FALSE& VRLNR==FALSE& BGNR==FALSE)
  #4.6.5 calculate ratio
  sub_data_ratio<-dim(sub_data_final)[1]/dim(sub_data)[1]
  #4.6.6 Cbind all data
  sub_data_table<-cbind(dim(sub_data)[1], dim(sub_data_final)[1], sub_data_ratio, as.Date(start_date))
  names(sub_data_table)<-c('all', 'weak', 'ratio', 'date')
  #4.6.7 rbind all data
  sub_data_rbind<-bind_rows(sub_data_rbind, sub_data_table)
  sub_data_rbind$date<-as.Date(sub_data_rbind$date)
  
}


#4.6.2Declaration of Date Data
start_date <- ymd(as.Date(17836))
#4.6.3 Choose Monthly Data
sub_data<-subset(resu3, date==start_date)
#4.6.4 Choose All-Weak time success data
sub_data_final<-subset(sub_data,  DWNR==FALSE & LJBNR==FALSE& BRNR==FALSE& VRLNR==FALSE& BGNR==FALSE)
dim(sub_data_final)
#4.6.5 calculate ratio
sub_data_ratio<-dim(sub_data_final)[1]/dim(sub_data)[1]
#4.6.6 Cbind all data
sub_data_table<-cbind(dim(sub_data)[1], dim(sub_data_final)[1], sub_data_ratio, as.Date(start_date))
names(sub_data_table)<-c('all', 'weak', 'ratio', 'date')
#4.6.7 rbind all data
sub_data_rbind<-bind_rows(sub_data_rbind, sub_data_table)
sub_data_rbind$date<-as.Date(sub_data_rbind$date)

dd1<-data.frame(name=NA, Born=NA)
dd2<-data.frame(name=NA, Born=NA)

for (j in 1:1600) {
  #3.2.3 Decide which cryptocurrencies can be used 
  #3.2.4 Data Collect
  assign(coin_dir_file[j], unique(fromJSON(paste(coin_dir, "/", coin_dir_file[j], sep=""))))

}
  

for (j in 1:1600) {
  #3.2.3 Decide which cryptocurrencies can be used 
  #3.2.4 Data Collect
  dd<-as.data.frame(get(coin_dir_file[j]))
  names(dd)<-c('date', 'price', 'volume', 'marketcap')
  dd1<-tryCatch(cbind(coin_dir_file[j],  as.Date(dd$date)[1], subset(dd, date=='2018-11-01')),
                error = function(e) e
  )
  if(inherits(dd1, "error")) next
  names(dd1)<-c('name',  'Born', 'date', 'price', 'volume', 'marketcap' )
  dd2<-bind_rows(dd2, dd1)
  
}

dd2$Born<-as.Date(as.numeric(dd2$Born))

dd4<-merge(sub_data_final, dd2, by='name')

dd5 <- dd4[c(order(-dd4$marketcap)),]
head(dd5)
dd6 <- dd5[!(dd5$marketcap == '0'),]
dim(dd6)


#event study


#Made Market Capital
dd1<-data.frame(names=NA, date=NA, market=NA)
dd2<-data.frame(names=NA, date=NA, market=NA)
BTC<-as.data.frame(`1-BTC 1`)
names(BTC)<-c('date', 'price', 'volume', 'marketcap')
dd3<-data.frame(date=seq(from=as.Date(15840), by=1, length.out=2384), BTC=BTC$marketcap)


for (j in 1:100) {
  #3.2.3 Decide which cryptocurrencies can be used 
  #3.2.4 Data Collect
  dd<-as.data.frame(get(coin_dir_file[j]))
  names(dd)<-c('date', 'price', 'volume', 'marketcap')
  dd1<-tryCatch(cbind(coin_dir_file[j],  as.Date(dd$date), dd$marketcap),
                error = function(e) e
  )
  if(inherits(dd1, "error")) next
  names(dd1)<-c('name',  'Born', 'date', 'price', 'volume', 'marketcap' )
  dd2<- as.data.frame(cbind(as.Date(dd$date), dd$marketcap))
  names(dd2)<-c('date', 'price')
  dd2$date<-as.Date(dd2$date)
  dd3<-merge(dd3, dd2, by='date', all=TRUE)
  
}



view(dd3)

ddd1<-data.frame(names=NA, date=NA, market=NA)
ddd2<-data.frame(names=NA, date=NA, market=NA)
ddd<-as.data.frame(get(coin_dir_file[2]))

names(ddd)<-c('date', 'price', 'volume', 'marketcap')
ddd1<-tryCatch(cbind(coin_dir_file[2],  as.Date(ddd$date), ddd$marketcap),
              error = function(e) e
)
if(inherits(ddd1, "error")) next
ddd1<- cbind(as.Date(ddd$date), ddd$marketcap)
ddd2<- unique(merge(ddd2, ddd1, key='date', , all=TRUE))
head(ddd2)
view(ddd2)

head(ddd2)
view(ddd2)
head(ddd2)
view(ddd2)

head(ddd1)




##4.7 write excel 
Cr_xl_ALL<-createWorkbook('xxx.xlsx')

addWorksheet(Cr_xl_ALL, "RUNS")
addWorksheet(Cr_xl_ALL, "DurbinWatson")
addWorksheet(Cr_xl_ALL, "LjungBox")
addWorksheet(Cr_xl_ALL, "BreuschGodfrey")
addWorksheet(Cr_xl_ALL, "Bartell Rank")
addWorksheet(Cr_xl_ALL, "Variance-Ratio LoMac")
addWorksheet(Cr_xl_ALL, "Weak Summary")
addWorksheet(Cr_xl_ALL, "ALL Time Weak Summary")
addWorksheet(Cr_xl_ALL, "ALL Weak Summary")
addWorksheet(Cr_xl_ALL, 'list weak summary')

writeDataTable(Cr_xl_ALL, "RUNS", result_RUNS)
writeDataTable(Cr_xl_ALL, "DurbinWatson", result_DW)
writeDataTable(Cr_xl_ALL, "LjungBox", result_LB)
writeDataTable(Cr_xl_ALL, "BreuschGodfrey",result_BG)
writeDataTable(Cr_xl_ALL, "Bartell Rank", result_BART)
writeDataTable(Cr_xl_ALL, "Variance-Ratio LoMac", result_VRLM)
writeDataTable(Cr_xl_ALL, "Weak Summary", result_ALL_ALL_ALL_CC)
writeDataTable(Cr_xl_ALL, "ALL Time Weak Summary",result5)
writeDataTable(Cr_xl_ALL, "ALL Weak Summary", sub_data_rbind)
writeDataTable(Cr_xl_ALL, 'list weak summary', dd6)

saveWorkbook(Cr_xl_ALL, file="xxx.xlsx")
