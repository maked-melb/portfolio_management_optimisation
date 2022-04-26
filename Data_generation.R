require("haven")
library("tidyr")

dir.name <- "C:/Users/mikea/OneDrive/Documents/Research/backtest_data/Most Recent"
setwd(dir.name)

dir.name.save <- "C:/Users/mikea/OneDrive/Documents/UniMelb/Optimisation/Group_Assignment/Data"

#data <- read_sas("allmonth_comp2102_crsp2102.sas7bdat")
#data <- read_sas("worldscope_202106.sas7bdat")
data <- read_sas("datastream_local_202106.sas7bdat")

dim(table(data$WS_ID))

## subset on trading currency

data_subset = subset(data,Trading_CUR=="USD")

dim(table(data_subset$WS_ID))

## subset on required data and delete any NA records

data_subset$Date <- data_subset$year*100+data_subset$month
data_subset$Date_Decimal <- data_subset$year+data_subset$month/12

plot(table(data_subset$Date_Decimal))

data_subset$dy = data_subset$return - data_subset$price
data_subset = subset(data_subset,select=c("Date","WS_ID","price","dy","mktcap"))
data_subset = na.omit(data_subset)

dim(table(data_subset$WS_ID))

## remove indexing rows and have all WS_ID start with X

data_subset = data_subset[,!grepl( "X" , names( data_subset ) )]
data_subset$WS_ID = paste("X",data_subset$WS_ID,sep=".")

## create wide tables for return and mktcap

data_mktcap <- 
  pivot_wider(data_subset,
              id_cols = Date,
              names_from = WS_ID,
              values_from = mktcap,
              values_fill = NA)

data_return <- 
  pivot_wider(data_subset,
              id_cols = Date,
              names_from = WS_ID,
              values_from = price,
              values_fill = NA)

data_dy <- 
  pivot_wider(data_subset,
              id_cols = Date,
              names_from = WS_ID,
              values_from = dy,
              values_fill = NA)

#dates = sort(unique(data_dy$Date))
#for ( i in dates) print(c(i,sum(is.na(subset(data_return,Date==i))),sum(!is.na(subset(data_return,Date==i)))))


##########################
## Calculate Performance series for different time horizons
##########################

calculate_rolling = function(data.returns.use) {

  # data.returns.use <- data_list[[1]]
  
library(dplyr)        ## for mutate
library(data.table)   ## for shift          
library(zoo)          ## for rollmean
library(tidyr)        ## for pivot_longer

## create series of rolling performance

data.predict <- NULL

data.returns.use = data.returns.use[order(data.returns.use$Date),]
data.returns.use = na.omit(data.returns.use)

data.returns.use.tmp = data.returns.use
data.returns.use.tmp[,2] = log(1+data.returns.use.tmp[,2])

data.price <- data.returns.use.tmp %>%
  mutate(across(.cols = contains('X'), .fns = shift,n=0,fill=NA,type=c("lag"))) %>%
  mutate(across(.cols = contains('X'), .fns = cumsum)) %>%
  mutate(across(.cols = contains('X'), .fns = exp)) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Asset",
    names_prefix = "wk",
    values_to = paste("Price"),
    values_drop_na = TRUE)

if (!is.null(data.predict)) data.predict <- merge(data.predict ,data.price ,by=c("Date","Asset"),all=TRUE)
if (is.null(data.predict)) data.predict <- data.price  

for (i in 1) {

data.forward.return <- data.returns.use %>%
  mutate(across(.cols = contains('X'),.fns = shift,n=1,fill=NA,type=c("lead"))) %>%
  mutate(across(.cols = contains('X'),.fns = rollmean,k=i,fill=NA,align=c("left")))%>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Asset",
    names_prefix = "wk",
    values_to = paste("Return.Forward",i,sep="."),
    values_drop_na = TRUE)

if (!is.null(data.predict)) data.predict <- merge(data.predict ,data.forward.return,by=c("Date","Asset"),all=TRUE)
if (is.null(data.predict)) data.predict <- data.forward.return  
}

for (i in 12) {
  
  data.backward.return <- data.returns.use %>%
    mutate(across(.cols = contains('X'), .fns = shift,n=0,fill=NA,type=c("lag"))) %>%
    mutate(across(.cols = contains('X'), .fns = rollsum,k=i,fill=NA,align=c("right"))) %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "Asset",
      names_prefix = "wk",
      values_to = paste("Return.Momentum"),
      values_drop_na = TRUE)

  if (!is.null(data.predict)) data.predict <- merge(data.predict ,data.backward.return,by=c("Date","Asset"),all=TRUE)
  if (is.null(data.predict)) data.predict <- data.backward.return  
}

#data.predict = na.omit(data.predict)

return(data.predict)
}

################## for all codes

list.ID <- names(data_return[,-1])
list.ID.no <- length(list.ID)

data_list <- list()
for (i in 1:list.ID.no) data_list[[i]] <- subset(data_return,select=c("Date",list.ID[i]))

library("parallel")
mc <- length(data_list)
cl <- makeCluster(min(mc,45))
# make this reproducible
clusterSetRNGStream(cl, 123)
out.lst <- parLapply(cl,data_list,calculate_rolling)
stopCluster(cl)

data_predict <- NULL
for (i in 1:list.ID.no) data_predict <- rbind(data_predict,out.lst[[i]])

dates = sort(unique(data_predict$Date))
for ( i in dates) print(c(i,sum(is.na(subset(data_predict,Date==i,select=c("Price")))),sum(!is.na(subset(data_predict,Date==i&!is.na(Return.Forward.1))))))

### merge in dy & mktcap data to performance data

table(data_predict$Date)

data_subset_mktcap <- subset(data_subset,select=c("Date","WS_ID","mktcap","dy"))
names(data_subset_mktcap) = c("Date","Asset","mktcap","dy")
data_predict <- merge(data_predict ,data_subset_mktcap,by=c("Date","Asset"),all=TRUE)

data_predict = na.omit(data_predict)

data_predict$Div = data_predict$Price * data_predict$dy

table(data_predict$Date)

#### calculate and indicate the largest x securities by date

keep_numb = 100
dates = unique(data_predict$Date)
dates_numb  = length(dates)
codes_keep = array(NA,dim=c(dates_numb,keep_numb))

data_predict_largest = NULL

for (i in 1:dates_numb) {
  data_predict_month = subset(data_predict,Date==dates[i])
  mktcap_subset = order(  data_predict_month$mktcap,decreasing=TRUE)[1:keep_numb]
  data_predict_largest = rbind(data_predict_largest,data_predict_month[mktcap_subset,])
  print(dates[i])}

data_predict_largest = na.omit(data_predict_largest)

data_predict_largest_true = subset(data_predict_largest,select=c("Date","Asset"))
data_predict_largest_true$Top100 = TRUE

data_predict_true = merge(data_predict,data_predict_largest_true,by=c("Date","Asset"),all=TRUE)

data_predict_true$Top100[is.na(data_predict_true$Top100)] = FALSE

table(data_predict_true$Top100)

data_predict = data_predict_true

keep_numb = 200
dates = unique(data_predict$Date)
dates_numb  = length(dates)
codes_keep = array(NA,dim=c(dates_numb,keep_numb))

data_predict_largest = NULL

for (i in 1:dates_numb) {
  data_predict_month = subset(data_predict,Date==dates[i])
  mktcap_subset = order(  data_predict_month$mktcap,decreasing=TRUE)[1:keep_numb]
  data_predict_largest = rbind(data_predict_largest,data_predict_month[mktcap_subset,])
  print(dates[i])}

data_predict_largest = na.omit(data_predict_largest)

data_predict_largest_true = subset(data_predict_largest,select=c("Date","Asset"))
data_predict_largest_true$Top200 = TRUE

data_predict_true = merge(data_predict,data_predict_largest_true,by=c("Date","Asset"),all=TRUE)

data_predict_true$Top200[is.na(data_predict_true$Top200)] = FALSE

table(data_predict_true$Top200)

data_predict = data_predict_true

table(data_predict$Top100,data_predict$Top200)

#######################################
## save files out
#######################################

setwd(dir.name.save)
write.csv(data_predict_largest,file="security_USD_largest.csv")
write.csv(data_predict,file="security_USD.csv")

