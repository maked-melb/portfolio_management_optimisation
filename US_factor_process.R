####################################################################
#
#  
#
####################################################################

# Load in pre-processed data for the US
# set Date as yyyydd as easy to cycle through

dir.data <- "C:/Users/mikea/OneDrive/Documents/UniMelb/Optimisation/Group_Assignment/Data"
setwd(dir.data)

region.name=c("US")
return.type.number <- 1

filename.load <- paste(region.name,"_kappa_data_factor_bins_20210831_TU.csv",sep="")

########################################################################
#
# Read in data and setup Dates
#
########################################################################
data=NULL
  
setwd(dir.data)
data <- read.csv(filename.load, header = TRUE, sep = ",")

data$DateYYYYMM <- data$year*100+data$month
subset.date <- unique(data$DateYYYYMM)
subset.date <- sort(subset.date)
subset.date.length <- length(subset.date)

unique(data$nation)

##########################################
#
# If wish to cut universe to particular country or region do it here
#
##########################################

#data <- subset(data,nation=="b'AUSTRALIA'")

tibble(data)
plot(table(data$date),type="l")

########################################################################
#
# Calculate Price and Div for each Asset per Date
# No price data for date if no return data exists
#
########################################################################

data_price = subset(data,select = c("DateYYYYMM","ws_id","price","return"))
data_price_keep = subset(data_price,(DateYYYYMM==(subset.date[1])),select=c("DateYYYYMM","ws_id"))
data_price_keep$Price = 1
data_price_keep$Div = 0

for (data.cycle in 1:(subset.date.length-1))  {

  data_current = subset(data_price_keep,(DateYYYYMM==(subset.date[data.cycle])),select=c("ws_id","Price"))
  data_forward = subset(data_price,(DateYYYYMM==(subset.date[data.cycle+1])),select = c("DateYYYYMM","ws_id","price","return"))
  data_merge = merge(data_current,data_forward,by=c("ws_id"),all.x=TRUE)
  data_merge$Price_next = data_merge$Price*(1+data_merge$price)
  data_merge$Div = data_merge$Price*(data_merge$return - data_merge$price)
  data_merge = na.omit(data_merge)
  
  data_merge = subset(data_merge,select=c("DateYYYYMM","ws_id","Price_next","Div"))
  names(data_merge) = c("DateYYYYMM","ws_id","Price","Div")
  head(data_merge)  
  data_price_keep = rbind(data_price_keep,data_merge)
  print(subset.date[data.cycle])
}



data_return <- merge(data,data_price_keep,by=c("ws_id","DateYYYYMM"))

head(data_return)

factor.rank <- c("beta_bin_number","volatility_5yr_usd_bin_number","mktcap_dec_bin_number","value_bin_number","illiq_bin_number","roe_bin_number","opprof_bin_number","invmt_bin_number","mom_bin_number")                 
factor.name <- c("beta","volatility_5yr_usd","mktcap_dec","value","illiq","roe","opprof","invmt","mom") 
factor.numb <- length(factor.rank)

########################################################################
#
# Calculate Price and Div for each Asset per Date
# No price data for date if no return data exists
#
########################################################################


size.types <- c("all","large","small")
size.types.length <- length(size.types)

head(data)

start.Date = min(data_return$DateYYYYMM)
end.Date = max(data_return$DateYYYYMM)

Assets_keep = NULL
results.keep = NULL

#data = subset(data_use,(Date>=start.Date)&(Date<=end.Date))
##data_use = subset(data_use,(Date>=start.date)&(Date<=end.date),select=c( "Date","Asset","Price","Div","Factor","mktcap"))

for (factor.use in c(9)) {

for (direction in c(1)) {  
  
if (factor.use > 0) Factor_choose = factor.name[factor.use]
if (factor.use == 0) Factor_choose = factor.name[1]

data_use = subset(data_return,(DateYYYYMM>=start.Date)&(DateYYYYMM<=end.Date)&(is_investable==1)&(is_liquid==1))
data_use = subset(data_use,select=c( "DateYYYYMM","ws_id","Price","Div",Factor_choose,"mktcap","median_volume_usd"))
names(data_use) = c( "Date","Asset","Price","Div","Factor","mktcap","median_volume_usd")
head(data_use)

data_use$Factor = direction*data_use$Factor

factor_bins = unique(data_use$Factor)
factor_bins_count = length(factor_bins)

data_use$mktcap = data_use$mktcap*1000000                         # get everything in $ as mktcap saved in $1m units
data_use$median_volume_usd = data_use$median_volume_usd*1000000   # get everything in $ as volume saved in $1m units

#### Plot Aggregate Mkt Cap

mkt_size = aggregate(mktcap ~ Date,data=data_use,sum)

head(mkt_size)
tail(mkt_size)

plot((mkt_size$Date - mkt_size$Date %% 100)/100 + (mkt_size$Date %% 100)/12,log(mkt_size$mktcap,10),type="l",xlab="Year",ylab="Log(10) Aggregate Mkt Cap")


#### Subset calculations with no and some optimisation

trade_impact = 6
cashflow_percent = 0    #.05   # annual cashflow out of account as % of assets
starting_assets = 10^(6)  
cashflow_percent = 0  
size = 500  

if (factor.use == 0) proportion.use = 1
if (factor.use > 0) proportion.use = c((1:4)/4)

weighting_scheme = "Cap"
optimised= TRUE
          
for (starting_assets in 10^(7)) {  
for (cashflow_percent in 0) {  
for (size in c(500)) {  
for (proportion in proportion.use) {
for (weighting_scheme in c("Cap")) {
for (trade_impact in c(6)) {

Dates_scroll = unique(data_use$Date)
Dates_scroll_no = length(Dates_scroll)

data_use_subset = subset(data_use,Date==Dates_scroll[1])
security_no = dim(data_use_subset)[1]

starting_mktcap = sum(data_use_subset$mktcap)

## set up cash asset
Cash = rep(0,Dates_scroll_no)
Div_paid = rep(0,Dates_scroll_no)
Assets = rep(0,Dates_scroll_no)
Index = rep(0,Dates_scroll_no)
Trade_Cost = rep(0,Dates_scroll_no)
tax_liability_keep =  rep(0,Dates_scroll_no)
tax_liability_monthly_keep =  rep(0,Dates_scroll_no)
tax_basis_monthly_keep =  rep(0,Dates_scroll_no)
Turnover = rep(0,Dates_scroll_no)
Cashflow = rep(0,Dates_scroll_no)

### First Month Data

i=1

data_use_subset = subset(data_use,Date== Dates_scroll[i])

# define eligible investments
# first must have a price next month
# second it must be of appropriate size

investment_eligible_out = investment_eligible(data_use,size,Dates_scroll[i])    
data_use_subset = merge(data_use_subset,investment_eligible_out)    

weighting_list = weighting_input(data_use_subset,weighting_scheme = weighting_scheme)
weights_new = weighting_strategy(weighting_list,proportion=proportion)

data_use_subset$Asset_Amount = weights_new * starting_assets
data_use_subset$Shares = floor(data_use_subset$Asset_Amount / data_use_subset$Price)
data_use_subset$Asset_Amount = data_use_subset$Shares * data_use_subset$Price

Cash[i] = starting_assets - sum(data_use_subset$Asset_Amount)
Assets[i] = Cash[i] + sum(data_use_subset$Asset_Amount)
Index[i] = 100

data_use_subset$Shares_Start = 0
data_use_subset$Asset_Amount_Start = 0

data_use_subset$Shares_Trade =  data_use_subset$Shares - data_use_subset$Shares_Start
data_use_subset$Price_Trade = data_use_subset$Price

data_use_subset= subset(data_use_subset,select=c("Date", "Asset", "Price","mktcap", "Factor" , "Shares_Start", "Asset_Amount_Start","Shares","Asset_Amount", "Shares_Trade","Price_Trade"))

data_use_keep = data_use_subset
data_use_last = data_use_subset
data_use_tax = subset(data_use_subset,select=c("Date", "Asset", "Price_Trade","Shares_Trade"))

### create portfolio holdings
i=i+1

for (i in 2:(Dates_scroll_no)) {
  
    data_use_subset = subset(data_use,Date== Dates_scroll[i])
  
    data_use_old = subset(data_use_last,select=c("Asset","Shares","Price"))
    names(data_use_old) = c("Asset","Shares_Start","Price_Start")
    data_use_new = merge(data_use_subset,data_use_old,by="Asset",all=TRUE)

    data_use_new = data_use_new[!is.na(data_use_new$Price),] 

    data_use_new$Shares[is.na(data_use_new$Shares)]=0
    data_use_new$Price_Start[is.na(data_use_new$Price_Start)]=0
    data_use_new$Shares_Start =   data_use_new$Shares
    data_use_new$Asset_Amount_Start =   data_use_new$Shares_Start*data_use_new$Price
    
    Div_paid[i] = sum(data_use_new$Shares_Start*data_use_new$Div)
    
    assets = sum(data_use_new$Asset_Amount_Start) + Cash[i-1] + Div_paid[i]
    head(data_use_new)   

## determine eligible if no data for following month
    
    investment_eligible_out = investment_eligible(data_use_new,size,Dates_scroll[i])
    dim(data_use_new) 
    dim(investment_eligible_out)
    data_use_new = merge(data_use_new,investment_eligible_out, )    
    weighting_list = weighting_input(data_use_new,weighting_scheme = weighting_scheme)
    weights_new = weighting_strategy(weighting_list,proportion=proportion)
    
    sum(weights_new)
    
    dim(data_use_new)
    length(weights_new)
    
    data_use_new$Asset_Amount = weights_new * assets
    sum(data_use_new$Asset_Amount)
    sum(data_use_new$Price)
    
    data_use_new[data_use_new$Price==0,]
    
    data_use_new$Shares = floor(data_use_new$Asset_Amount / data_use_new$Price)
    data_use_new$Shares[is.nan(data_use_new$Shares)]=0
    sum(data_use_new$Shares)
    data_use_new$Asset_Amount = data_use_new$Shares * data_use_new$Price
    data_use_new$Asset_Amount / data_use_new$Price
    
    sum(data_use_new$Shares) 
    sum(data_use_new$Price)
    
    head(data_use_new)
    data_use_new

   ## optimise trading
    
    if (optimised) {
      
      head(data_use_new)
      
      Investment_allowable = (data_use_new$Shares_Start>0)|data_use_new$Eligible
            
      data_subset_opt = data_use_new[Investment_allowable,]
      data_subset_opt$Exp_Return = signal_processing(data_subset_opt)
      data_subset_opt$Exp_Return
      
      head(data_subset_opt)
      
      weights_new_allowable = weights_new[Investment_allowable]
      weights_new_allowable = weights_new_allowable/sum(weights_new_allowable)
      weights_new_allowable[data_subset_opt$mktcap==0] = 0
      weights_new_allowable = weights_new_allowable/sum(weights_new_allowable)
      
      weights_start_allowable = data_subset_opt$Asset_Amount_Start/sum(data_subset_opt$Asset_Amount_Start)
      weights_start_allowable[data_subset_opt$mktcap==0] = 0
      weights_start_allowable = weights_start_allowable/sum(weights_start_allowable)

      max.weight = rep(1,dim(data_subset_opt)[1])
      max.weight[data_subset_opt$mktcap==0] = 0
      
      library(nloptr)    
      
      # constraint function
      eval_g0 <- function(x,data_subset_opt) {
        return( sum(x) -1) }
      
      stocks = dim(data_subset_opt) [1] 
      # Solve using NLOPT_LN_COBYLA without gradient information
      res1 <- nloptr( x0=weights_start_allowable,
                      eval_f=tc_calc,
                      lb = rep(0,stocks),
                      ub = max.weight,
                      eval_g_ineq = eval_g0,
                      opts = list("algorithm"="NLOPT_LN_COBYLA", print_level=0,xtol_rel = 1e-5,maxeval=250,maxtime=50),
                      data_subset_opt = data_subset_opt )
     
      print(c(proportion,Dates_scroll[i],tc_calc(res1$solution,data_subset_opt),tc_calc(weights_start_allowable,data_subset_opt),tc_calc(weights_new_allowable,data_subset_opt)))

      data_use_new$Asset_Amount = 0
      data_use_new$Asset_Amount[Investment_allowable] = res1$solution * assets
      
    }

    ### Calculate new data for next month
    
    data_use_new$Shares = floor(data_use_new$Asset_Amount / data_use_new$Price)
    data_use_new$Shares[is.nan(data_use_new$Shares)]=0
    
    sum(data_use_new$Asset_Amount)
    sum( data_use_new$Price)

    data_use_new$Asset_Amount = data_use_new$Shares * data_use_new$Price
    
    data_use_new$Shares_Trade =  data_use_new$Shares - data_use_new$Shares_Start
    
    data_use_new = subset(data_use_new,select=c("Date", "Asset", "Price","mktcap", "Factor" , "Shares_Start", "Asset_Amount_Start","Shares","Asset_Amount", "Shares_Trade"))
    data_use_new$mktcap[data_use_new$mktcap==0] = min(data_use_new$mktcap[data_use_new$mktcap>0])
    
    output_tc = trasaction_costs(data_use_new)
    data_use_new = merge(data_use_new,output_tc,by=c("Asset"))

    data_use_new$Price_Trade = (data_use_new$Price + data_use_new$Trade_cost_IC)
    
    Cash[i] = Cash[i-1] + Div_paid[i] - sum(data_use_new$Price_Trade*data_use_new$Shares_Trade) - sum(data_use_new$Trade_cost_EC)
    Assets[i] = Cash[i] + sum(data_use_new$Price*data_use_new$Shares)
    Index[i] = Assets[i]/(Assets[i-1])*Index[i-1]
    Cashflow[i] = cashflow_percent/12*Assets[i]
    Cash[i] = Cash[i] - Cashflow[i]
    Assets[i] = Assets[i] - Cashflow[i]
    sum(data_use_new$Price)
    sum(data_use_new$Shares)

        Trade_Cost[i] = sum(data_use_new$Price*data_use_new$Shares_Trade)  - sum(data_use_new$Price_Trade*data_use_new$Shares_Trade)          
    Turnover[i] = sum(abs(data_use_new$Price_Trade*data_use_new$Shares_Trade))
        
    data_use_last = data_use_new

    #print(c(Dates_scroll[i],Assets[i]))    
}


if (FALSE) {
  results =  (c(trade_impact,
         starting_assets,
         weighting_scheme,
         optimised,
         Assets[i],
         proportion,
         weighting_scheme,
         (Assets[i]/Assets[1])^(12/i)-1,
         mean(Turnover[1:i]/Assets[1:i])*12/2,
        mean(Trade_Cost[1:i]/Assets[1:i])))}

results =  c(optimised,
             Factor_choose, direction,
             trade_impact,
             size,
             starting_assets,
              proportion,
             (Index[i]/Index[1])^(12/i)-1,
              mean(Turnover[1:i]/Assets[1:i])*12/2)

print(results)
results.keep = rbind(results.keep,results)
data_save_filename=c("New_Data_output_Mom_Opt.csv")
setwd(dir.data)
write.csv(results.keep,file=data_save_filename)

Assets_keep <- cbind(Assets_keep,Index)

}
}
}
}
}
}
}}
