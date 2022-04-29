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

filename.load <- paste(region.name,"_latest_data_20220428.csv",sep="")
##data_return = read.csv(file=filename.load )

head(data_return)

table(data_return$DateYYYYMM)

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

head(data_return)

start.Date = min(data_return$DateYYYYMM)
end.Date = max(data_return$DateYYYYMM)

Assets_keep = NULL
results.keep = NULL

#### Subset calculations with no and some optimisation

factor.use = 0
direction = -1  
starting_assets = 10^(8) 
cashflow_percent = 0    #.05   # annual cashflow out of account as % of assets
weighting_scheme = "Cap"
size = 1000  
trade_cost_include = TRUE
optimised = TRUE

for (factor.use in c(9:1)) {

if (factor.use == 0) proportion.use = 1
if (factor.use > 0) proportion.use = c((1)/4)
proportion = proportion.use[1]
          
for (starting_assets in 10^(8)) {  
for (cashflow_percent in 0) {  
for (size in c(1000)) {  
for (proportion in proportion.use) {
for (weighting_scheme in c("Cap")) {
for (trade_cost_include in c(TRUE,FALSE)) {

  if (factor.use == 0) Factor_choose = factor.name[1]
  if (factor.use > 0) Factor_choose = factor.name[factor.use]

data_use = subset(data_return,(DateYYYYMM>=start.Date)&(DateYYYYMM<=end.Date)&(is_investable==1)&(is_liquid==1))
data_use = subset(data_use,select=c( "DateYYYYMM","ws_id","Price","Div",Factor_choose,"mktcap","median_volume_usd"))
names(data_use) = c( "Date","Asset","Price","Div","Factor","mktcap","median_volume_usd")
head(data_use)

data_use$Factor = direction*data_use$Factor

factor_bins = unique(data_use$Factor)
factor_bins_count = length(factor_bins)

data_use$mktcap = data_use$mktcap*1000000                         # get everything in $ as mktcap saved in $1m units
data_use$median_volume_usd = data_use$median_volume_usd*1000000   # get everything in $ as volume saved in $1m units
  
table(data_return$DateYYYYMM)  
table(data_use$Date)  
  
Dates_scroll = unique(data_use$Date)
Dates_scroll = sort(Dates_scroll)
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

weights_new_compare = data_use_subset$mktcap/sum(data_use_subset$mktcap)

A = cbind(weights_new,weights_new_compare)
max(abs(apply(A,1,diff)))

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

#Dates_scroll_no=322

for (i in 2:Dates_scroll_no) {
  
    data_use_subset = subset(data_use,Date== Dates_scroll[i])
  
    data_use_old = subset(data_use_last,select=c("Asset","Shares","Price"))
    names(data_use_old) = c("Asset","Shares_Start","Price_Start")
    data_use_new = merge(data_use_subset,data_use_old,by="Asset",all=TRUE)
    
    data_use_new = data_use_new[!is.na(data_use_new$Price),] 

    data_use_new$Shares[is.na(data_use_new$Shares)]=0
    data_use_new$Price_Start[is.na(data_use_new$Price_Start)]=0
    data_use_new$Shares_Start =   data_use_new$Shares
    data_use_new$Asset_Amount_Start =   data_use_new$Shares_Start*data_use_new$Price
    sum(data_use_new$Shares_Start*data_use_new$Price)
    sum(data_use_new$Shares_Start*data_use_new$Price_Start)
    
    Div_paid[i] = sum(data_use_new$Shares_Start*data_use_new$Div)
    
    assets = sum(data_use_new$Asset_Amount_Start) + Cash[i-1] + Div_paid[i]
    head(data_use_new)   

## determine eligible if no data for following month
    
    investment_eligible_out = investment_eligible(data_use,size,Dates_scroll[i])
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
    sum(data_use_new$Asset_Amount_Start)
    sum(data_use_new$Asset_Amount)
    
    
    data_use_new[data_use_new$Eligible,]
    
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
     
      print(c(Dates_scroll[i],tc_calc(res1$solution,data_subset_opt),tc_calc(weights_start_allowable,data_subset_opt),tc_calc(weights_new_allowable,data_subset_opt)))
      data_use_new$Asset_Amount = 0      
      if (tc_calc(weights_start_allowable,data_subset_opt)<tc_calc(res1$solution,data_subset_opt)) 
          {print("Start")
        data_use_new$Asset_Amount[Investment_allowable] = weights_start_allowable * assets
      } else 
          {print("Optimised")
          data_use_new$Asset_Amount[Investment_allowable] = res1$solution * assets
          }
      #data_use_new$Asset_Amount[Investment_allowable] = weights_new_allowable * assets
  }      

        ### Calculate new data for next month
    
    data_use_new$Shares = floor(data_use_new$Asset_Amount / data_use_new$Price)
    data_use_new$Shares[is.nan(data_use_new$Shares)]=0
    
    sum(data_use_new$Shares_Trade*data_use_new$Price) / sum(data_use_new$Asset_Amount)
    
   max((data_use_new$Shares_Trade*data_use_new$Price) / sum(data_use_new$Asset_Amount))
   min((data_use_new$Shares_Trade*data_use_new$Price) / sum(data_use_new$Asset_Amount))
   
    sum(data_use_new$Asset_Amount)
    
    sum( data_use_new$Price)

    data_use_new$Asset_Amount = data_use_new$Shares * data_use_new$Price
    
    data_use_new$Shares_Trade =  data_use_new$Shares - data_use_new$Shares_Start
    
    data_use_new = subset(data_use_new,select=c("Date", "Asset", "Price","mktcap", "Factor" , "Shares_Start", "Asset_Amount_Start","Shares","Asset_Amount", "Shares_Trade"))
    data_use_new$mktcap[data_use_new$mktcap==0] = min(data_use_new$mktcap[data_use_new$mktcap>0])
    
    output_tc = trasaction_costs(data_use_new,trade_cost_include)
    data_use_new = merge(data_use_new,output_tc,by=c("Asset"))

    data_use_new$Price_Trade = (data_use_new$Price + data_use_new$Trade_cost_IC)
    
    p=770
    data_use_new[p,]
    order(data_use_new$Trade_cost_IC / data_use_new$Price)
    max(data_use_new$Trade_cost_IC / data_use_new$Price)
    
    
    Cash[i] = Cash[i-1] + Div_paid[i] - sum(data_use_new$Price_Trade*data_use_new$Shares_Trade) - sum(data_use_new$Trade_cost_EC)
    Assets[i] = Cash[i] + sum(data_use_new$Price*data_use_new$Shares)
    Index[i] = Assets[i]/(Assets[i-1])*Index[i-1]
    Cashflow[i] = cashflow_percent/12*Assets[i]
    Cash[i] = Cash[i] - Cashflow[i]
    Assets[i] = Assets[i] - Cashflow[i]
    sum(data_use_new$Price)
    sum(data_use_new$Shares)

    Trade_Cost[i] = sum(data_use_new$Price*data_use_new$Shares_Trade)  - sum(data_use_new$Price_Trade*data_use_new$Shares_Trade)    - sum(data_use_new$Trade_cost_EC)      
    Turnover[i] = sum(abs(data_use_new$Price_Trade*data_use_new$Shares_Trade))
        
    data_use_last = data_use_new

    #print(c(Dates_scroll[i],Assets_keep[i,1],Assets_keep[i,2],Assets[i], tc_calc(res1$solution,data_subset_opt),tc_calc(weights_start_allowable,data_subset_opt),tc_calc(weights_new_allowable,data_subset_opt)))    
    plot(log(index),type="l")
    lines(log(Index[1:i]),col=2)
    lines(log(index_2),col=3)
}

data_use_new$Asset_Amount/sum(data_use_new$Asset_Amount)

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
             trade_cost_include,
             size,
             starting_assets,
              proportion,
             (Index[i]/Index[1])^(12/i)-1,
              mean(Turnover[1:i]/Assets[1:i])*12/2)

print(results)
results.keep = rbind(results.keep,results)
data_save_filename=c("Data_output_Many.csv")
setwd(dir.data)
write.csv(results.keep,file=data_save_filename)

Assets_keep <- cbind(Assets_keep,Assets)

}
}
}
}
}
}
}