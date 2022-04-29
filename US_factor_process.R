####################################################################
#
#  Program to build post-transaction cost equity factor portfolios
#
####################################################################

# Load in pre-processed data for the US
# includes prices, div, mktcap, liquidity, factor signals

## load in data 
dir.data <- "C:/Users/mikea/OneDrive/Documents/UniMelb/Optimisation/Group_Assignment/Data"
setwd(dir.data)

region.name=c("US")
filename.load <- paste(region.name,"_latest_data_20220428.csv",sep="")
data_return = read.csv(file=filename.load )

## define factors
## factor_name are columns with the factor score 

factor_rank <- c("Cap","beta_bin_number","volatility_5yr_usd_bin_number","mktcap_dec_bin_number","value_bin_number","illiq_bin_number","roe_bin_number","opprof_bin_number","invmt_bin_number","mom_bin_number")                 
factor_name <- c("Cap","beta","volatility_5yr_usd","mktcap_dec","value","illiq","roe","opprof","invmt","mom") 
factor_numb <- length(factor.rank)

#
Assets_keep = NULL
results.keep = NULL
Index_keep = NULL

### set up variables for a given strategy run

factor_use =      10
direction =       1  
proportion =  0.25
size =            1000  
weighting_scheme = "Cap"

starting_assets = 10^(8) 
cashflow_percent = 0    #.05   # annual cashflow out of account as % of assets

trade_cost_include = TRUE
optimised =       TRUE

# Model setup summary

print(c(paste("Factor ",factor_name[factor_use]," with direction ",direction," and selecting ",proportion," of ",size,
      " names with ",weighting_scheme," weighting",sep=""),
paste("Starting Assetes ",starting_assets," with ",if (!trade_cost_include) "no ", "trade costs and ",
      if (!optimised) "no ","optimisation",sep="")))

## if wish to capture multiple runs then create loops here
if (1==0) {

    for (factor.use in c(10:2)) {

        if (factor.use == 1) proportion_use = 1
        if (factor.use > 0) proportion_use = c(0.25,0.5)
        proportion = proportion.use[1]
        for (starting_assets in 10^(8)) {  
          for (cashflow_percent in 0) {  
            for (size in c(1000)) {  
              for (proportion in proportion_use) {
                for (weighting_scheme in c("Cap")) {
                  for (trade_cost_include in c(TRUE,FALSE)) {

}}}}}}}}


## source usable database from primary database
# after this everything uses data_use for the strategy                  

start_Date = min(data_return$DateYYYYMM)
end_Date = max(data_return$DateYYYYMM)
                    
Factor_choose = factor_name[factor_use]  
data_use = subset(data_return,(DateYYYYMM>=start.Date)&(DateYYYYMM<=end.Date)&(is_investable==1)&(is_liquid==1))
data_use = subset(data_use,select=c( "DateYYYYMM","ws_id","Price","Div",Factor_choose,"mktcap","median_volume_usd"))
names(data_use) = c( "Date","Asset","Price","Div","Factor","mktcap","median_volume_usd")
data_use$mktcap = data_use$mktcap*1000000                         # get everything in $ as mktcap saved in $1m units
data_use$median_volume_usd = data_use$median_volume_usd*1000000   # get everything in $ as volume saved in $1m units
head(data_use)

# reversing factor scores to reverse if need be as "high is better"
data_use$Factor = direction*data_use$Factor

## set up size of strategy, dates
# set up array to store results

Dates_scroll = unique(data_use$Date)
Dates_scroll = sort(Dates_scroll)
Dates_scroll =c(min(Dates_scroll)-1,Dates_scroll)
Dates_scroll_no = length(Dates_scroll)

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

## Set up stating matrix to trade at i=2
# all assets in cash at i=1 (one period before first data date)
i=1
Cash[i] = starting_assets
Assets[i] = Cash[i]
Index[i] = 100

# create one zero holding asset for data_use_last to kick of strategy
data_use_subset = subset(data_use,Date== Dates_scroll[i+1])
data_use_subset$Shares = 0
mktcap_max = max(data_use_subset$mktcap)
data_use_last = subset(data_use_subset,mktcap==mktcap_max,select=c("Asset","Shares","Price"))

for (i in 2:(Dates_scroll_no)) {
# i=i+1
      
    # subset to i date and merge i-1 date holdings
    data_use_subset = subset(data_use,Date== Dates_scroll[i])
    data_use_old = subset(data_use_last,select=c("Asset","Shares","Price"))
    names(data_use_old) = c("Asset","Shares_Start","Price_Start")
    data_use_new = merge(data_use_subset,data_use_old,by="Asset",all=TRUE)
    
    # clean up data 
    #remove securities without a current price
    data_use_new = data_use_new[!is.na(data_use_new$Price),] 
    data_use_new$Shares[is.na(data_use_new$Shares)]=0
    data_use_new$Price_Start[is.na(data_use_new$Price_Start)]=0
    
    # define month end portfolio before rebalance
    data_use_new$Shares_Start =   data_use_new$Shares
    data_use_new$Asset_Amount_Start =   data_use_new$Shares_Start*data_use_new$Price

    # month end assets - current shares, cash and dividends        
    Div_paid[i] = sum(data_use_new$Shares_Start*data_use_new$Div)
    assets = sum(data_use_new$Asset_Amount_Start) + Cash[i-1] + Div_paid[i]

    ## determine eligible investments
    # returns boolean
    investment_eligible_out = investment_eligible(data_use,size,Dates_scroll[i])
    data_use_new = merge(data_use_new,investment_eligible_out, )    

    ## determine target weight and new asset amount
    weighting_list = weighting_input(data_use_new,weighting_scheme = weighting_scheme)
    weights_new = weighting_strategy(weighting_list,proportion=proportion)
    data_use_new$Asset_Amount = weights_new * assets

    ## if wish to optimise portfolio to trade do define and undertake here
    if (optimised) {
      
      ## determine wider allowable - includes pre-trade holding
      # subset data to only allowable investments
      Investment_allowable = (data_use_new$Shares_Start>0)|data_use_new$Eligible
      data_subset_opt = data_use_new[Investment_allowable,]
      
      # subset current and target weights to allowable investments
      weights_target_allowable = weights_new[Investment_allowable]
      weights_target_allowable[data_subset_opt$mktcap==0] = 0
      weights_target_allowable = weights_target_allowable/sum(weights_target_allowable)
      
      weights_current_allowable = data_subset_opt$Asset_Amount_Start/sum(data_subset_opt$Asset_Amount_Start)
      weights_current_allowable[data_subset_opt$mktcap==0] = 0
      weights_current_allowable = weights_current_allowable/sum(weights_current_allowable)
      
      ## Target to be minimised
      # precalculate Expected Return as does not vary with weight
      data_subset_opt$Exp_Return = signal_processing(data_subset_opt)
      ## tc_calc function 

      ## contraints
      security_count = dim(data_subset_opt)[1]
      min_weights = rep(0,security_count)
      max_weight = rep(1,dim(data_subset_opt)[1])
      max_weight[data_subset_opt$mktcap==0] = 0     
      eval_g0 <- function(x,data_subset_opt) {      # budget constraints sum of weights less that 1
        return( sum(x) -1) }
      
      library(nloptr)    
      security_count
      stocks = dim(data_subset_opt) [1] 
      # Solve using NLOPT_LN_COBYLA without gradient information
      
      res1 <- nloptr( x0=weights_current_allowable,
                      eval_f=tc_calc,
                      lb = min_weight,
                      ub = max_weight,
                      eval_g_ineq = eval_g0,
                      opts = list("algorithm"="NLOPT_LN_COBYLA", print_level=0,xtol_rel = 1e-5,maxeval=250,maxtime=50),
                      data_subset_opt = data_subset_opt )
     
      # calculations to show progress of optimiser
      if (FALSE) {
            print(c(Dates_scroll[i],tc_calc(res1$solution,data_subset_opt),tc_calc(weights_current_allowable,data_subset_opt),tc_calc(weights_target_allowable,data_subset_opt)))
            if (tc_calc(weights_current_allowable,data_subset_opt)<tc_calc(res1$solution,data_subset_opt)) 
              {print("Start")
              data_use_new$Asset_Amount[Investment_allowable] = weights_current_allowable * assets
            } else 
              {print("Optimised")
              data_use_new$Asset_Amount[Investment_allowable] = res1$solution * assets
              }}
      
    } # end of optimised
    

    ## Calculate trade rebalanced portfolio
    # process removes multiple shares
    data_use_new$Shares = floor(data_use_new$Asset_Amount / data_use_new$Price)
    data_use_new$Shares[is.nan(data_use_new$Shares)]=0
    data_use_new$Asset_Amount = data_use_new$Shares * data_use_new$Price
    
    # Final shares to trade for i date
    data_use_new$Shares_Trade =  data_use_new$Shares - data_use_new$Shares_Start
    
    # matrix cleanup
    # this should not be used (as zero mktcap excluded as investment) but get rid of divide zero errors  
    data_use_new = subset(data_use_new,select=c("Date", "Asset", "Price","mktcap", "Factor" , "Shares_Start", "Asset_Amount_Start","Shares","Asset_Amount", "Shares_Trade"))
    data_use_new$mktcap[data_use_new$mktcap==0] = min(data_use_new$mktcap[data_use_new$mktcap>0])

    # calculate transaction cost for actual trades    
    output_tc = trasaction_costs(data_use_new,trade_cost_include)
    data_use_new = merge(data_use_new,output_tc,by=c("Asset"))

    # Implicit cost increases (buys) or decreases(sells) Share Price
    data_use_new$Price_Trade = (data_use_new$Price + data_use_new$Trade_cost_IC)
    
    ## calculate post trade portfolio statistics
    # Cash account after dividends, trades, and explicit transaction costs (brokerage)
    Cash[i] = Cash[i-1] + Div_paid[i] - sum(data_use_new$Price_Trade*data_use_new$Shares_Trade) - sum(data_use_new$Trade_cost_EC)
    Assets[i] = Cash[i] + sum(data_use_new$Price*data_use_new$Shares)
    Index[i] = Assets[i]/(Assets[i-1])*Index[i-1]
    Trade_Cost[i] = sum(data_use_new$Price*data_use_new$Shares_Trade)  - sum(data_use_new$Price_Trade*data_use_new$Shares_Trade)    - sum(data_use_new$Trade_cost_EC)      
    Turnover[i] = sum(abs(data_use_new$Price_Trade*data_use_new$Shares_Trade))
    
    # adjustment if portfolio has a cash-flow in or out
    Cashflow[i] = cashflow_percent/12*Assets[i]
    Cash[i] = Cash[i] - Cashflow[i]
    Assets[i] = Assets[i] - Cashflow[i]

    # keep final portfolio for next month
    data_use_last = data_use_new
    print( c(Dates_scroll[i],Assets[i]))
}

# basic result statistics    
results =  c(optimised,
             Factor_choose, direction,
             trade_cost_include,
             size,
             starting_assets,
              proportion,
             (Index[i]/Index[1])^(12/i)-1,
              mean(Turnover[1:i]/Assets[1:i])*12/2)

print(results)
data_save_filename=c("Data_output.csv")

results.keep = rbind(results.keep,results)
Assets_keep <- cbind(Assets_keep,Assets)
setwd(dir.data)
write.csv(results.keep,file=data_save_filename)

# if doing cycle of many strategies close off here.