## functions to determine weightings and trade costs

################
### weighting input
################

weighting_input = function(data_use_subset,weighting_scheme=c("Cap"),signal.name=c("Factor")) {

  signal = as.matrix(subset(data_use_subset,select=signal.name))
  if (weighting_scheme=="Cap") weight_input = as.matrix(subset(data_use_subset,select=c("mktcap")))
  if (weighting_scheme=="Equal") weight_input = rep(1,dim(data_use_subset)[1])
  
  eligible = data_use_subset$Eligible

  data_input = list(signal,weight_input,eligible)
  
}

################
### robust percentile ranking
################

p.rank = function(f_data) {
  #f_data =  data_factor_date$wealth_excess_return.1mf
  ## remove NA data so not part of sort
  rm_na_data = is.na(f_data)
  f_data_clean = f_data[!rm_na_data]
  # return NA as NA and fill not NA with percent ranks
  p_data = order(f_data_clean,decreasing=TRUE) / length(f_data_clean)
  p_rank = rep(NA,length(f_data))
  p_rank[!rm_na_data] = order(f_data_clean,decreasing=TRUE) / length(f_data_clean)
  return(p_rank)
}


################
### signal processing
################

signal_processing = function(data_use_subset,signal.name=c("Factor"),return_spread=0.01) {
  
  signal = as.matrix(subset(data_use_subset,select=signal.name))
  signal_order = p.rank(signal)
  signal_return = (signal_order -0.5) * return_spread
  signal_return[is.na(signal_return)] = 0
  
  return(signal_return)
}


################
### weighting strategy
################

weighting_strategy = function(data_input,proportion=0.25) {

    
  signal=data_input[[1]]
  weight_input=data_input[[2]]
  eligible = data_input[[3]]
  
  signal_use = signal[eligible]
  weight_input_use = weight_input[eligible]
  
  asset_length = length(weight_input)
  asset_length_eligible = length(weight_input_use)
  
  order_signal = order(signal_use,decreasing = TRUE)
  order_signal = order_signal[1:(floor(asset_length_eligible*proportion))]
  signal_use[order_signal]
  weight_use = rep(0,asset_length_eligible)
  weight_use[order_signal] = weight_input_use[order_signal]/sum(weight_input_use[order_signal])
  
  weight = rep(0,asset_length)
  weight[eligible] = weight_use
  
  #data_temp = cbind(signal,weight,eligible)
  #data_temp=subset(data_temp,(eligible==TRUE))
  #data_temp = data.frame(data_temp)
  #head(data_temp)
  #data_temp$Factor = as.numeric(data_temp$Factor)
  
  #aggregate(weight~Factor,data=data_temp,mean)  

  return(weight)
}        

################
### set eligible investments for forward month
################

investment_eligible = function(data_use,size,date_select) {
    #date_select = 198001
    
    data_use_subset_current = subset(data_use,Date== date_select,select=c("Asset","Price","mktcap"))
    asset_count = dim(data_use_subset_current)[1]
    date_scroll = c(sort(unique(data_use$Date)),999999)
    date_select_next = min(date_scroll[date_scroll > date_select] )
    data_use_subset_fwd = subset(data_use,Date== date_select_next,select=c("Asset","Price"))
    
    names(data_use_subset_fwd) = c("Asset","Price_Fwd")
    data_use_new_fwd = merge(data_use_subset_current, data_use_subset_fwd,by="Asset",all.x=TRUE)
    if (date_select_next==999999) data_use_new_fwd$Price_Fwd = 1
    data_use_new_fwd$Eligible = TRUE
    data_use_new_fwd$Eligible[is.na(data_use_new_fwd$Price_Fwd)] = FALSE
    data_use_new_fwd$Eligible[(data_use_new_fwd$Price_Fwd)==0] = FALSE
    data_use_new_fwd$Eligible[(data_use_new_fwd$Price)==0] = FALSE
    
    dim(data_use_new_fwd)
    
    data_use_new_fwd$mktcap[!data_use_new_fwd$Eligible] = 0
    data_use_new_fwd$Eligible = FALSE
    size_all = length(order(data_use_new_fwd$mktcap,decreasing=TRUE))
    size_use = min(size,size_all)
    data_use_new_fwd$Eligible[order(data_use_new_fwd$mktcap,decreasing=TRUE)[1:size_use]] = TRUE

    investment_eligible = subset(data_use_new_fwd,select=c("Asset","Eligible"))
    
    dim(investment_eligible)
    dim(data_use_subset_current)
    
    return(investment_eligible)
}

################
### Calculate transactions costs
###
### explicit and implicit
################

trasaction_costs = function(data_sub) {

  #data_sub=data_use_new
  
  Output = subset(data_sub,select=c("Asset"))
  Output$Trade_Cost_IC_rel = (data_sub$Shares_Trade*data_sub$Price^2)/data_sub$mktcap*trade_impact
  Output$Trade_Cost_IC_abs = sign(data_sub$Shares_Trade)*0.0028/2*min(data_sub$mktcap)/data_sub$mktcap*data_sub$Price
  Output$Trade_cost_IC = sign(Output$Trade_Cost_IC_abs)*apply(abs(subset(Output,select=c(Trade_Cost_IC_abs,Trade_Cost_IC_rel))),1,max)

  Output$Trade_Cost_EC_rel = 0.0001*abs(data_sub$Shares_Trade*data_sub$Price)
  Output$Trade_Cost_EC_abs = 5
  Output$Trade_Cost_EC_abs[data_sub$Shares_Trade==0] = 0
  Output$Trade_cost_EC = apply(abs(subset(Output,select=c(Trade_Cost_EC_abs,Trade_Cost_EC_rel))),1,max)    
  return(Output)
}

### trade cost (less expected return)

tc_calc = function(x,data_subset_opt) {
  
  weight_test = x
  #weight_test = weights_start_allowable
  
  data_subset_opt$Asset_Amount = weight_test*sum(data_subset_opt$Asset_Amount)
  
  data_subset_opt$Shares = floor(data_subset_opt$Asset_Amount / data_subset_opt$Price)
  data_subset_opt$Shares[is.nan(data_subset_opt$Shares)]=0
  data_subset_opt$Asset_Amount = data_subset_opt$Shares * data_subset_opt$Price
  
  data_subset_opt$Shares_Trade =  data_subset_opt$Shares - data_subset_opt$Shares_Start
  
  data_subset_opt = subset(data_subset_opt,select=c("Date", "Asset", "Price","mktcap", "Factor" , "Shares_Start", "Asset_Amount_Start","Shares","Asset_Amount", "Shares_Trade","Exp_Return"))
  data_subset_opt$mktcap[data_subset_opt$mktcap==0] = min(data_subset_opt$mktcap[data_subset_opt$mktcap>0])
  
  output_tc = trasaction_costs(data_subset_opt)
  data_subset_opt = merge(data_subset_opt,output_tc,by=c("Asset"))
  
  transaction_cost = sum(data_subset_opt$Trade_cost_EC) + sum(data_subset_opt$Trade_cost_IC*data_subset_opt$Shares_Trade)
  exp_return = sum(data_subset_opt$Asset_Amount*data_subset_opt$Exp_Return)
  
  return(-exp_return + transaction_cost)
}
