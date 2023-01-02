#' Title
#'
#' @param rates_list
#'
#' @return
#' @export
#'
#' @examples

#test
library(quantmod)

#function to load in forex data
load_fx<- function(rates_list=c("EUR/USD","EUR/GBP","EUR/AUD")){
start_date <- Sys.Date()-180
end_date <- Sys.Date()
rates_df <- NULL
for (idx in seq(length(rates_list))){
  rate_index = rates_list[idx]
  getFX(rate_index,
        from=start_date,to=end_date)
  temp_df = as.data.frame(get(gsub("/","",rate_index)))
  temp_df$Date = row.names(temp_df)
  temp_df$Index = rate_index
  row.names(temp_df) = NULL
  colnames(temp_df) = c("rate","Date","Index")

  rates_df = rbind(rates_df, temp_df)
  return(rates_df)
}
}
#saving forex data
rates_df<-load_fx()
#function to get exchange rates
exchange <- function(base,foreign,date=Sys.Date()-1){
  date<-toString(date)
  dayrates<-rates_df[rates_df$Date==date,]
  base<-toupper(base)
  foreign<-toupper(foreign)
  if(base=="EUR"){
    index_rate<-paste(base,foreign,sep = "/")
    rate<-dayrates[dayrates$Index==index_rate,dayrates$rate]
    return(rate[1])
  }
  else if(foreign=="EUR"){
    index_rate<-paste(foreign,base,sep = "/")
    rate<-as.numeric(dayrates[dayrates$Index==index_rate,dayrates$rate])
    return(1/rate[1])
  }
  else{
    index_rate<-paste("EUR",base,sep = "/")
    ratetoeuro<-as.numeric(dayrates[dayrates$Index==index_rate,dayrates$rate])
    ratetoeuro<-1/ratetoeuro[1]
    index_rate<-paste("EUR",foreign,sep = "/")
    euroToForeign<-dayrates[dayrates$Index==index_rate,dayrates$rate]
    euroToForeign<-euroToForeign[1]
    out<-euroToForeign*ratetoeuro
    return(out[1,1])
  }
}
#few sample calls
exchange("EUR","AUD" ,"2022-10-03")
exchange("AUD","EUR" )
exchange("GBP","AUD")
