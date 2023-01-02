

#' function to load in Forex data
#' @param rates_list A string or list of strings corresponding to Forex currency abbreviations
#'
#' @return data frame with historic exchange rates from last 180 days
#' @export
#'
#' @importFrom quantmod "getFX"
#'
#' @examples
#' dat1<- load_fx()
#' dat2<-load_fx("EUR/USD")
#' dat3<-load_fx(c("EUR/USD","EUR/GBP","EUR/AUD"))
#'
load_fx<- function(rates_list=c("EUR/USD","EUR/GBP","EUR/AUD")){
  start_date <- Sys.Date()-180
  end_date <- Sys.Date()
  rates_df <- NULL
  for (idx in seq(length(rates_list))){
    rate_index = rates_list[idx]
    quantmod::getFX(rate_index,
          from=start_date,to=end_date)
    temp_df = as.data.frame(get(gsub("/","",rate_index)))
    temp_df$Date = row.names(temp_df)
    temp_df$Index = rate_index
    row.names(temp_df) = NULL
    colnames(temp_df) = c("rate","Date","Index")

    rates_df = rbind(rates_df, temp_df)
  }
  return(rates_df)
}



#' Load in exchange rate for a particular date from last 180 days
#'
#' @param base the currency you want to convert from as Forex currency abbreviation
#' @param foreign the currency you want to convert to as Forex currency abbreviation
#' @param date the date this of which you would like th exchange rate from
#'
#' @return the specified exchange rate
#' @export
#'
#' @examples
#'exchange_rate("EUR","AUD" ,"2022-10-03")
#'exchange_rate("AUD","EUR" )
#'exchange_rate("GBP","AUD")
#'
exchange_rate <- function(base,foreign,date=Sys.Date()-1){
  base<-toupper(base)
  foreign<-toupper(foreign)
  date<-toString(date)
  #loading forex data
  rates_df<-load_fx(paste(base,foreign, sep="/"))
  dayrates<-rates_df[rates_df$Date==date,]
  index_rate<-paste(base,foreign,sep = "/")
  rate<-dayrates[,"rate"]
  return(rate)
}



