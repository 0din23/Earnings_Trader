sector_mapper <- function(NAME = NULL, ETF = NULL){
  
  # create mapping
  etfs <- c("XLY", "XLP", "XLE",
            "XLF", "XLV", "XLI", "XLB", "XLK", "XLU",
            "XME", "IYR", "XHB", "XRT", "XLK",
            "XLK")
  namen <- c("Consumer Discretionary","Consumer Staples", "Energy","Financials",
             "Health Care","Industrials","Materials","Technology","Utilities",
             "Metals and Mining","Real Estate","Home Builders","Retail", "Information Technology",
             "Communication Services")
  mapping <- data.frame("etf" = etfs, "name" = namen)
  
  if(!is.null(NAME)){
    res <- mapping %>% filter(namen == NAME) %>% pull(etf)
  } else{
    res <- mapping %>% filter(etf == ETF) %>% pull(etf)
  }
  return(res)
  
}

# from to performance
be4_after_ret <- function(start, end, data){
  
}

quarter_mapper <- function(data){
  
  data$quarter <- data %>%
    pull(earnings_date) %>%
    lapply(., function(x){
      
      monat <- month(x)
      if((monat >=1) & (monat < 4)){
        return(1)
      } else if((monat >= 4) & (monat < 7)){
        return(2)
      } else if((monat >= 7) & (monat < 10)){
        return(3)
      } else{
        return(4)
      }
    }) %>% unlist()
  
  return(data)
}

# Check if tradeable
check_Universe <- function(ticker){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(),"C:/0cap_2/Infrastructure/Earnings/DATASET.db")
  check <- tbl(conn, "META_DATA") %>% data.frame()
  CHECK <- ticker %in% (check %>% pull(symbol))
  RSQLite::dbDisconnect(conn)
  return(CHECK)
}


