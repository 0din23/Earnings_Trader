# Dependencies, Set-Up usw. ----------------------------------------------------
source("R/dependencies.R")

# prep -------------------------------------------------------------------------
# connect to DB
conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                           paste0(getwd(), "/DATA/", "Earnings_Data.db"))

index <- tq_index("SP500") %>%
  select(symbol, company, sector)

not <- index %>%
  pull(symbol) %>%
  lapply(., function(x){
    catch <- tryCatch(expr = {
      a <- get_historical_earnings(x)
      print_timed(x, " is on Board.")
      return(FALSE)
    }, error = function(cond){
      print_timed(x, " is Shit.")
      return(TRUE)
    })
    if(catch){return(NA)} else{return(x)}
    
  }) %>%
  unlist() %>%
  is.na() %>%
  !.

index <-  index[not,]

# Repeatable filter if code fails ----------------------------------------------
loaded <- tbl(conn, "EARNINGS_DATA") %>%
  data.frame() %>%
  pull(symbol) %>%
  unique()

loaded <- append(loaded, "CEG")
index <- index %>%
  filter(!(symbol %in% loaded))

# Cycla through stocks and create data -----------------------------------------
for(k in 1:nrow(index)){
  
  TICKER <- index[k,"symbol"] %>% as.character()
  sec <- index[k,"sector"] %>% as.character()
  temp <- load_ticker_data_2(ticker = TICKER)
  # temp <- add_features(data=temp, SECTOR=SECTOR[[sec]], MARKET=SNP, VIX=VIX)
  temp[, "Sector"] <- sec
  
  if(TICKER == "AAPL"){
    RSQLite::dbWriteTable(conn, "EARNINGS_DATA", temp %>%
                            mutate(earnings_date = as.character(earnings_date)),
                          append =FALSE, overwrite=TRUE)
  } else{
    RSQLite::dbWriteTable(conn, "EARNINGS_DATA", temp %>%
                            mutate(earnings_date = as.character(earnings_date)),
                          append = TRUE, overwrite=FALSE)
  }
  print_timed(TICKER, " is done.")
}

# integrity check --------------------------------------------------------------
data <- tbl(conn, "EARNINGS_DATA") %>%
  data.frame()
N <- nrow(data)
data <- data[, 1:27] %>% distinct()

# Get earnings day Price -------------------------------------------------------
stock_data <- Epsilon_Data_Loader(ticker = data %>% pull(symbol) %>% unique(),
                                  db_type = "stock",
                                  StockDB = CONFIG("stocks")) %>%
  distinct()
stock_dates <- stock_data %>% pull(date) %>% unique() %>% as.character()
data[,"pre_earnings_price"] <- c(1:nrow(data)) %>%
  lapply(., function(x){
    
    # get parameter
    time <- data[x, "earnings_time"]
    e_date <- data[x, "earnings_date"] %>% as.Date()
    
    if(time == "AMC"){
      e_date <- e_date
    } else{
      e_date <- e_date -1
    }
    
    while(!(e_date %>% as.character() %in% stock_dates)){
      e_date <- e_date - 1
    }
    return(e_date %>% as.character())
  }) %>%
  unlist()

data <- data %>% left_join(stock_data %>%
                             select(date, symbol, close),
                           by = c("pre_earnings_price"= "date", "symbol"="symbol"))
data[,"pre_earnings_price"] <- data$close
data <- data %>% select(-close)
# Get Vix Data -----------------------------------------------------------------
VIX <- tq_get("^VIX", from = "1900-01-01")%>%
  mutate(SMA20 = SMA(adjusted, 20),
         rollRet_20B = rollReturns(adjusted, 20, F),
         date = date %>% as.character())
vix_dates <- VIX %>% pull(date) %>% unique() %>% as.character()

## Get Change in VIX before earnings
data[, "vix_perf_before"] <- c(1:nrow(data)) %>%
  lapply(., function(x){
    time <- data[x, "earnings_time"]
    e_date <- data[x, "earnings_date"] %>% as.Date()
    if(time == "AMC"){
      end <- e_date
    } else{
      end <- e_date -1
    }
    while(!(end %>% as.character() %in% vix_dates)){
      end <- end - 1
    }
    return(end %>% as.character())
  }) %>%
  unlist()

## VIX Regime before
data[, "vix_regime"] <- c(1:nrow(data)) %>%
  lapply(., function(x){
    time <- data[x, "earnings_time"]
    e_date <- data[x, "earnings_date"] %>% as.Date()
    if(time == "AMC"){
      end <- e_date
    } else{
      end <- e_date -1
    }
    while(!(end %>% as.character() %in% vix_dates)){
      end <- end - 1
    }
    return(end %>% as.character())
  }) %>%
  unlist()

# join
data <- data %>% left_join(VIX %>%
                             select(date, rollRet_20B),
                           by = c("vix_perf_before"= "date"))
data <- data %>% left_join(VIX %>%
                             select(date, SMA20),
                           by = c("vix_regime"= "date"))
data[,"vix_perf_before"] <- data$rollRet_20B
data[,"vix_regime"] <- data$SMA20

# Get Sector Data --------------------------------------------------------------
SECTOR <- index %>%
  pull(sector) %>%
  unique() %>%
  lapply(., function(x){
    ticker <- sector_mapper(NAME=x)
    tq_get(ticker, from = "1900-01-01") %>%
      mutate(sector = x,
             date= date %>% as.character(),
             Ret_20B = rollReturns(adjusted, 20, F))
  }) %>%
  rbindlist()

SECTOR %>% select(date, sector) %>% distinct() %>% nrow()

sec_dates <- SECTOR %>% pull(date) %>% unique() %>% as.character()

data[, "sector_perf_before"] <- c(1:nrow(data)) %>%
  lapply(., function(x){
    time <- data[x, "earnings_time"]
    e_date <- data[x, "earnings_date"] %>% as.Date()
    if(time == "AMC"){
      end <- e_date
    } else{
      end <- e_date-1
    }
    
    # check for holidays and weekends
    while(!(end %>% as.character() %in% sec_dates)){
      end <- end - 1
    }
    return(end %>% as.character())
    
  }) %>%
  unlist()

data <- data %>% left_join(., SECTOR %>%
                             select(date, sector, Ret_20B),
                           by = c("sector_perf_before"= "date","Sector"="sector"))
data[,"sector_perf_before"] <- data$Ret_20B

# Get Market Indicators --------------------------------------------------------
MARKET <- tq_get("^GSPC", from = "1900-01-01") %>%
  mutate(
    date= date %>% as.character(),
    Ret_20B_m = rollReturns(adjusted, 20, F))

data[, "market_perf_before"] <- c(1:nrow(data)) %>%
  lapply(., function(x){
    time <- data[x, "earnings_time"]
    e_date <- data[x, "earnings_date"] %>% as.Date()
    if(time == "AMC"){
      end <- e_date
    } else{
      end <- e_date-1
    }
    
    # check for holidays and weekends
    # check for holidays and weekends
    while(!(end %>% as.character() %in% sec_dates)){
      end <- end - 1
    }
    return(end %>% as.character())
  }) %>%
  unlist()
data <- data %>% left_join(MARKET %>%
                             select(date, Ret_20B_m),
                           by = c("market_perf_before"= "date"))
data[,"market_perf_before"] <- data$Ret_20B_m

# Aufräumen --------------------------------------------------------------------
data %>% colnames()
data <- data %>%
  select(
    -c("close", "SMA20", "rollRet_20B", "Ret_20B", "Ret_20B_m")
  )

# Calculate Ratios and differences ---------------------------------------------
data[, "Sharpe_before"] <- data$perf_before / data$vol_before
data[,"Stock_vs_Sector"] <- data$perf_before - data$sector_perf_before
data[,"Stock_vs_Market"] <- data$perf_before - data$market_perf_before
data[,"Sector_vs_Market"] <- data$sector_perf_before - data$market_perf_before

# Calculate Earnings Day PE Indicators -----------------------------------------
## PE Ratios
data[,"PE_avg_before_earnings"] <- data$pre_earnings_avg_price / data$eps_ttm_pre
data[,"PE_before_earnings"] <- data$pre_earnings_price / data$eps_ttm_pre
data[,"PE_after_oldPrice"] <- data$pre_earnings_price / data$eps_ttm
data[,"PE_after_newPrice"] <- data$pre_earnings_avg_price / data$eps_ttm

## PE Ratio Indicators
data[,"PE_Market_Estimate"] <- data$PE_before_earnings - data$PE_avg_before_earnings
data[, "PE_Market_Surprise"] <- data$PE_after_oldPrice - data$PE_after_newPrice
data[,"PE_Market_Outlook"] <- data$PE_after_newPrice - data$PE_avg_before_earnings

## rewuired returnto reach the old pe ratio
data[,"PE_Required_Return"] <- (data$PE_avg_before_earnings / data$PE_after_newPrice) - 1

# Save in DB --------------------------------------------------------------------
RSQLite::dbWriteTable(conn, "EARNINGS_DATA", data %>%
                        mutate(earnings_date = as.character(earnings_date)) %>%
                        distinct(),
                      append =FALSE, overwrite=TRUE)

symbols <- tbl(conn, "EARNINGS_DATA") %>% 
  data.frame() %>% pull(symbol) %>% unique()
index <- tq_index("SP500") %>%
  select(symbol, company, sector) %>% 
  filter(symbol %in% symbols)

# write metatable
RSQLite::dbWriteTable(conn, "META_DATA", index, append = FALSE, overwrite=TRUE)
