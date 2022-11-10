# Main Function for Scrapping relevant earnings for the different strategies
ScrapeEarnings <- function(start_date, end_date){
  
  ## yahoo finance Earnings
  yahoo <- getEarningsCalendar(date1 = start_date, date2 = end_date)
  
  ## get nasdaqq earnings calendar
  nasdaqq <- pullNQ()
  
  ## get seeking alpha calendar
  seeking_alpha <- pullSeekingAlpha()
  seeking_alpha <- mutateSeekingAlpha(calendar = seeking_alpha) %>%
    as.data.frame() %>%
    mutate(
      estimate = NA,
      eps = NA,
      surprise = NA,
    )
  
  ## filter and combine
  yahoo_filter <- yahoo %>% pull(symbol) %>% lapply(., check_Universe) %>% unlist()
  nasdaqq_filter <- nasdaqq %>% pull(symbol) %>% lapply(., check_Universe) %>% unlist()
  seeking_alpha_filter <- seeking_alpha %>% pull(symbol) %>% lapply(., check_Universe) %>% unlist()
  
  yahoo <- yahoo[yahoo_filter,] %>% distinct(symbol, .keep_all = TRUE)
  nasdaqq <- nasdaqq[nasdaqq_filter,] %>% distinct(symbol, .keep_all = TRUE)
  seeking_alpha <- seeking_alpha[seeking_alpha_filter,] %>% distinct(symbol, .keep_all = TRUE)
  
  res <- yahoo %>%
    rbind(., nasdaqq) %>%
    rbind(., seeking_alpha) %>%
    select(symbol) %>%
    distinct() %>%
    left_join(., yahoo %>% select(-company), by = c("symbol"="symbol"),
              suffix = c("", "_y")) %>%
    left_join(., nasdaqq %>% select(-company), by = c("symbol"="symbol"),
              suffix = c("", "_nq")) %>%
    left_join(., seeking_alpha %>% select(-company), by = c("symbol"="symbol"),
              suffix = c("", "_sa"))
  
  
  ## consolidate
  res$earnings_date <- if_else(!is.na(res$earnings_date),
                               as.Date(res$earnings_date),
                               as.Date(res$earnings_date_nq))
  res$earnings_date <- if_else(!is.na(res$earnings_date),
                               as.Date(res$earnings_date),
                               as.Date(res$earnings_date_sa))
  
  res$earnings_time <- if_else(!is.na(res$earnings_time),
                               res$earnings_time,
                               res$earnings_time_nq)
  res$earnings_time <- if_else(!is.na(res$earnings_time),
                               res$earnings_time,
                               res$earnings_time_sa)
  
  res$estimate <-ifelse(!is.na(res$estimate),res$estimate,res$estimate_nq)
  res$estimate <-ifelse(!is.na(res$estimate),res$estimate,res$estimate_sa)
  
  res$eps <-ifelse(!is.na(res$eps),res$eps,res$eps_nq)
  res$eps <-ifelse(!is.na(res$eps),res$eps,res$eps_sa)
  
  res$surprise <-ifelse(!is.na(res$surprise),res$surprise,res$surprise_nq)
  res$surprise <-ifelse(!is.na(res$surprise),res$surprise,res$surprise_sa)
  
  res <- res %>%
    select("symbol", "earnings_date", "earnings_time", "estimate", "eps", "surprise" ) %>%
    filter(!is.na(earnings_date))
  res$earnings_time[is.na(res$earnings_time)] <- "kp"
  
  ## Filter for the viability in a certain strategy
  res <- res %>%
    filter(earnings_date <= Sys.Date()+1,
           earnings_date >= Sys.Date()-2)
  
  ### Construct PEAD Viability signal
  res$PEAD <- res$earnings_date < Sys.Date()
  res$PEAD <- res$PEAD | (res$earnings_date == Sys.Date() & res$earnings_time != "AMC")
  
  ### construct over earnings trade viability signal
  res$OverEarnings <- res$earnings_date == Sys.Date() & res$earnings_time != "BMO"
  res$OverEarnings <- res$OverEarnings | (res$earnings_date == (Sys.Date()+1) & res$earnings_time != "AMC")
  
  return(res)
}

################################################################################
# Seeking alpha #
################################################################################
pullSeekingAlpha <- function(){
  
  for(j in 1:5){
    catcher <- tryCatch(expr = {
      ## pull html and how many sites
      url <- "https://seekingalpha.com/earnings/earnings-calendar"
      data <- read_html(url)
      sites <- data %>%
        html_element(xpath = "/html/body/div[3]/div/div[2]/div[1]/div/div[2]/div/ul") %>%
        html_text() %>%
        gsub(pattern = "Next Page", replacement = "", x = .) %>%
        str_split(string = ., pattern = "") %>%
        unlist()
      
      RES <- list()
      for(k in 1:length(sites)){
        if(k == 1){
          RES[[k]] <- data %>%
            html_table() %>%
            data.frame()
        } else{
          temp_url <- paste0(url, "/", k)
          temp_data <- read_html(temp_url)
          RES[[k]] <- temp_data %>%
            html_table() %>%
            data.frame()
        }
      }
      return(RES %>% rbindlist())
    }, error = function(cond){
      return(FALSE)
    })
    
    if(!catcher){
      break
    }
    
  }
  res <- RES %>% rbindlist()
  
  return(res)
}

# mapping seeking alpha earnignstime stuff -------------------------------------
mutateSeekingAlpha <- function(calendar){
  calendar <- calendar[,1:4]
  calendar$Release.Time <- ifelse(calendar$Release.Time == "Pre-Market", "BMO", calendar$Release.Time)
  calendar$Release.Time <- ifelse(calendar$Release.Time == "Post-Market", "AMC", calendar$Release.Time)
  calendar$Release.Time <- ifelse(calendar$Release.Time %in% c("BMO", "AMC"), calendar$Release.Time, "kp")
  calendar$Release.Date <- calendar$Release.Date %>%
    as.Date(x = ., format = "%m/%d/%Y")
  
  colnames(calendar) <- c("symbol", "company", "earnings_date", "earnings_time")
  return(calendar)
}

################################################################################
# Seeking alpha #
################################################################################
pullNQ <- function(){
  
  ## Initialize Selenium
  rD <- rsDriver(browser=c("chrome"),
                 chromever="107.0.5304.18",
                 port=4444L,
                 verbose=F)
  rD$client$open()
  
  ## Navigate to webpage
  url <- "https://www.nasdaq.com/market-activity/earnings"
  rD$client$navigate(url = url)
  
  ## fuck off pop ups
  Sys.sleep(5)
  popup <- rD$client$findElements(using = "class name",
                                  value = "banner-actions-container")
  popup[[1]]$clickElement()
  
  ## get todays
  Sys.sleep(runif(n = 1, 1,3) %>% as.numeric())
  page <- rD$client$findElements(using = "class name", value = "market-calendar-table__row")
  todays <- nasdaqGetEarningsTable_Today(page)
  
  ### get earnings time
  icons <- rD$client$findElements(using= "class name",
                                  value = "market-calendar-table__icons")
  todays$earnings_time <- getEarningsTableIcons(icons)
  
  ## get yesterday
  ### navigate to yesterdays
  time_belt <- rD$client$findElements(using = "class name", value = "time-belt__item")
  yesterDay <- Sys.Date()-1
  yesterDay <- yesterDay %>% day()
  for(k in 1:length(time_belt)){
    
    tDay  <- time_belt[[k]]$getElementAttribute("data-day") %>%
      as.numeric()
    
    if(yesterDay == tDay){
      yesterday_button <- k
    }
  }
  time_belt[[yesterday_button]]$clickElement()
  Sys.sleep(runif(n = 1, 1,3) %>% as.numeric())
  
  ### Get Yesterdays Earnings
  page <- rD$client$findElements(using = "class name", value = "market-calendar-table__row")
  yesterdays <- nasdaqGetEarningsTable_Past(page)
  icons <- rD$client$findElements(using= "class name",
                                  value = "market-calendar-table__icons")
  yesterdays$earnings_time <- getEarningsTableIcons(icons)
  
  
  ## Get tomorrows earnings
  time_belt <- rD$client$findElements(using = "class name", value = "time-belt__item")
  toMorrow <- Sys.Date()+ 1
  toMorrow <- toMorrow %>% day()
  for(k in 1:length(time_belt)){
    
    tDay  <- time_belt[[k]]$getElementAttribute("data-day") %>%
      as.numeric()
    
    if(toMorrow == tDay){
      t_button <- k
    }
  }
  time_belt[[t_button]]$clickElement()
  Sys.sleep(runif(n = 1, 1,3) %>% as.numeric())
  
  ## get tomorrows
  page <- rD$client$findElements(using = "class name", value = "market-calendar-table__row")
  tomorrows <- nasdaqGetEarningsTable_Future(page)
  
  ### get earnings time
  icons <- rD$client$findElements(using= "class name",
                                  value = "market-calendar-table__icons")
  tomorrows$earnings_time <- getEarningsTableIcons(icons)
  
  
  result <- yesterdays %>%
    rbind(., todays) %>%
    rbind(., tomorrows) %>%
    select(symbol, company, earnings_date, earnings_time,
           estimate, eps, surprise)
  
  rD$client$close()
  return(result)
  
}
nasdaqGetEarningsTable_Future <-function(page){
  # page <- rD$client$findElements(using = "class name", value = "market-calendar-table__row")
  res <- list()
  for(k in 1:length(page)){
    temp_data <- page[[k]]$getElementAttribute("outerHTML")[[1]] %>%
      read_html() %>%
      html_text() %>%
      gsub(pattern = " ", replacement = "", x = .) %>%
      str_split(string = ., pattern = "\n") %>%
      unlist() %>%
      .[.!= ""] %>%
      .[c(1,2,5,8)]
    
    res[[k]] <- data.frame(
      "symbol"=c(temp_data[1]),
      "company"=c(temp_data[2]),
      "earnings_date"=c(Sys.Date()+1),
      "estimate"=c(temp_data[4])
    )
  }
  todays <- res %>%
    rbindlist() %>%
    mutate(
      earnings_date = earnings_date %>%
        as.Date(., tryFormats=c("%m/%d/%Y")),
      estimate = estimate %>%
        str_remove(string = ., pattern = ".") %>%
        as.numeric(),
      eps = NA,
      surprise = NA
    ) %>%
    filter(!is.na(symbol))
  
  return(todays)
}


nasdaqGetEarningsTable_Today <-function(page){
  # page <- rD$client$findElements(using = "class name", value = "market-calendar-table__row")
  res <- list()
  for(k in 1:length(page)){
    temp_data <- page[[k]]$getElementAttribute("outerHTML")[[1]] %>%
      read_html() %>%
      html_text() %>%
      gsub(pattern = " ", replacement = "", x = .) %>%
      str_split(string = ., pattern = "\n") %>%
      unlist() %>%
      .[.!= ""] %>%
      .[c(1,2,5,7,8)]
    
    res[[k]] <- data.frame(
      "symbol"=c(temp_data[1]),
      "company"=c(temp_data[2]),
      "earnings_date"=c(temp_data[4]),
      "estimate"=c(temp_data[3])
    )
  }
  todays <- res %>%
    rbindlist() %>%
    mutate(
      earnings_date = earnings_date %>%
        as.Date(., tryFormats=c("%m/%d/%Y")),
      estimate = estimate %>%
        str_remove(string = ., pattern = ".") %>%
        as.numeric(),
      eps = NA,
      surprise = NA
    ) %>%
    filter(!is.na(symbol))
  
  return(todays)
}
nasdaqGetEarningsTable_Past <-function(page){
  # page <- rD$client$findElements(using = "class name", value = "market-calendar-table__row")
  res <- list()
  for(k in 1:length(page)){
    temp_data <- page[[k]]$getElementAttribute("outerHTML")[[1]] %>%
      read_html() %>%
      html_text() %>%
      gsub(pattern = " ", replacement = "", x = .) %>%
      str_split(string = ., pattern = "\n") %>%
      unlist() %>%
      .[.!= ""] %>%
      .[c(1,2,3,4,7)]
    
    res[[k]] <- data.frame(
      "symbol"=c(temp_data[1]),
      "company"=c(temp_data[2]),
      "earnings_date"=c(Sys.Date()-1),
      "estimate"=c(temp_data[5]),
      "eps"=c(temp_data[3])
    )
  }
  yesterdays <- res %>%
    rbindlist() %>%
    mutate(
      earnings_date = earnings_date %>%
        as.Date(., tryFormats=c("%m/%d/%Y")),
      estimate = estimate %>%
        str_remove(string = ., pattern = ".") %>%
        as.numeric(),
      eps = eps %>%
        str_remove(string = ., pattern = ".") %>%
        as.numeric(),
      surprise = (eps - estimate ) / estimate,
    ) %>%
    filter(!is.na(symbol))
  
  return(yesterdays)
}


getEarningsTableIcons <- function(icons){
  icons %>%
    lapply(., function(x){
      time <- x$getElementAttribute("alt")[[1]]
      if(time == "time-pre-market"){
        return("BMO")
      } else if(time == "time-after-hours"){
        return("AMC")
      } else{
        return("kp")
      }
    }) %>%
    unlist()
}

################################################################################
# yahoo Finance #
################################################################################
# Load historical earnings for any ticker --------------------------------------
get_historical_earnings = function(ticker)
{
  url <- paste0("https://finance.yahoo.com/calendar/earnings?failsafe=1&ynet=0&_device=desktop&device=desktop&symbol=" , ticker)
  data <- read_html(url)
  data <- data %>% html_table()
  data <- as.data.frame(data)
  time <- data[,"Earnings.Date"] %>%
    str_sub(., -5,-4)
  data[, "earnings_time"] <- ifelse(time == "PM", "AMC", "BMO")
  data[,"Earnings.Date"] <- paste0(str_sub(data[,"Earnings.Date"], 8,12),"-",
                                   str_sub(data[,"Earnings.Date"], 1,3) %>%
                                     lapply(., function(x){
                                       if(x == "Jan"){
                                         return(1)
                                       } else if(x == "Feb"){
                                         return(2)
                                       } else if(x == "Mar"){
                                         return(3)
                                       } else if(x == "Apr"){
                                         return(4)
                                       } else if(x == "May"){
                                         return(5)
                                       } else if(x == "Jun"){
                                         return(6)
                                       } else if(x == "Jul"){
                                         return(7)
                                       } else if(x == "Aug"){
                                         return(8)
                                       } else if(x == "Sep"){
                                         return(9)
                                       } else if(x == "Oct"){
                                         return(10)
                                       } else if(x == "Nov"){
                                         return(11)
                                       } else if(x == "Dec"){
                                         return(12)
                                       }
                                     }) %>%
                                     unlist(), "-",
                                   str_sub(data[,"Earnings.Date"], 4,6)
  ) %>%
    gsub(pattern = " ", replacement = "") %>%
    as.Date()
  data <- data %>%
    select(symbol = "Symbol", company = "Company",
           earnings_date = "Earnings.Date",
           earnings_time, estimate = "EPS.Estimate",
           eps = "Reported.EPS", surprise = "Surprise...")
  
  data <- data %>%
    filter(eps != "-") %>%
    mutate(
      "estimate" = estimate %>% as.numeric(),
      "eps" = eps %>% as.numeric(),
      "surprise" = (surprise %>%
                      gsub(pattern = "+", replacement = "") %>%
                      as.numeric()) / 100
    )
  
  return(data)
}
################################################################################
# Load historical earnings for any ticker --------------------------------------
get_current_earnings = function(ticker)
{
  url <- paste0("https://finance.yahoo.com/calendar/earnings?failsafe=1&ynet=0&_device=desktop&device=desktop&symbol=" , ticker)
  data <- read_html(url)
  data <- data %>% html_table()
  data <- as.data.frame(data)
  time <- data[,"Earnings.Date"] %>%
    str_sub(., -5,-4)
  data[, "earnings_time"] <- ifelse(time == "PM", "AMC", "BMO")
  data[,"Earnings.Date"] <- paste0(str_sub(data[,"Earnings.Date"], 8,12),"-",
                                   str_sub(data[,"Earnings.Date"], 1,3) %>%
                                     lapply(., function(x){
                                       if(x == "Jan"){
                                         return(1)
                                       } else if(x == "Feb"){
                                         return(2)
                                       } else if(x == "Mar"){
                                         return(3)
                                       } else if(x == "Apr"){
                                         return(4)
                                       } else if(x == "May"){
                                         return(5)
                                       } else if(x == "Jun"){
                                         return(6)
                                       } else if(x == "Jul"){
                                         return(7)
                                       } else if(x == "Aug"){
                                         return(8)
                                       } else if(x == "Sep"){
                                         return(9)
                                       } else if(x == "Oct"){
                                         return(10)
                                       } else if(x == "Nov"){
                                         return(11)
                                       } else if(x == "Dec"){
                                         return(12)
                                       }
                                     }) %>%
                                     unlist(), "-",
                                   str_sub(data[,"Earnings.Date"], 4,6)
  ) %>%
    gsub(pattern = " ", replacement = "") %>%
    as.Date()
  data <- data %>%
    select(symbol = "Symbol", company = "Company",
           earnings_date = "Earnings.Date",
           earnings_time, estimate = "EPS.Estimate",
           eps = "Reported.EPS", surprise = "Surprise...")
  
  data <- data %>%
    filter(estimate != "-") %>%
    mutate(
      "estimate" = estimate %>% as.numeric(),
      "eps" = eps %>% as.numeric(),
      "surprise" = (surprise %>%
                      gsub(pattern = "+", replacement = "") %>%
                      as.numeric()) / 100
    )
  
  return(data)
}
################################################################################
getEarningsCalendar <- function(date1, date2){
  
  ## Load initial Calendar
  if(weekdays(date1) == "Sonntag"){
    date1 <- date1+1
  } else if(weekdays(date1) == "Samstag"){
    date1 <- date1-1
  }
  url = paste0("https://finance.yahoo.com/calendar/earnings?day=", date1)
  data <- read_html(url)
  calendar <- data %>%
    html_table() %>%
    as.data.frame() %>%
    mutate(date = date1)
  
  ## rbind the rest
  days <- as.numeric(as.Date(date2) - as.Date(date1))
  
  for(k in 1:days){
    wd <- as.Date(date1) + k
    if(!(weekdays(wd) %in% c("Sonntag", "Samstag"))){
      Sys.sleep(sample(10, 1) * 0.1)
      catcher <- tryCatch({
        tmp_date <- as.Date(date1)+k
        url = paste0("https://finance.yahoo.com/calendar/earnings?day=", tmp_date)
        data <- read_html(url)
        calendar <- calendar %>%
          rbind(., data %>%
                  html_table() %>%
                  as.data.frame() %>%
                  mutate(date = tmp_date)
          )
        FALSE
      }, error = function(cond){
        print(paste0("Error: ", tmp_date))
        TRUE
      })
      if(catcher){
        Sys.sleep(sample(10, 1) * 0.1)
        tmp_date <- as.Date(date1)+k
        url = paste0("https://finance.yahoo.com/calendar/earnings?day=", tmp_date)
        data <- read_html(url)
        calendar <- calendar %>%
          rbind(., data %>%
                  html_table() %>%
                  as.data.frame() %>%
                  mutate(date = tmp_date)
          )
      }
    }
  }
  
  calendar %>%
    mutate(
      "earnings_time" = case_when(
        Earnings.Call.Time == "Before Market Open" ~ "BMO",
        Earnings.Call.Time == "After Market Close" ~ "AMC",
        Earnings.Call.Time == "TAS" ~ as.character(NA),
        Earnings.Call.Time == "Time Not Supplied" ~ as.character(NA),
        TRUE ~ as.character(NA)
      ),
      "estimate"=ifelse(EPS.Estimate == "-", NA, EPS.Estimate) %>% as.numeric(),
      "eps"=ifelse(Reported.EPS == "-", NA, Reported.EPS) %>% as.numeric(),
      "surprise"=  (ifelse(Surprise...=="-", NA, Surprise...) %>%
                      gsub(pattern="+", replacement = "") %>% as.numeric())/100,
    ) %>%
    select(symbol = Symbol, company = Company, earnings_date = date,
           earnings_time, estimate, eps, surprise) %>%
    return(.)
  
}

################################################################################
# Finviz #
################################################################################
## Work in Progress
getFinviz_Data <- function(ticker){
  
  url <- paste0("https://finviz.com/quote.ashx?t=", ticker, "&p=d")
  data <- read_html(url) %>%
    html_table() %>%
    .[[9]] %>%
    as.data.frame()
  
  cols <- 1:ncol(data) %>%
    lapply(., function(x){
      if(x %% 2 != 0){
        return(data[,x] %>% as.character())
      } else{
        return(NA)
      }
      
    }) %>%
    unlist() %>%
    .[!is.na(.)]
  content <- 1:ncol(data) %>%
    lapply(., function(x){
      if(x %% 2 == 0){
        return(data[,x])
      } else{
        return(NA)
      }
      
    }) %>%
    unlist() %>%
    .[!is.na(.)]
  content[content == "-"] <- NA
  
  ## build output
  res <- matrix(nrow = 1, ncol = length(cols)) %>%
    as.data.frame()
  
  colnames(res) <- cols
  rownames(res) <- ticker
  res[1,] <- content
  
}
