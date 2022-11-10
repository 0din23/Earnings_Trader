# dependencies -----------------------------------------------------------------
source("R/dependencies.R")
source("R/config.R")
source("R/generell_helper_functions.R")

# Scrape relevant information --------------------------------------------------
source("R/Scrapping_Tools.R")
relevant_earnings <- ScrapeEarnings(start_date = Sys.Date()-1, end_date = Sys.Date()+1)

pead_candidates <- relevant_earnings %>% 
  filter(PEAD & !is.na(surprise))
oe_candidates <- relevant_earnings %>% 
  filter(OverEarnings)

DO_OE <- nrow(oe_candidates) > 0
DO_PEAD <- nrow(pead_candidates) > 0

# get relevant Option Data -----------------------------------------------------
if(DO_OE){
  oe_options <- oe_candidates %>%
    pull(symbol) %>%
    lapply(.,getOptionChain, Exp = NULL)  
} 

if(DO_PEAD){
  pead_options <- pead_candidates %>%
    pull(symbol) %>%
    lapply(.,getOptionChain, Exp = NULL) 
}




