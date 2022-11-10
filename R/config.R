# Get functions to share config stuff across the ap
CONFIG <- function(STR){
  
  if(STR == "macro/etf"){
    return("C:/0cap_2/Infrastructure/Ressources/EPSILON_DB_1.db")
  } else if(STR == "stocks"){
    return("C:/0cap_2/Infrastructure/Ressources/YahooDB_1.db")
  } else if(STR == "NQ"){
    return("YjcYtntQykd9hXCpk6CZ")
  }
}
