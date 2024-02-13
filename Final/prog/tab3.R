
  library(lfe)
  library(dplyr)
  library(xtable)
  library(stargazer)
  library(multcomp)
  
  # Construct average price and total consumption
  df <- read.csv("data/winningBids.csv")
  df$pricePerPound <- with(df, winningbid/grossweight)
  df$year <- as.numeric(substr(df$biddate, 1, 4))
  idall <- sort(unique(df$id))
  price <- df %>% group_by(id , year) %>%
    dplyr::summarise(avgPrice    = mean(pricePerPound),
                     consumption = sum(totalShares))
  
  # Exclude FB with zero/missing yellowPounds or goalFactor
  all <- read.csv('data/pounds.csv')
  all <- all[all$year %in% c(2005:2011),]
  idna<- unique(
    c(all$id[which(is.na(all$yellowPounds))],
      all$id[which(all$yellowPounds==0)],
      all$id[which(is.na(all$goalFactor))],
      all$id[which(all$goalFactor==0)]))
  all <- all[which(!all$id %in% idna),]
  all <- merge(all, price)
  all$consumption_gf <- with(all, consumption/goalFactor)
  all$year <- as.factor(all$year)
  
  form <- c("log(avgPrice) ~ log(goalFactor) + year | 0 | 0 | id + year", 
            "log(avgPrice) ~ log(goalFactor) + year + log(goalFactor):year | 0 | 0 | id + year",
            "log(consumption_gf) ~ log(goalFactor) + year | 0 | 0 | id + year",
            "log(consumption_gf) ~ log(goalFactor) + year + log(goalFactor):year | 0 | 0 | id + year")

  regs <- lapply(form, function(x) felm(as.formula(x), 
                                        data = all[all$avgPrice!=0,]))  
  stargazer(regs)
  
  