
  library(dplyr)
  library(xtable)
  pounds <- read.csv("data/pounds.csv") 
  poundsum <- pounds %>%
    group_by(year) %>%
    dplyr::summarise(`Maroon Pounds` = sum(maroonPounds, na.rm=T)/1000000)
  
  df <- read.csv('data/bidding.csv') %>%
    mutate(num_losingbids = num_losingbids + 
             ifelse(numBanks_jointBid_unique == 0, 1,
                    numBanks_jointBid_unique))
  
  sumtable <- df %>% 
    group_by(year) %>%
    dplyr::summarise(`Average Number of Bids` = mean(num_losingbids),
                     `Credit` = mean(isCredit) * 100,
                     `Joint Bids` = mean(isJointBids) * 100 / 2,
                     `Negative Price` = mean(sign(pricePerPound < 0)) * 100) %>%
    inner_join(poundsum)
  
  print(xtable(sumtable), include.rownames = F)
  