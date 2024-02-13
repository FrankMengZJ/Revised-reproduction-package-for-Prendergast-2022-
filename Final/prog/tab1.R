  
  auction <- readRDS('data/auction.rds')
  auction$year <- as.numeric(substr(auction$biddate, 1, 4))
  auction <- auction[which(auction$year %in% c(2005:2011)),]
  
  stat <- auction %>% 
    group_by(type) %>%
    dplyr::summarise(supply = sum(totalPounds),
                     bidder = mean(NBidder))
  
  stat$supply <- stat$supply/sum(stat$supply) * 100
  stat <- stat[order(stat$supply, decreasing = T),]
  