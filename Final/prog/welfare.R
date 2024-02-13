
  # Welfare
  library(xtable)
  library(tidyverse)
  df <- readRDS('data/bidding.rds') 
  
  gfid <- unique(df[, c('choicemember', 'id', 'biddate', 'goalFactor', 'year')])
  
  gfid$year <- as.character(gfid$year)
  
  df$bimonth <- paste(df$year, 
                      (df$month_num+1) %/% 2, 
                      sep = '-')
  df$quarter <- paste(df$year, 
                      (df$month_num+2) %/% 3, 
                      sep = '-')
  TT <- levels(as.factor(df$type))
  II <- levels(as.factor(df$id))
  MM <- levels(as.factor(df$monthyear))
  BB <- levels(as.factor(df$bimonth))
  CC <- levels(as.factor(df$quarter))
  
  
  # Price share
  p_i <- df %>% 
    group_by(type) %>% 
    dplyr::summarise(P = mean(pricePerPound),
                     pound = sum(totalPounds))
  
  s_i <- df %>% 
    group_by(type) %>%
    dplyr::summarise(si = sum(totalPounds))
  s_i$si <- s_i$si / sum(s_i$si)
  
  
  # Elasticity
  p_i$E1 <- p_i$E2 <- (-0.6)
  p_i$E1[which(p_i$type %in%
                 c('Drinks', 'Meat', 'Dairy',
                   'Cereal', 'Produce', 'Juice'))] <-  
    c(-0.79, -0.75, -0.65, -0.6, -0.58, -0.76)
  p_i$E2[which(p_i$type %in%
                 c('Produce', 'Meat', 'Dairy',
                   'Cereal', 'Snack'))] <-  
    c(-0.53, -0.6, -0.6, -0.43, -0.56)
  
  # Quantity share
  sif1 <- df %>% 
    group_by(id, type) %>%
    dplyr::summarise(pounds = sum(totalPounds))
  sif <- expand.grid(levels(as.factor(sif1$id)),
                     levels(as.factor(sif1$type)))
  colnames(sif) <- c('id', 'type')
  sif <- merge(sif, sif1, all.x = T)
  sif$pounds[which(is.na(sif$pounds))] <- 0
  sif <- left_join(sif, p_i) %>%
    mutate(sif = pounds/pound)
  
  # SHare over mean
  uniquegf <- unique(df[,
                                                 c('choicemember', 'id', 'biddate', 'goalFactor')])
  uniquegf <- aggregate(goalFactor ~ choicemember + id,
                        data = uniquegf, mean)
  uniquegf$sgf <- sum(uniquegf$goalFactor)
  sf <- uniquegf
  sf$goalFactor <- sf$goalFactor / sf$sgf
  sf$totalgf <- NULL
  sf$sgf <- NULL
  sf <- sf[order(sf$id),]
  sif <- left_join(sif, sf)
  sif <- left_join(sif, s_i)
  
  
  
  # Quantities over 1, 2, 3 month interval
  Qi1 <- df %>% group_by(type, year, monthyear) %>% dplyr::summarise(LB = sum(totalPounds))
  Qi2 <- df %>% group_by(type, year, bimonth) %>% dplyr::summarise(LB = sum(totalPounds))
  Qi3 <- df %>% group_by(type, year, quarter) %>% dplyr::summarise(LB = sum(totalPounds))
  
  # make balanced panel again
  QQ1 <- expand.grid(levels(as.factor(df$type)),
                     levels(as.factor(df$monthyear)))
  colnames(QQ1) <- c('type', 'monthyear')
  Qi1 <- left_join(QQ1, Qi1)
  Qi1$year <- as.character(Qi1$year)
  Qi1$LB[which(is.na(Qi1$LB))] <- 0
  Qi1$year <- substr(Qi1$monthyear, 4, 7)
  
  QQ2 <- expand.grid(levels(as.factor(df$type)),
                     levels(as.factor(df$bimonth)))
  colnames(QQ2) <- c('type', 'bimonth')
  Qi2 <- left_join(QQ2, Qi2)
  Qi2$year <- as.character(Qi2$year)
  Qi2$LB[which(is.na(Qi2$LB))] <- 0
  Qi2$year <- substr(Qi2$bimonth, 1, 4)
  
  QQ3 <- expand.grid(levels(as.factor(df$type)),
                     levels(as.factor(df$quarter)))
  colnames(QQ3) <- c('type', 'quarter')
  Qi3 <- left_join(QQ3, Qi3)
  Qi3$year <- as.character(Qi3$year)
  Qi3$LB[which(is.na(Qi3$LB))] <- 0
  Qi3$year <- substr(Qi3$quarter, 1, 4)
  
  # Absolute value
  oif <- df %>% group_by(type, id) %>% 
    dplyr::summarise(o = sum(totalPounds))
  oif$o <- oif$o / sum(oif$o)
  
  sif   <- left_join(sif, oif)
  sif$o[which(is.na(sif$o))] <- 0
  with(sif, sum(abs(sif - goalFactor) * si))
  with(sif, sum(abs((sif/goalFactor) - 1) * o))
  
  
  fff <- df %>% group_by(type) %>% 
    dplyr::summarise(food = sum(totalPounds))
  fff$food <- fff$food / sum(fff$food)
  
  
  # unit of analysis
  Qif1 <- df %>% group_by(type, id, monthyear) %>% dplyr::summarise(LBf = sum(totalPounds))
  pan1 <- expand.grid(type = TT,
                      id   = II,
                      monthyear = MM)
  Qif1 <- left_join(pan1, Qif1)
  Qif1 <- left_join(Qif1, oif)
  Qif1 <- left_join(Qif1, fff)
  Qif1$LBf[which(is.na(Qif1$LBf))] <- 0
  Qif1$o[which(is.na(Qif1$o))] <- 0
  Qif1 <- left_join(left_join(Qif1, Qi1), sif)
  
  Qif2 <- df %>% group_by(type, id, bimonth) %>% dplyr::summarise(LBf = sum(totalPounds))
  pan2 <- expand.grid(type = TT,
                      id   = II,
                      bimonth = BB)
  Qif2 <- left_join(pan2, Qif2)
  Qif2 <- left_join(Qif2, oif)
  Qif2 <- left_join(Qif2, fff)
  Qif2$LBf[which(is.na(Qif2$LBf))] <- 0
  Qif2$o[which(is.na(Qif2$o))] <- 0
  Qif2 <- left_join(left_join(Qif2, Qi2), sif)
  
  Qif3 <- df %>% group_by(type, id, quarter) %>% dplyr::summarise(LBf = sum(totalPounds))
  pan3 <- expand.grid(type = TT,
                      id   = II,
                      quarter = CC)
  Qif3 <- left_join(pan3, Qif3)
  Qif3 <- left_join(Qif3, oif)
  Qif3 <- left_join(Qif3, fff)
  Qif3$LBf[which(is.na(Qif3$LBf))] <- 0
  Qif3$o[which(is.na(Qif3$o))] <- 0
  Qif3 <- left_join(left_join(Qif3, Qi3), sif)
  
  
  
  # A.  Compute Total Welfare
  # Short period increases welfare
  Qif1$D   <- with(Qif1, sif * LB)
  Qif1$ND1 <- with(Qif1, (P * (LBf - goalFactor * LB)^2) / (20 * sif * LB * E1))
  Qif1$ND2 <- with(Qif1, (P * (LBf - goalFactor * LB)^2) / (20 * sif * LB * E2))
  Qif1$A   <- with(Qif1, abs((LBf - goalFactor * LB) / (goalFactor * LB) ) * si * goalFactor)
  Qif1$O   <- with(Qif1, abs((LBf - goalFactor * LB) / (goalFactor * LB) ) * o)
  Qif1$Year <- substr(Qif1$monthyear, 4, 7)
  W1 <- Qif1[-which(Qif1$D == 0),] %>% group_by(Year) %>%
    dplyr::summarise(W1 = -sum(ND1),
                     W2 = -sum(ND2))
  A1 <- Qif1[-which(Qif1$LB == 0),] %>% 
    group_by(year, monthyear) %>%
    dplyr::summarise(A1 = sum(A))
  O1 <- Qif1[-which(Qif1$LB == 0),] %>% 
    group_by(year, monthyear) %>%
    dplyr::summarise(O1 = sum(O))
  
  Qif2$D   <- with(Qif2, sif * LB)
  Qif2$ND1 <- with(Qif2, (P * (LBf - goalFactor * LB)^2) / (20 * sif * LB * E1))
  Qif2$ND2 <- with(Qif2, (P * (LBf - goalFactor * LB)^2) / (20 * sif * LB * E2))
  Qif2$A   <- with(Qif2, abs((LBf - goalFactor * LB) / (goalFactor * LB) ) * si * goalFactor)
  Qif2$O   <- with(Qif2, abs((LBf - goalFactor * LB) / (goalFactor * LB) ) * o)
  Qif2$Year <- substr(Qif2$bimonth, 1, 4)
  W2 <- Qif2[-which(Qif2$D == 0),] %>% group_by(Year) %>%
    dplyr::summarise(W1 = -sum(ND1),
                     W2 = -sum(ND2))
  A2 <- Qif2[-which(Qif2$LB == 0),] %>% 
    group_by(year, bimonth) %>%
    dplyr::summarise(A2 = sum(A))
  O2 <- Qif2[-which(Qif2$LB == 0),] %>% 
    group_by(year, bimonth) %>%
    dplyr::summarise(O2 = sum(O))
  
  
  
  Qif3$D   <- with(Qif3, sif * LB)
  Qif3$ND1 <- with(Qif3, (P * (LBf - goalFactor * LB)^2) / (20 * sif * LB * E1))
  Qif3$ND2 <- with(Qif3, (P * (LBf - goalFactor * LB)^2) / (20 * sif * LB * E2))
  Qif3$A   <- with(Qif3, abs((LBf - goalFactor * LB) / (goalFactor * LB) ) * si * goalFactor)
  Qif3$O   <- with(Qif3, abs((LBf - goalFactor * LB) / (goalFactor * LB) ) * o)
  Qif3$Year <- substr(Qif3$quarter, 1, 4)
  W3 <- Qif3[-which(Qif3$D == 0),] %>% group_by(Year) %>%
    dplyr::summarise(W1 = -sum(ND1),
                     W2 = -sum(ND2))
  A3 <- Qif3[-which(Qif3$LB == 0),] %>% 
    group_by(year, quarter) %>%
    dplyr::summarise(A3 = sum(A))
  O3 <- Qif3[-which(Qif3$LB == 0),] %>% 
    group_by(year, quarter) %>%
    dplyr::summarise(O3 = sum(O))
  
  
  
  
  
  # MERGE ALL
  monthdf <- data.frame(m = 1:12,
                        month = c('Jan','Feb','Mar',
                                  'Apr','May','Jun',
                                  'Jul','Aug','Sep',
                                  'Oct','Nov','Dec'))
  bimonthdf <- data.frame(m = 1:6,
                          month = c('Jan-Feb', 'Mar-Apr', 'May-Jun',
                                    'Jul-Aug','Sep-Oct','Nov-Dec'))
  
  asd1 <- left_join(A1, O1)
  asd1$month <- substr(asd1$monthyear, 1, 3)
  asd1 <- left_join(asd1, monthdf)
  asd1 <- as.data.frame(asd1)
  asd1 <- with(asd1, asd1[order(year,m),])
  asd1 <- asd1[,c('year','month','A1','O1')]
  
  asd2 <- left_join(A2, O2)
  asd2$m <- as.numeric(substr(asd2$bimonth, 6, 6))
  asd2 <- left_join(asd2, bimonthdf)
  asd2 <- as.data.frame(asd2)
  asd2 <- asd2[,c('year','month','A2','O2')]
  
  asd3 <- left_join(A3, O3)
  asd3$m <- as.numeric(substr(asd3$quarter, 6, 6))
  asd3 <- as.data.frame(asd3)
  asd3 <- asd3[,c('year','m','A3','O3')]
  
  print(xtable(asd1), include.rownames = F)
  print(xtable(asd2), include.rownames = F)
  print(xtable(asd3), include.rownames = F)
  
  apply(asd1[,3:4], 2, mean)
  apply(asd2[,3:4], 2, mean)
  apply(asd3[,3:4], 2, mean)
  
  
  
  Welfare <- cbind(W1, W2, W3)
  Welfare <- Welfare[,-c(4,7)]      
  D <- Welfare[,2:7] * 5.97 / 1000000
  Dollars <- cbind(2005:2011,  round(D,1))
  round(apply(apply(Welfare, 2, as.numeric), 2, mean)/1000000, 2)
  round(apply(D,2,mean), 1)
  Welfare[,2:7] <- round(Welfare[,2:7]/1000000, 2)
  
  library(xtable)
  print(xtable(Welfare), include.rownames = F)
  print(xtable(Dollars, digits = 1), include.rownames = F)
  
  
  
  
  # B.  Compute Welfare due to Permanent Sorting
  # Short period increases welfare
  Qif1$ND1 <- with(Qif1, (P * LB * (sif - goalFactor)^2) / (20 * sif * E1))
  Qif1$ND2 <- with(Qif1, (P * LB * (sif - goalFactor)^2) / (20 * sif * E2))
  Qif1$Year <- substr(Qif1$monthyear, 4, 7)
  W1 <- Qif1[-which(Qif1$D == 0),] %>% group_by(Year) %>%
    dplyr::summarise(W1 = -sum(ND1),
                     W2 = -sum(ND2))
  
  Qif2$ND1 <- with(Qif2, (P * LB * (sif - goalFactor)^2) / (20 * sif * E1))
  Qif2$ND2 <- with(Qif2, (P * LB * (sif - goalFactor)^2) / (20 * sif * E2))
  Qif2$Year <- substr(Qif2$bimonth, 1, 4)
  W2 <- Qif2[-which(Qif2$D == 0),] %>% group_by(Year) %>%
    dplyr::summarise(W1 = -sum(ND1),
                     W2 = -sum(ND2))
  
  Qif3$ND1 <- with(Qif3, (P * LB * (sif - goalFactor)^2) / (20 * sif * E1))
  Qif3$ND2 <- with(Qif3, (P * LB * (sif - goalFactor)^2) / (20 * sif * E2))
  Qif3$Year <- substr(Qif3$quarter, 1, 4)
  W3 <- Qif3[-which(Qif3$D == 0),] %>% group_by(Year) %>%
    dplyr::summarise(W1 = -sum(ND1),
                     W2 = -sum(ND2))
  
  
  Welfare <- cbind(W1, W2, W3)
  Welfare <- Welfare[,-c(4,7)]      
  D <- Welfare[,2:7] * 5.97 / 1000000
  Dollars <- cbind(2005:2011,  round(D,1))
  round(apply(apply(Welfare, 2, as.numeric), 2, mean)/1000000, 2)
  round(apply(D,2,mean), 1)
  Welfare[,2:7] <- round(Welfare[,2:7]/1000000, 2)
  
  library(xtable)
  print(xtable(Welfare), include.rownames = F)
  print(xtable(Dollars, digits = 1), include.rownames = F)
  
  
  #  C. Absolute value statistic
  
  Qif1  <- left_join(Qif1, oif)
  Qif2  <- left_join(Qif2, oif)
  Qif3  <- left_join(Qif3, oif)
  
  Qif1$A <- with(Qif1, abs((LBf - goalFactor * LB) / (goalFactor * LB) ) * o)
  A1 <- Qif1 %>% group_by(Year) %>%
    dplyr::summarise(W1 = -sum(ND1),
                     W2 = -sum(ND2))
  