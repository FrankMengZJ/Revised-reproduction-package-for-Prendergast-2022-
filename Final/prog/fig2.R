  
  library(ggplot2)
  library(dplyr)
  source('prog/theme_tanutama.R')
  df <- read.csv('data/bidding.csv')
  df$unitprice <- df$pricePerPound
  df$unitprice[which(df$unitprice>1.5)] <- 1.5
  df$unitprice[which(df$unitprice< -0.2)] <- (-0.2)
  N <- length(which(!is.na(df$unitprice)))
  yax <- seq(0, 0.35, 0.05)
  figup <- ggplot(df, aes(unitprice)) +
    geom_histogram( binwidth=0.05, fill = 'grey',
                   color = 'black', size= 0.35,
                   alpha = 0.75) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = yax*N,
                       labels = c('0', '0.05',
                                  '0.10', '0.15',
                                  '0.20', '0.25',
                                  '0.30', '0.35')) +
    coord_cartesian(ylim=c(0,20650)) +
    xlab('Shares per pound') +
    scale_x_continuous(breaks = c(0,5,10,15)/10,
                       labels = c(0,0.5,1.0,'1.5+')) +
    ylab('Frequency') +
    theme_tanutama()
  ggsave('figs/pdf/priceperpound.pdf', figup,
         width = 7, height = 4.5)  
  
  
  