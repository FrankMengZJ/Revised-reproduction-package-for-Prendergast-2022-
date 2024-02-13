  
  library(ggplot2)
  library(dplyr)
  source('prog/theme_tanutama.R')
  df <- read.csv('data/bidding.csv')
  df <- df %>% group_by(choicemember) %>%
    dplyr::summarise(shares = sum(totalShares, na.rm=T),
                     pounds = sum(totalPounds, na.tm=T)) %>%
    mutate(pricePerPound = shares/pounds,
      avgPounds = 1/pricePerPound)
  df$avgPounds[which(df$avgPounds>20)] <- 20
  
  
  fig <- ggplot(df, aes(avgPounds)) +
    geom_histogram(binwidth = 1, fill = 'grey',
                   color = 'black', size= 0.35,
                   alpha = 0.75) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0,20,5)) +
    coord_cartesian(ylim=c(0,21)) +
    geom_vline(xintercept=1/0.28,
               color = 'firebrick4') +
    xlab('Pounds per share') +
    scale_x_continuous(breaks = c(0,5,10,15,20),
                       labels = c(0,5,10,15,'20+')) +
    ylab('Number of Food Banks') +
    theme_tanutama()
  ggsave('figs/pdf/pps.pdf', fig,
         width = 7, height = 4.5)  
  
  
  