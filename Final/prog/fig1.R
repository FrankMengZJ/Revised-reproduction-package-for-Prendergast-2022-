
  library(ggplot2)
  source('prog/theme_tanutama.R')
  df <- read.csv('data/yellowpounds.csv')
  fig <- ggplot(df, aes(Year, Pounds..million.)) +
    geom_line(color='grey60', size=1.1) +
    geom_point(size=2.5) + 
    scale_y_continuous(breaks = seq(150, 350, 25)) +
    coord_cartesian(ylim=c(200,355)) +
    scale_x_continuous(breaks = 2000:2011) +
    ylab('Pounds (Million)') +
    theme_tanutama()
  ggsave('figs/pdf/pounds1.pdf', fig,
         width = 7, height = 4.5)  
  