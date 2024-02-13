
  library(forcats)
  library(ggplot2)
  source('prog/theme_tanutama.R')
  df <- read.csv('data/bidding.csv')
  df$type <- as.factor(df$type)
  df$type <- relevel(df$type, ref = 'Cereal')
  vars <- c('poundsPerShare', 'monthyear', 'goalFactor', 'id', 'type')
  
  df$monthyear <- as.factor(df$monthyear)
  df$monthyear <- relevel(df$monthyear, ref = 'Jul2005')
  df$price <- df$pricePerPound * df$totalPounds
  df$m <- df$month_num
  df$y <- as.numeric(df$year)
  
  df$resprice <- lm(pricePerPound ~ monthyear, data = df)$residuals
  p1 <- lm( resprice ~ type, data = df)
  food1 <- data.frame(type = levels(df$type))
  food1 <- cbind(food1,
                 predict(p1, food1, interval = 'confidence'))
  M <- mean(df$pricePerPound)
  food1[,2:4] <- food1[,2:4] + M
  food1[,2:4] <- food1[,2:4]/food1$fit[1]
  food1$type <- forcats::fct_reorder(food1$type, food1$fit)
  
  pricelin <- ggplot(food1, 
                     aes(type, fit)) +
    geom_col() + coord_flip() +
    geom_errorbar(aes(ymin = lwr, 
                      ymax = upr), width = 0) +
    theme_tanutama() +
    xlab('') +
    scale_fill_gradient(low = "grey45", high = "grey35") +
    scale_y_continuous(breaks = seq(-0.2,1.6,0.2)) +
    ylab('Estimated Price/Pound') +
    theme(legend.position = 'none')
  
  ggsave('figs/pdf/price-linear.pdf', pricelin,
         width = 7, height = 5.5)
  
  