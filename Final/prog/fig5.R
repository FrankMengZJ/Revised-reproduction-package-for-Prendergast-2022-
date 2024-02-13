
  library(lfe)
  library(ggplot2)
  source('prog/theme_tanutama.R')
  d <- read.csv('data/bidding_posloads.csv')
  d$year <- as.factor(d$year)
  d$year <- relevel(d$year, ref = '2005')
  
  r <- felm(log(yellowPounds) ~ log(goalFactor) + year + log(goalFactor):year| 0 | 0 | id + year, d)
  co <- as.data.frame(summary(r)$coefficients[12:20,])
  co$Year <- as.numeric(gsub('\\D', '', rownames(co)))
  co$cih  <- co$Estimate + 1.96 * co$`Cluster s.e.`
  co$cil  <- co$Estimate - 1.96 * co$`Cluster s.e.`
  co[nrow(co)+1,] <- 0
  co$Year[nrow(co)] <- 2005
  
  # intercept
  i <- r$coefficients[2]
  co$Estimate <- co$Estimate + i
  co$cih <- co$cih + i
  co$cil <- co$cil + i
  
  p <- ggplot(co, aes(x = Year, y = Estimate)) +
    geom_hline(yintercept = i, color = 'firebrick4', lwd=1) +
    geom_errorbar(aes(x = Year,
                      y = Estimate,
                      ymin=cil, 
                      ymax=cih), width=0,
                  position='dodge2',
                  color = 'grey50') +
    geom_point(size = 2) +
    scale_y_continuous(breaks = seq(-0.3,1.1,0.1)) +
    scale_x_continuous(breaks = 2002:2011) +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.line.x= element_line(size=0.75),
          axis.text  = element_text(colour="gray50"),
          axis.text.y= element_text(size=10),
          axis.text.x= element_text(size=10),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  ggsave('figs/pdf/GF1.pdf', p,
         width = 5, height = 4)
    