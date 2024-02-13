
  library(tidyverse)
  rm(list = ls())
  
  # Boots distance
  load("~/Final/sims-bounds/bankmeans-kround.RData")
  any.nas = which(apply(boots, 2,compose(any,is.na)))
  apply(boots, 2, compose(sum,is.na))
  
  boots$indices = indices
  boots = gather(boots, banks,
                 key = "bank", value = "km")
  thresh.seq    = unique(c(seq(0.75 , 0.9 , by=0.05),
                           seq(0.9  , 0.98, by=0.01),
                           seq(0.97 ,    1, by=0.005)))
  boots$threshold = thresh.seq[(boots$indices %%length(thresh.seq))+1]
  boots2 = boots %>% 
    group_by(bank,threshold) %>% 
    summarize(mean_dist = mean(km,na.rm=T))

  load("~/Final/sims/bank-quantiles.RData")
  colnames(all_quantiles)[1]   = "threshold"
  all_quantiles = gather(all_quantiles,
                         key   ="bank",
                         value ="Reject_dist",-1)
  boots2 = inner_join(boots2, all_quantiles)
  boots  = boots2 %>% 
    group_by(threshold) %>% 
    summarize(mean_dist = mean(mean_dist), 
              mean_reject_threshold = mean(Reject_dist))
  