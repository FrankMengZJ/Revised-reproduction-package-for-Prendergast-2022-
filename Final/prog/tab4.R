
  library(lfe)
  library(dplyr)
  library(xtable)
  library(stargazer)
  library(multcomp)
  
  df <- readRDS('data/bidding.rds') 
  auction <- readRDS('data/auction.rds')
  
  
  # Construct homogeneous goods
  if (T) {
    
    df$foodType <- as.character(df$type)
    
    # :: Index for food subtypes
    ffruit   <- grep('(A|a)pple|(B|b)an(|a)na|(C|c)antaloupe|(W|w)atermelon|(G|g)rape|(M|m)elon|(O|o)range|(P|p)each|(P|p)ear|(P|p)ineapple', 
                     df$description)
    fvegs    <- setdiff(which(df$type=='Produce'), ffruit)
    
    milk  <- grep('(\\s|^)(M|m)ilk', df$description)
    milk  <- setdiff(milk, which(df$type == 'Health Drinks/Bars'))
    yoghurt <- grep('(Y|y)og(h|)urt', df$description)
    yoghurt <- intersect(yoghurt, which(df$type == 'Dairy'))
    
    water <- grep('(W|w)ater\\s', df$description)
    water <- setdiff(water, grep('itness|(F|f)lav(ored|)|herry', df$description))
    water <- setdiff(water, grep('Life Water', df$description))
    water <- intersect(water, which(df$type %in% c('Other', 'Drinks')))
    sports <- grep('(G|g)atorade|(P|p)owerade|(G|g)atrade|(P|p)ropel', df$description)
    sports <- intersect(sports, which(df$type %in% c('Drinks')))
    
    saltysnacks <- intersect(grep('Snack', df$type),
                             grep('(S|s)nacks|(N|n)ut|(C|c)hip|(C|c)racker|(N|n)oodle|(P|p)opcorn|(P|p)ringles|(P|p)izza', df$description))
    
    # Code new foodtype
    df$foodType[which(df$type=='Produce')] <- 'Fresh Vegetables'
    df$foodType[ffruit] <- 'Fresh Fruits'
    
    df$foodType[which(df$type=='Dairy')] <- 'Dairy: Other'
    df$foodType[milk] <- 'Dairy: Milk'
    df$foodType[yoghurt] <- 'Dairy: Yoghurt'
    
    df$foodType[which(df$type=='Drinks')] <- 'Drinks: Other'
    df$foodType[water] <- 'Drinks: Water'
    df$foodType[sports] <- 'Drinks: Sports'
    
    df$foodType[which(df$type=='Snack')] <- 'Sweet Snacks'
    df$foodType[saltysnacks] <- 'Salty Snacks'
    
    df$type <- as.factor(df$foodType)
    
    # Categorize vegetables
    df$produce <- as.character(df$first)
    df$produce[which(df$type!='Fresh Vegetables')] <- ''
    df$produce <- gsub('and\\s', '& ', df$produce)
    df$produce <- gsub('\\/', ' & ', df$produce)
    df$produce <- gsub('comb(o$|\\sload$)', '', df$produce)
    df$produce <- gsub('(-|)combo load$', '', df$produce)
    df$produce <- gsub('(-\\s|\\s)no vap$', '', df$produce)
    df$produce <- gsub('-.*$', '', df$produce)
    df$produce <- trimws(gsub('fresh', '', df$produce), 'both')
    df$produce <- trimws(gsub('mix load', '', df$produce), 'both')
    df$produce <- trimws(gsub('combo(\\s|\\.)', '', df$produce), 'both')
    df$produce <- trimws(gsub('load$', '', df$produce), 'both')
    df$produce <- gsub('potatoe(s|)', 'potato', df$produce)
    df$produce <- gsub('cabbage(s|)', 'cabbage', df$produce)
    df$produce <- gsub('carrot(s|)', 'carrot', df$produce)
    df$produce <- gsub('onion(s|)', 'onion', df$produce)
    df$produce <- gsub('(produce assorted)|(assorted produce)', 'produce', df$produce)
    df$produce <- gsub('cucubmers', 'cucumbers', df$produce)
    df$produce <- gsub('ds waters \\& sparkletts crystal  water', 'water', df$produce)
    df$produce <- gsub('poatoes', 'potato', df$produce)
    df$produce <- gsub('no vap', 'produce', df$produce)
    df$produce <- gsub('potato 10# bags', 'potato', df$produce)
    df$produce <- gsub('vegetables', 'produce', df$produce)
    df$produce <- gsub('salad mixed', 'salad mixes', df$produce)
    df$produce[which(df$produce == 'salad mix')] <- 'salad mixes'
    df$produce <- gsub('\\s\\&$', '', df$produce)
    df$produce <- gsub('carrot \\& potato \\& onion \\& cabbage', 'carrot & cabbage & onion & potato', df$produce)
    
    # Edit food type
    water <- which(df$produce=='water')
    df$type <- as.character(df$type)
    df$type[water] <- 'Drinks: Water'
    df$produce[water] <- ''
    produce <- as.data.frame(table(df$produce))
    mainproduce <- c('cabbage', 'carrot', 'celery', 'corn',
                     'cucumbers', 'onion', 'potato',
                     'red cabbage', 'salad mixes', 'tomatoes')
    other <- which(!df$produce %in% mainproduce & df$produce != '')
    df$produce[other] <- 'assorted produce'
    
    
    # Edit Fruits
    df$type[grep('((J|j)uice)|(JUICE)', df$description)] <- 'Juice'
    ftype  <- c('apple', 'watermelon', 'cantaloupe', 'banana', 'grapefruit',
                'stonefruit', 'orange', 'honeydew', 'honeydew', 'pear', 
                'grape', 'pineapple', 'summer fruit',
                'stonefruit')
    fruits <- c('\\s(A|a)pples', '(W|w)atermelon', '(C|c)antaloupe', 
                '(B|b)anana', '(G|g)rapefruit', '(P|p)eaches', '(O|o)ranges', 
                '(H|h)oneydew', '\\s(M|m)elon', '(P|p)ears', '(G|g)rapes\\s', '(P|p)ineapple',
                '(S|s)ummer\\s(F|f)ruit', '(S|s)tone\\s(F|f)ruit')
    wf <- lapply(fruits, function(x) grep(x, df$description))
    for (i in 1:length(wf)) {
      df$produce[wf[[i]]] <- ftype[i]
    }
    
    # Turns out there are vegetables categorized as blanks. Fix
    prod <- c('(C|c)abbage', '(C|c)arrot', '(C|c)elery', '(C|c)orn',
              '(C|c)ucumbers', '(O|o)nion', '(P|p)otato',
              '(R|r)ed (C|c)abbage', '(S|s)alad\\s(M|m)ixes', '(T|t)omatoes')
    wf <- lapply(prod, function(x) intersect(grep(x, df$description),
                                             grep('(fresh)|(veg)', df$first)))
    for (i in 1:length(wf)) {
      df$produce[wf[[i]]] <- mainproduce[i]
    }
    df$type[unique(unlist(wf))] <- 'Fresh Vegetables'
    
    
    # Subset to homogenous - change name
    homog <- c('Fresh Fruits', 'Fresh Vegetables',
               'Drinks: Water', 'Drinks: Sports', 
               'Dairy: Yoghurt', 'Dairy: Milk')
    df$f <- as.factor(df$id)
    df$anysubsidy <- ifelse(!is.na(df$subsidy), 1, 0)
    df <- merge(df, auction[, c('auctionid', 'NBidder')], all.x = T)
    df$truckdist_km <- df$truckdist_km/1000
    df$totalPounds  <- df$totalPounds/1000000
    df$price <- df$pricePerPound * df$totalPounds * 1000
    
    
    # Some produce have blank names but whose types have been correctly identified
    prof <- c('(P|p)lums', '(C|c)ombo', '(C|c)a(gg|b)ag(d|e)', '(N|n)ectarine',
              '(P|p)umpkin', '(A|a)pple\\-(P|p)ear', '(S|s)trawberries',
              '(M|m)ixed\\s(C|c)itrus', '(Iceberg Lettuce)|(Lettues)', 
              '(P|p)eppers', '(T|t)angerines', '(S|s)quash', '(K|k)iwi',
              '(Z|z)ucchini', '(A|a)ppleas', '(C|c)ant(a|e)l(ou|o)pe', '(O|o)range',
              '(E|e)ggplant', '(P|p)oatoes', '(S|s)alads', '(Y|y)ams',
              '(O|o)n(ii|i)(o|oi)ns', '(B|b)annas')
    profnames <- c('stonefruit', 'combo load', 'cabbage', 'stonefruit',
                   'squash', 'apple', 'berries', 'orange', 'lettuce',
                   'peppers', 'orange', 'squash', 'kiwi', 'squash', 'apple',
                   'canteloupe', 'orange', 'eggplant', 'potato', 'salad mixes',
                   'sweet potato', 'onion', 'banana')
    wf <- lapply(prof, function(x)  grep(paste0('(F|f)resh.*', x), df$description))
    for (i in 1:length(wf)) {
      df$produce[wf[[i]]] <- profnames[i]
    }
    
    # Group assorted produce
    df$produce[which(df$produce == 'red cabbage')] <- 'cabbage'
    assor <- c('berries', 'celery', 'grape', 'kiwi', 'lettuce',
               'sweet potato')
    df$produce[which(df$produce %in% assor)] <- 'assorted produce'
    df$produce[grep('Potaotes', df$description)] <- 'potato'
    
    # other empty are nonveg nonfruits
    non <- which(df$produce=='' & df$type %in% c('Fresh Fruits', 'Fresh Vegetables'))
    df  <- df[-non,]
    df$produce[grep('trawberri', df$description)] <- 'summer fruit'
    
    
    # Finally clean!
    empty <- which(df$produce=='')
    df$produce[empty] <- gsub('^.*\\:\\s', '', df$type[empty])
    df$produce <- stringr::str_to_title(df$produce)
    
    fruits <- c('Apple', 'Banana', 'Canteloupe',
                'Grapefruit', 'Honeydew', 'Orange',
                'Pear', 'Pineapple', 'Stonefruit',
                'Summer Fruit', 'Watermelon')
    vegs <- c('Assorted Produce', 'Cabbage', 'Carrot',
              'Combo Load', 'Corn', 'Cucumbers', 'Eggplant',
              'Onion', 'Peppers', 'Potato', 'Salad Mixes',
              'Squash', 'Tomatoes')
    
    ff <- which(df$produce %in% fruits)
    df$produce[ff] <- paste0('Fruit: ', df$produce[ff])
    df$type[ff] <- 'Fresh Fruits'
    
    ff <- which(df$produce %in% vegs)
    df$produce[ff] <- paste0('Veg: ', df$produce[ff])
    df$type[ff] <- 'Fresh Vegetables'
    
    # Can subset now
    # Homogenous kicks in
    
    homog <- c('Fresh Fruits', 'Fresh Vegetables',
               'Drinks: Water', 'Drinks: Sports', 
               'Dairy: Yoghurt', 'Dairy: Milk')
    df$produce[which(df$type=='Drinks: Water')] <- 'Drinks: Water'
    df$produce[which(df$type=='Drinks: Sports')] <- 'Drinks: Sports'
    df$produce[which(df$type=='Dairy: Yoghurt')] <- 'Dairy: Yoghurt'
    df$produce[which(df$type=='Dairy: Milk')] <- 'Dairy: Milk'
    df$produce[which(df$type=='Dairy: Other')] <- 'Dairy: Other'
    df$produce[which(df$type=='Drinks: Other')] <- 'Drinks: Other'
    
    df$type[which(df$type=='Drinks: Water')] <- 'Drinks'
    df$type[which(df$type=='Drinks: Sports')] <- 'Drinks'
    df$type[which(df$type=='Dairy: Yoghurt')] <- 'Dairy'
    df$type[which(df$type=='Dairy: Milk')] <- 'Dairy'
    df$type[which(df$type=='Dairy: Other')] <- 'Dairy'
    df$type[which(df$type=='Drinks: Other')] <- 'Drinks'
    
    
    wh <- union(which(df$type %in% homog),
                which(df$produce %in% c('Drinks: Water', 'Drinks: Sports', 
                                        'Dairy: Yoghurt', 'Dairy: Milk')))
    df$homogenous <- 0
    df$homogenous[wh] <- 1
    
    df$ift <- with(df, paste(type, id, year, sep = '-'))
    df$IF  <- with(df, paste(type, id, sep = '-'))
    df$IT  <- with(df, paste(type, year, sep = '-'))
    df$FT  <- with(df, paste(id, year, sep = '-'))
    df$ifm <- with(df, paste(type, id, year, month_num %/% 2, sep = '-')) 
    
    dfall   <- df
    dfother <- df[-wh,]
    df <- df[wh,]
    
  }
  
  
  # Apples
  reg1 <- felm(price ~ totalPounds + anysubsidy + num_losingbids + truckdist_km  | ifm | 0 | id, 
               data = df[which(df$produce=='Fruit: Apple'),])
  
  # Homogeneous goods
  df$ift <- with(df, paste(produce, id, year, sep = '-'))
  df$IF  <- with(df, paste(produce, id, sep = '-'))
  df$IT  <- with(df, paste(produce, year, sep = '-'))
  df$FT  <- with(df, paste(id, year, sep = '-'))
  df$ifm <- with(df, paste(produce, id, year, month_num %/% 2, sep = '-')) 
  
  reg2 <- felm(price ~ totalPounds + anysubsidy + num_losingbids + truckdist_km  | ift | 0 | id, data = df)
  reg3 <- felm(price ~ totalPounds + anysubsidy + num_losingbids + truckdist_km  | ifm | 0 | id, data = df)
  
  # All, homogeneous goods interaction
  dfall$produce[which(dfall$homogenous==0)] <- ''
  reg4 <- felm(price ~ totalPounds + anysubsidy + num_losingbids + truckdist_km * homogenous - homogenous + produce | ift | 0 | id, data = dfall)
  reg5 <- felm(price ~ totalPounds + anysubsidy + num_losingbids + truckdist_km * homogenous - homogenous + produce | ifm | 0 | id, data = dfall)
  
  # Output table
  stargazer(reg1, reg2, reg3, reg4, reg5,
            omit.stat = 'ser',
            omit = c('truckdist\\_km\\:f\\d', '^f\\d', '^year\\d+', '^produce'),
            covariate.labels = c('Million Pounds',
                                 'Any Subsidy',
                                 '\\# Losing Bids',
                                 'Truck Distance',
                                 'Truck Distance $\\times$ Homogenous'))

  # Implied coefficients
  if (F) {
    getglht <- function(X, r) {
      
      coef  <- X$coefficients
      stde  <- X$sigma
      star  <- stars(X$pvalues)
      if (r == 1) {
        return(paste0(round(coef, 3), ' ', star))
      } else{
        return(paste0('(', round(stde, 3), ')'))
      }
      
    }
    
    lincoms <- 'truckdist_km + `truckdist_km:homogenous` == 0'
    models  <- lapply(list(reg4,reg5),
                      glht, lincoms)
    models.coef <- lapply(models, function(x) summary(x)$test)
    paste(unlist(lapply(models.coef, getglht, 1)), collapse = ' & ')
    paste(unlist(lapply(models.coef, getglht, 2)), collapse = ' & ')
    
  }
  
    