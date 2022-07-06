vecm_fun <- function(
  name = "",
  df,
  order = c("WTI", "emp_total", "discount", "total_oil"),
  VARselect_type = "both",
  season = NULL,
  spec = "longrun",
  ecdet = "trend",
  cajotesttype = "trace",
  cajotestpval = "1pct",
  n.ahead = 12){
  
  #browser()
  
  if(!all(order %in% names(df))){stop("Not all variables in the order argument are in the supplied data. Please check the column names.")}
  if(!is.numeric(season)){stop("Season must be a numeric variable. Use 0 to indicate no seasonal dynamics in VARselect.")}
  if(season==0){season <- NULL}
  
  
  out <- list()
  
  
  ## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  series <- ts(df[-1],start = 1976,frequency = 12)
  series <- na.trim(series)
  series <- series[,order]
  #plot(series)
  
  out$name <- name
  out$indata <- series
  
  
  ## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #VARselect(series)
  
  lagselection <- VARselect(series,type = VARselect_type,season = season)
  K = max(2,lagselection$selection["SC(n)"])

  #VARselect(series,type = "const")
  #VARselect(series,type = "trend")
  
  
  ## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  cajo <- ca.jo(series, type = cajotesttype, ecdet = ecdet, K = K, season = season,spec = spec)
  #summary(cajo)
  
  r <- nrow(cajo@cval) - max(which(cajo@cval[,cajotestpval] > cajo@teststat))
  
  ## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  rrr <- cajorls(cajo,r = r)
  
  
  ## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  H <- rrr$beta
  #H <- matrix(rep(1,10),c(5,2))
  #H[5,2] <- 0
  #H[4,1] <- 0
  #H[1,2] <- 0
  H[2,1] <- 0
  #H[2,2] <- 1
  
  
  ## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  betatest <- blrtest(cajo,H=H,r=r)
  
  A <- betatest@W
  A[1,] <- 0
  
  
  ## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  abtest <- ablrtest(cajo,H = H,A = A,r = r)
  
  
  ## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  var <- vec2var(cajo,r = r)
  
  
  ## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Obtain IRF
  irfs <- irf(var,n.ahead = n.ahead,runs = 500,ortho = TRUE,cumulative = F)

  out$betatest <- betatest
  out$A <- A
  out$H <- H
  out$cajo <- cajo
  out$abtest <- abtest
  out$var <- var
  out$irfs <- irfs
  
  return(out)
  
}



extract_irf <- function(vecm_list,impulse,variable){
  if(length(vecm_list)==9){warning("Make sure to add more than one vecm_fun object - this will fail if you only add one. If you have added 9, then you will also receive this warning, which you can then ignore.")}
  out <- tibble()
  for (i in seq_along(vecm_list)){
    #browser()
    #print(i)
    modelname <- vecm_list[[i]] %>% with(name)
    
    vecm_list[[i]] %>% 
      with(irfs) %>% 
      with(irf) %>% 
      with(get(impulse)) %>% 
      as_tibble() %>% 
      select(all_of(variable)) %>% 
      mutate(model = modelname) %>% 
      relocate(model) -> out_mean
    
    vecm_list[[i]] %>% 
      with(irfs) %>% 
      with(Lower) %>% 
      with(get(impulse)) %>% 
      as_tibble() %>% 
      select(all_of(variable)) %>% 
      rename_with(.cols = all_of(variable),.fn = ~paste0(variable,"_lower")) -> out_lower
    
    
    vecm_list[[i]] %>% 
      with(irfs) %>% 
      with(Upper) %>% 
      with(get(impulse)) %>% 
      as_tibble() %>% 
      select(all_of(variable)) %>% 
      rename_with(.cols = all_of(variable),.fn = ~paste0(variable,"_upper")) -> out_upper
    
    out_mean %>% 
      bind_cols(.,out_lower) %>% 
      bind_cols(.,out_upper) %>% 
      bind_rows(out,.)-> out
  }
  
  return(out)
}