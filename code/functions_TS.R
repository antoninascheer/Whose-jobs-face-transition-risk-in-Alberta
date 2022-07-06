
# 1 Multiple differences function definition

multiple_differences <- function(data = .,
                                 execute_on_variables = vars(everything()), 
                                 difference_vector = c(1)){
  intermediate <- tibble(data)
  for(i in difference_vector){
    data %>% 
      mutate_at(.vars = execute_on_variables, .funs = list(differenced = ~ . - dplyr::lag(x=.,n = i))) %>% 
      
      select(ends_with("_differenced")) %>% 
      
      # Just renaming the variables to D.variable
      rename_at(.vars = vars(everything()), .funs = ~paste0("D",i,".",gsub("_differenced","",.))) %>% 
      
      bind_cols(intermediate,.) -> intermediate
  }
  return(intermediate)
}







# 2 Multiple lag function definition

multiple_lags <- function(data = .,
                          execute_on_variables,
                          lag_vector = c(1)){
  intermediate <- tibble(data)
  for(i in lag_vector){
    if(i == 0){next}
    data %>% 
      # Lags - note we don't include the dependent variable because we want to regulate this using the ARX object
      #mutate_at(.vars = execute_on_variables, .funs = list(lagged = ~dplyr::lag(x=.,n = i))) %>% 
      mutate(across(.cols = any_of(execute_on_variables), .fns = list(lagged = ~dplyr::lag(x=.,n = i)))) %>% 
      
      select(ends_with("_lagged")) %>% 
      
      # Just renaming the variables to Li.variable
      rename_at(.vars = vars(everything()), .funs = ~paste0("L",i,".",gsub("_lagged","",.))) %>% 
      
      bind_cols(intermediate,.) -> intermediate
  }
  return(intermediate)
}







# 3 Keep vector function definition

find_keep_vector <- function(data = .,
                             fixed_effects = "month",
                             theory_variables = "WTI",
                             lags_in_adl = 2){
  keep_vector <- c(grep(fixed_effects,names(data)))
  
  if(!orthogonalize&no_selection_over_certain_variables){
    # Lagged Levels                        
    keep_vector <- append(keep_vector, c(which(names(data) %in% paste0("L1.",theory_variables))))
    # Simple Differences
    keep_vector <- append(keep_vector, c(which(names(data) %in% paste0("D1.",theory_variables))))
    
    # Further periods (we start at 2, because 1 is already included above)
    for(i in 2:lags_in_adl[lags_in_adl>1]){
      keep_vector <- append(keep_vector,c(which(names(data) %in% paste0("D1.L",i,".",theory_variables))))
    }
  }
  return(keep_vector)
}




est_fun_level <- function(data = .,
                    dep_var = "total",
                    theory_variables = c("WTI","discount"),
                    vars_to_estimate = c("all_goods_producing_industries_unadjusted","gas_price"),
                    fixed_effects = c("month"),
                    ar = 1,
                    lags_in_adl = 1:3,
                    indicator_saturation = TRUE,
                    iis = TRUE, 
                    sis = TRUE,
                    gets_selection = TRUE,
                    fix_theory = TRUE,
                    diagnostic_target_value = 0.01,
                    selection_target_value = 0.05,
                    indicator_target_value = 0.001,
                    tolerance = 1e-15) {
  
  data %>% 
    select(date,all_of(dep_var),any_of(c(theory_variables,vars_to_estimate))) %>% 
  
    # Lags
    {if(ar>0){
      multiple_lags(data = .,
                  execute_on_variables = dep_var,
                  lag_vector = ar)}else{.}} %>% 
    
    # Lags
    multiple_lags(data = .,
                  execute_on_variables = c(theory_variables,vars_to_estimate),
                  lag_vector = lags_in_adl) %>% 
  
    # Create Month Dummies
    mutate(month = month(date)) %>% 
    dummy_cols(select_columns = "month",remove_first_dummy = TRUE,remove_selected_columns = TRUE) %>% 
    
    drop_na -> intermed
  
  if(fixed_effects == "none"){intermed <- select(intermed,-contains("month"))}
  if(indicator_saturation){
    isat(y = intermed  %>% select(all_of(dep_var)) %>% pull,
         mxreg = intermed %>% select(-all_of(dep_var),-date) %>% as.matrix,
         t.pval = indicator_target_value,
         iis = iis,
         sis = sis,
         mc = T) -> isat_result
    
    #saturation
    impulses <- as.numeric(gsub("iis","",isat_result$ISnames[grepl("iis",isat_result$ISnames)])) # get the location of the impulses
    steps <- as.numeric(gsub("sis","",isat_result$ISnames[grepl("sis",isat_result$ISnames)])) # get the location of the steps
    
    df_isat <- intermed 
    if(!is_empty(impulses)){
      data.frame(iim(x = nrow(df_isat), which.ones = impulses)) %>% 
        setNames(paste0("iis_",impulses)) %>% 
        cbind(df_isat,.) -> df_isat}
    if(!is_empty(steps)){
      data.frame(sim(x = nrow(df_isat), which.ones = steps)) %>% 
        setNames(paste0("sis_",steps)) %>% 
        cbind(df_isat,.) -> df_isat}
    rm(impulses,steps)
    
    #plot(isat_result)
    
  } else {
    df_isat <- intermed
  }
  
  # Ensure that we don't get an ARX that is a singular matrix - this removes all 
  # Intercept yes or no?
  regr <- lm(data = df_isat %>% select(-date), 
             formula = as.formula(paste0(dep_var," ~ .")))
  data.frame(var = names(regr$coefficients),coef = regr$coefficients,row.names = NULL) %>%
    filter(!is.na(coef)) %>%
    filter(var !="(Intercept)") %>% 
    pull(var) -> select_over
  
  select_over <- gsub("`","",select_over)
  
  if(gets_selection){
    arx_model <- arx(y = df_isat  %>% select(all_of(dep_var)) %>% pull,
                     mxreg = df_isat %>% select(all_of(select_over)) %>% as.matrix,
                     mc=T,
                     tol=tolerance)
    
    keep_vector <- df_isat %>% 
      select(all_of(select_over)) %>% names
    
    keep_vector_numeric <- grep(fixed_effects,keep_vector)
    
    if(fix_theory){keep_vector_numeric <- append(keep_vector_numeric,grep(paste0(theory_variables,collapse ="|"),
                                                                          keep_vector))}
    
    
    
    selection <- getsm(object = arx_model, 
                       t.pval = selection_target_value,
                       
                       ar.LjungB = list(lag = NULL, pval = diagnostic_target_value),
                       arch.LjungB = list(lag = NULL, pval = diagnostic_target_value), 
                       normality.JarqueB = diagnostic_target_value, # Jarque and Bera (1980) test for non-normality
                       do.pet = TRUE, # Parsimonious Encompassing Test 
                       
                       tol = tolerance,
                       keep = keep_vector_numeric + 1, # +1 because of mc=T
                       plot = F)
    
    selection %>% coef %>% names -> selected_vars
    selected_vars <- selected_vars[!selected_vars %in% "mconst"]
    
    lm(df_isat %>% select(all_of(dep_var),all_of(selected_vars)),
       formula = as.formula(paste0(dep_var," ~ ."))) -> selection_lm
  
  } else {
    regr -> selection_lm
  }

  output <- list()
  output$dep_var <- dep_var
  output$data <- df_isat
  output$isat_model <- if(indicator_saturation){isat_result} else {NULL}
  output$gum <- regr
  output$gets_model <- if(gets_selection){selection} else {NULL}
  output$final_model <- selection_lm

  return(output)
}
