EstimHypParam <- function(GridParam, imput = F){
  #-----------------------------------------
  # Param:
  # GridParam = Data.frame
  # Function computes proper grid search based on TuneGrid passed as parameter (Gridparam) 
  # Gridparam must be a dataframe containnig all necessary column for parameters and a column "method" indicating the method for  which its used
  #
  #------------------------------------------
  output <-  list()
  
  if(imput == T){
  data <- GetImputedKNN(data)
  }
 
    for(i in 1:length(Y)){
  
    y      <- Y[i]
    d      <- D[i]
    
    datause   <- data.frame(data[complete.cases(data[, c(controls, y, d, affected, baseline)]),]) %>% 
      mutate_all(~as.numeric(.x))

    ind_u <- which(datause[,d]==1)         # treatment indicator
    ind_nu <- which(datause[,baseline]==1)
      
      for(l in 1:length(methods)){
      
      x         <- X
      form           <- as.formula(paste(y,"~",x,sep=""));
      
      ############ Estimate Scores using ML ############
      
      TuneGrid <- GridParam %>% 
        filter(method == methods[l]) %>% 
        dplyr::select(-method)
      
      TuneGrid <- TuneGrid[, !colSums(is.na(TuneGrid)), drop = FALSE]
      
      fitControl   <- trainControl(method = "cv", 
                                   number = 3, 
                                   repeats = NA, 
                                   allowParallel = TRUE, 
                                   verboseIter=TRUE, 
                                   #search="random", 
                                   selectionFunction="best")
      
      arg          <- c(list(form=form, 
                             data = datause[ind_u,],  
                             method = methods[l],  
                             tuneGrid = TuneGrid, 
                             trControl = fitControl, 
                             preProcess= "range", 
                             tuneLength= NA), 
                        verbose = TRUE)
      if(methods[l] %in% c("xgbTree")){
        arg          <- c(list(form=form, 
                               data = datause[ind_u,],  
                               method = methods[l],  
                               tuneGrid = TuneGrid, 
                               trControl = fitControl, 
                               preProcess= "range", 
                               tuneLength= NA, 
                               nthread=1), 
                          verbose = TRUE)
        
      }
      
      fit.yz1      <- suppressWarnings(do.call(caret::train, arg))
      
      
      tunegrid_1 <- fit.yz1$bestTune
      write_csv(fit.yz1$results %>% mutate(method = methods[l], Y = Y[i], D = D[i], type = 1), "tuning.csv", append = TRUE)
      
      
      fitControl   <- trainControl(method = "cv", 
                                   number = 3, 
                                   repeats = NA, 
                                   allowParallel = TRUE, 
                                   verboseIter=TRUE, 
                                   search="random", 
                                   selectionFunction="best")
      
      arg          <- c(list(form=form, 
                             data = datause[ind_nu,],  
                             method = methods[l],  
                             tuneGrid = TuneGrid, 
                             trControl = fitControl, 
                             preProcess= "range", 
                             tuneLength= NA), 
                        verbose = TRUE)
      
      if(methods[l]  %in% c("xgbTree")){
        arg          <- c(list(form=form, 
                               data = datause[ind_nu,],  
                               method = methods[l],  
                               tuneGrid = TuneGrid, 
                               trControl = fitControl, 
                               preProcess= "range", 
                               tuneLength= NA, 
                               nthread=1), 
                          verbose = TRUE)
        
      }
      
      
      fit.yz0      <- suppressWarnings(do.call(caret::train, arg))
      tunegrid_0 <- fit.yz0$bestTune
    
      write_csv(fit.yz0$results %>% mutate(method = methods[l], Y = Y[i], D = D[i], type = 0), "tuning.csv", append = TRUE)
      output[paste(methods[l], Y[i], D[i], sep = "_")] = list(list(tune0 = tunegrid_0, tune1 = tunegrid_1))
      output[paste(methods[l], Y[i], D[i], sep = "_")][[1]][["tune1"]]
      }
    }
    
  return(output)
}


GetImputedKNN <- function(data){
  preProcValues <- preProcess(x = data, method = c("knnImpute"))
  training <- predict(preProcValues, data)
  return(training)
} 
