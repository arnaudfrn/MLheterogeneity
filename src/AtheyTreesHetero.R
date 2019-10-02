## Script to compute empirically the ATE on our dataset accounting for uncertainty in random splits ##

library(haven)
library(dplyr)
library(caret)
library(grf)
library(caret)
library(dummies)
library(readr)


data_b <- read_dta("/home/jovyan/MLheterogeneity/Data/data_ML_nonmiss_0626.dta")
data_b <- as.data.frame(data_b) %>% 
  dplyr::select(-a7, -bribe_combined, -salongo, -salongo_hours, -sanctions, -pubgoods) %>% dummy.data.frame( names = "tmt_ML") %>% 
  rename(tmt_100 = tmt_ML0 , 
         tmt_83 = tmt_ML1,
         tmt_66 = tmt_ML2,
         tmt_50 = tmt_ML3) %>% 
  filter( tmt_50 == 1 | tmt_100 == 1) %>% 
  mutate_all(~as.numeric(.)) 

X <- data_b %>% dplyr::select(-tmt_100, -tmt_50,-taxes_paid) %>% as.matrix.data.frame()
Y <- data_b %>% dplyr::select(taxes_paid) %>% as.vector()
W <- data_b %>% dplyr::select(tmt_50) 


#building for loop

tau.hat.full <- c()
ATE.full <- c()
ATE.error.full <- c()

for (i in (1:100))
{
  paste('running ', i, ' on 100')
  # Predict Y forest, Classification of X on Y with probabililties as output
  Y.forest = regression_forest(X[,],Y[,], tune.parameters = TRUE,  num.threads = 6, seed = i)
  Y.hat = predict(Y.forest)$predictions
  
  # Prediction W Forest, Regression of X on W
  W.forest = regression_forest(X[,],W[,], tune.parameters = TRUE,  num.threads = 6, seed = i)
  W.hat = predict(W.forest)$predictions
  
  #train causal forest
  cf.raw = causal_forest(X[,], Y[,], W[,], Y.hat = Y.hat , W.hat = W.hat, num.threads = 6, seed = i)
  varimp = variable_importance(cf.raw)
  
  selected.idx = which(varimp  > mean(varimp ))
  cf = causal_forest(X[,selected.idx], Y[,], W[,], Y.hat = Y.hat , W.hat = W.hat, tune.parameters = TRUE, num.threads = 6, seed = i)
  
  tau.hat = predict(cf)$prediction
  ATE <- average_treatment_effect(cf)
  
  ATE.full <- c(ATE.full, unname(ATE[1]))
  ATE.error.full <- c(ATE.error.full, unname(ATE[2]))
  tau.hat.full = c(tau.hat.full, tau.hat)
}

write_csv(ATE.full, "ATEfull.csv")
write_csv(ATE.error.full, "ATE_error_full.csv")
write_csv(tau.hat.full, "tau_hat_full.csv")


