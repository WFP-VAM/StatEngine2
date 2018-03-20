setwd("G:/A WFP/variables selection and analysis")
options(warn=-1)
# load the libraries
library(mlbench)
library(bnlearn)
#install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
library(class)
library(gam)
# load the data
load("lessData.RData")
############ Remove Redundant Features ################
variables <- c(
               "ADM1_NAME",
               "BirthYear",
               "Gender",
               "Region",
               "HoHSex",
               "HHSize",
               "IDP_YN",
               "HouseType",
               "Staples",
               "Pulses",
               "Veg",
               "Fruits",
               "Proteins",
               "Dairy",
               "Sugars",
               "Fats",
               "LessExpensiveFood",
               "BorrowOrHelp",
               "ReduceNumMeals",
               "LimitPortionSize",
               "RestrictConsumption",
               "FoodAssistance_YN",
               "Form_FoodAssistance",
               "FCS",
               "rCSI")
obs <- obsDf[, variables]
obs <- obs[complete.cases(obs),]
#???str(obs)
#summary(obs)

# 
obs$BirthYear<- as.numeric(obs$BirthYear)
obs$HHSize <- as.numeric(obs$HHSize)
obs$Staples <- as.numeric(obs$Staples)
obs$Pulses <- as.numeric(obs$Pulses)
obs$Veg <- as.numeric(obs$Veg)
obs$Fruits <- as.numeric(obs$Fruits)
obs$Proteins <- as.numeric(obs$Proteins)
obs$Dairy <- as.numeric(obs$Dairy)
obs$Sugars <- as.numeric(obs$Sugars)
obs$Fats <- as.numeric(obs$Fats)
obs$LessExpensiveFood <- as.numeric(obs$LessExpensiveFood)
obs$BorrowOrHelp <- as.numeric(obs$BorrowOrHelp)
obs$ReduceNumMeals <- as.numeric(obs$ReduceNumMeals)
obs$LimitPortionSize <- as.numeric(obs$LimitPortionSize)
obs$RestrictConsumption <- as.numeric(obs$RestrictConsumption)
obs$FCS <- as.numeric(obs$FCS)
obs$rCSI <- as.numeric(obs$rCSI)

#correlationMatrix <- cor(obs[,c(9:21,24:25)])
# summarize the correlation matrix
#correlationMatrix





# ########### Features By Bayesian Network and Importance ##################
# rCSI
res <- hc(obs[,c(1:16,24:25)])
plot(res)
fittedbn_rCSI <- bn.fit(res, data = obs[,c(1:16,24:25)])
print(fittedbn_rCSI$rCSI)

# # Random Forest for rCSI
library(randomForest)
rf <- randomForest(rCSI~., data=obs[,c(1:16,24:25)], ntree=100, importance=TRUE)
importance_rCSI <- as.data.frame(rf$importance)
importance_rCSI[order(importance_rCSI$`%IncMSE`,decreasing=TRUE),]
fittedbn_rCSI$rCSI

# # Recursive Feature Selection (RFE)
# controlRFE <- rfeControl(functions=rfFuncs, method="cv", number=3)
# RFE_rCSI <- rfe(x=obs[,c(1:16,24)], y=obs[,c(25)], rfeControl=controlRFE)
# RFE_rCSI
# predictors(RFE_rCSI)
# #plot(results)
# ############### Univariate filters #######################
# filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
# set.seed(10)
# rfWithFilter_rCSI <- sbf(obs[,c(1:16,24)], obs[,c(25)], sbfControl = filterCtrl)
# rfWithFilter_rCSI
# summary(rfWithFilter_rCSI)
# rfWithFilter_rCSI$optVariables
# #obs1 <- obs[,c(rfWithFilter_rCSI$optVariables)]
# # save.image(file="vars.RData")


# ########### Features By Bayesian Network and Importance ##################
# FCS

res <- hc(obs[,c(1:8,17:25)])
plot(res)
fittedbn_FCS <- bn.fit(res, data = obs[,c(1:8,17:25)])
print(fittedbn_FCS$FCS)

# # Random Forest for FCS
library(randomForest)
rf <- randomForest(FCS~., data=obs[,c(1:8,17:25)], ntree=100, importance=TRUE)
importance_FCS <- as.data.frame(rf$importance)
importance_FCS[order(importance_FCS$`%IncMSE`,decreasing=TRUE),]
fittedbn_FCS$FCS

# # Recursive Feature Selection (RFE)
# controlRFE <- rfeControl(functions=rfFuncs, method="cv", number=3)
# RFE_FCS <- rfe(x=obs[,c(1:8,17:23,25)], y=obs[,c(24)], rfeControl=controlRFE)
# RFE_FCS
# predictors(RFE_FCS)
# #plot(results)
# ############### Univariate filters #######################
# filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
# set.seed(10)
# rfWithFilter_FCS <- sbf(obs[,c(1:8,17:23,25)], obs[,c(24)], sbfControl = filterCtrl)
# rfWithFilter_FCS
# summary(rfWithFilter_FCS)
# rfWithFilter_FCS$optVariables
# #obs1 <- obs[,c(rfWithFilter_FCS$optVariables)]
# # save.image(file="vars.RData")

# ############### Genetic Algorithm using Random Forests ########################
# # http://blog.revolutionanalytics.com/2015/12/caret-genetic.html
# library(doParallel)
# registerDoParallel(2) # Registrer a parallel backend for train
# getDoParWorkers() # check that there are 4 workers
# 
# ga_ctrl <- gafsControl(functions = rfGA, # Assess fitness with RF
#                        method = "boot",    
#                        number = 3,
#                        genParallel=TRUE, # Use parallel programming
#                        allowParallel = TRUE)
# ## 
# set.seed(10)
# system.time(rf_ga <- gafs(x = obs1, y = obs[,c(25)],
#                            iters = 10,
#                            popSize = 5,
#                            gafsControl = ga_ctrl))
# rf_ga
# plot(rf_ga) + theme_bw()
# save.image(file="vars.RData")
# # ga_ctrl <- gafsControl(functions = rfGA, method="repeatedcv", repeats=1)
# # set.seed(10)
# # rf_GA <- gafs(x = obs1, 
# #                  y = obs[,c(25)],
# #                      iters = 10,
# #                      gafsControl = ga_ctrl)
# # rf_GA
# # plot(rf_GA) + theme_bw()
# # 
# 
# ############### Simulated Annealing using Random Forests #######################
# sa_ctrl <- safsControl(functions = rfSA, method="repeatedcv", repeats=2)
# set.seed(10)
# rf_SA <- gafs(x = obs[,c(1:16)], 
#               y = obs[,c(25)],
#               iters = 10,
#               safsControl = sa_ctrl)
# rf_SA
# plot(rf_SA) + theme_bw()
# 

