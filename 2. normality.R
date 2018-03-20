setwd("G:/A WFP/variables selection and analysis")
library(nortest, pos=24)
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

# 
# obs$BirthYear<- as.numeric(obs$BirthYear)
# obs$HHSize <- as.numeric(obs$HHSize)
# obs$Staples <- as.numeric(obs$Staples)
# obs$Pulses <- as.numeric(obs$Pulses)
# obs$Veg <- as.numeric(obs$Veg)
# obs$Fruits <- as.numeric(obs$Fruits)
# obs$Proteins <- as.numeric(obs$Proteins)
# obs$Dairy <- as.numeric(obs$Dairy)
# obs$Sugars <- as.numeric(obs$Sugars)
# obs$Fats <- as.numeric(obs$Fats)
# obs$LessExpensiveFood <- as.numeric(obs$LessExpensiveFood)
# obs$BorrowOrHelp <- as.numeric(obs$BorrowOrHelp)
# obs$ReduceNumMeals <- as.numeric(obs$ReduceNumMeals)
# obs$LimitPortionSize <- as.numeric(obs$LimitPortionSize)
# obs$RestrictConsumption <- as.numeric(obs$RestrictConsumption)
# obs$FCS <- as.numeric(obs$FCS)
# obs$rCSI <- as.numeric(obs$rCSI)

# str(obs)
# summary(obs)

# # Lilliefors (Kolmogorov-Smirnov)
# library(nortest, pos=24)
# with(obs, lillie.test(FCS))
# with(obs, lillie.test(rCSI))
# # Anderson-Darling
# with(obs, ad.test(FCS))
# with(obs, ad.test(rCSI))
# # Cramer-Von Mises
# with(obs, cvm.test(FCS))
# with(obs, cvm.test(rCSI))


#############################################################
# FCS
split.screen(c(2, 2))
screen(1)
hist(obs$FCS,prob=TRUE)
lines(density(obs$FCS),col="blue",lwd=2)
norm <- rnorm(100000,mean=mean(obs$FCS),sd=sd(obs$FCS))
lines(density(norm),col="red",lwd=2)
norm <- norm[norm > 0]
cat ("\n Normality tests on FCS distribution (straight)")
shapiro.test(obs$FCS)
with(obs, lillie.test(FCS))
with(obs, ad.test(FCS))
with(obs, cvm.test(FCS))
screen(2)
qqplot(norm,obs$FCS)
abline(a=0,b=1,col="red",lwd=2)
#tit <- paste("Shapiro-Wilk test - W = ",round(shapiro.test(obs$FCS)$statistic,3))
tit <- paste("Kolmogorov-Smirnov test = ",round(lillie.test(obs$FCS)$statistic,3))
title(tit,cex.main=0.8)
screen(3)
qqplot(log(norm),log(obs$FCS))
abline(a=0,b=1,col="red",lwd=2)
#tit <- paste("Shapiro-Wilk test - W = ",round(shapiro.test(log(obs$FCS[log(obs$FCS)> -Inf]))$statistic,3))
tit <- paste("Kolmogorov-Smirnov test = ",round(lillie.test(log(obs$FCS[log(obs$FCS)> -Inf]))$statistic,3))
title(tit,cex.main=0.8)
cat ("\n Normality tests on FCS distribution (log)")
shapiro.test(log(obs$FCS))
with(obs, lillie.test(log(FCS)))
with(obs, ad.test(log(FCS)))
with(obs, cvm.test(log(FCS)))
screen(4)
qqplot(sqrt(norm),sqrt(obs$FCS))
abline(a=0,b=1,col="red",lwd=2)
#tit <- paste("Shapiro-Wilk test - W = ",round(shapiro.test(sqrt(obs$FCS))$statistic,3))
tit <- paste("Kolmogorov-Smirnov test = ",round(lillie.test(sqrt(obs$FCS))$statistic,3))
title(tit,cex.main=0.8)
cat ("\n Normality tests on FCS distribution (sqrt)")
shapiro.test(sqrt(obs$FCS))
with(obs, lillie.test(sqrt(FCS)))
with(obs, ad.test(sqrt(FCS)))
with(obs, cvm.test(sqrt(FCS)))
close.screen(all.screen=TRUE)
erase.screen()

#############################################################
# rCSI
split.screen(c(2, 2))
screen(1)
hist(obs$rCSI,prob=TRUE)
lines(density(obs$rCSI),col="blue",lwd=2)
norm <- rnorm(100000,mean=mean(obs$rCSI),sd=sd(obs$rCSI))
lines(density(norm),col="red",lwd=2)
norm <- norm[norm > 0]
cat ("\n Normality tests on rCSI distribution (straight)")
shapiro.test(obs$rCSI)
with(obs, lillie.test(rCSI))
with(obs, ad.test(rCSI))
with(obs, cvm.test(rCSI))
screen(2)
qqplot(norm,obs$rCSI)
abline(a=0,b=1,col="red",lwd=2)
#tit <- paste("Shapiro-Wilk test - W = ",round(shapiro.test(obs$rCSI)$statistic,3))
tit <- paste("Kolmogorov-Smirnov test = ",round(lillie.test(obs$rCSI)$statistic,3))
title(tit,cex.main=0.8)
screen(3)
qqplot(log(norm),log(obs$rCSI))
abline(a=0,b=1,col="red",lwd=2)
#tit <- paste("Shapiro-Wilk test - W = ",round(shapiro.test(log(obs$rCSI[log(obs$rCSI)> -Inf]))$statistic,3))
tit <- paste("Kolmogorov-Smirnov test = ",round(lillie.test(log(obs$rCSI[log(obs$rCSI)> -Inf]))$statistic,3))
title(tit,cex.main=0.8)
cat ("\n Normality tests on rCSI distribution (log)")
shapiro.test(log(obs$rCSI))
with(obs, lillie.test(log(rCSI[log(obs$rCSI)> -Inf])))
with(obs, ad.test(log(rCSI[log(obs$rCSI)> -Inf])))
with(obs, cvm.test(log(rCSI[log(obs$rCSI)> -Inf])))
screen(4)
qqplot(sqrt(norm),sqrt(obs$rCSI))
abline(a=0,b=1,col="red",lwd=2)
#tit <- paste("Shapiro-Wilk test - W = ",round(shapiro.test(sqrt(obs$rCSI))$statistic,3))
tit <- paste("Kolmogorov-Smirnov test = ",round(lillie.test(sqrt(obs$rCSI))$statistic,3))
title(tit,cex.main=0.8)
cat ("\n Normality tests on rCSI distribution (sqrt)")
shapiro.test(sqrt(obs$rCSI))
with(obs, lillie.test(sqrt(rCSI)))
with(obs, ad.test(sqrt(rCSI)))
with(obs, cvm.test(sqrt(rCSI)))
close.screen(all.screen=TRUE)
erase.screen()


