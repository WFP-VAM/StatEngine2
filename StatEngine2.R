setwd('C:\\Users\\gaurav.singhal\\Code\\StatEngine\\RScripts')
################################LOAD NECESSARY MODULES################################
library(survey)
library(plyr)
library(phia)
library(RColorBrewer)

if(!exists("createSvyDsgn", mode="function")) source("createSvyDsgn.R")
if(!exists("conditionVars", mode="function")) source("conditionVars.R")
if(!exists("calcSumStats", mode="function")) source("calcSumStats.R")
if(!exists("calcANOVA", mode="function")) source("calcANOVA.R")
if(!exists("calcMultCmp", mode="function")) source("calcMultCmp.R")
if(!exists("replaceVarNames", mode="function")) source("replaceVarNames.R")
if(!exists("plotSumStats", mode="function")) source("plotSumStats.R")

#Determine Type of variable (binary, continuous, discrete) 
#This need not be in R and should be determined at time when dependent and independent variables are chosen
evalVarTypes <- function(svyDsgnObj){
  
  #Helper function determines if discrete
  isDiscrete <- function(VarVec){
    return( is.factor(VarVec) || is.logical(VarVec) || length(unique(VarVec))<5 )
  }
  
  
  if (isDiscrete(svyDsgnObj$variables$depVar)){
    if(length(unique(svyDsgnObj$variables$depVar))==2){
      depVarType <- 'Bin'
    } else {
      depVarType <- 'Dsc'
    }
  } else {
    depVarType <- 'Cnt'
  }
  
  if (isDiscrete(svyDsgnObj$variables$indVar1)){
    indVar1Type <- 'Dsc'
  } else {
    indVar1Type <- 'Cnt'  
  }
  
  if (!all(is.na(svyDsgnObj$variables$indVar2))){#2 indepedent variables
    if (isDiscrete(svyDsgnObj$variables$indVar2)){
      indVar2Type <- 'Dsc'
    } else {
      indVar2Type <- 'Cnt'
    }
  } else {
    indVar2Type <- NA
  }
  
  return(c(depVarType,indVar1Type,indVar2Type))
}

#function to take parameter inputs from GUI and create human-understandable variable name
#This need not be in R and should be determined at time when dependent and independent variables are chosen
varNameConstructor <- function(prmList){
  rplStr <- prmList[1]
  if(!is.na(prmList[2]) && !is.na(prmList[3])){
    rplStr <- paste(rplStr,prmList[2],prmList[3],sep='')
  } else if (!is.na(prmList[4]) && prmList[4]=='TRUE'){
    rplStr <- paste('fctr[',rplStr,']',sep='')
  } else if (!is.na(prmList[5]) && prmList[5]=='TRUE'){
    rplStr <- paste('log[',rplStr,']',sep='')
  }
  return(rplStr)
}

################################LOAD NECESSARY DATA################################

#Create object to hold input data from DB
inputDB = list(svyID = NA, #ID of survey you would like to load
               splWghtCol = c(NA), #base population weight column
               adjWghtCols = c(NA), #columns to multiply base weight by to adjust for different selection biases
               PPSYN = FALSE, #Was sampling with probability proportional to size
               smplWRplcYN = FALSE, #Was sampling with or without replacement
               rptRspYN = TRUE, #Are respondents repeated (this may need to be modified for rotation panels)
               splUnitCols = c('RspID'), #Sampling Units in order of largest to smallest (if two stage cluster, first item would be cluster ID)
               strUnitCols = c(NA), #Stratification Variables for above sampling units
               fpcUnitCols = c(NA), #Finite population variables for above sampling units
               obsDf = data.frame(), #Observations to be loaded for survey
               aggDf = data.frame(), #Separate table holding administrative area groupings that we may want to analyze
               popDf = data.frame(), #Seperate table with population levels for weights or FPC
               postStrYN = TRUE,
               postStrDf = data.frame(), #Table for post-stratification information
               calbrYN = TRUE,
               calbrTbl = list(NA),
               calbrCfg = list(calfun ='logit', #calibration link function
                               bounds = c(0.125,8), #bounds for estimation
                               trim = c(0.5,2.5)) #Trim values if weights still outside of bounds
              )

#These will all be Mongo DB queries in the future
library(RODBC)
channel <- odbcConnect("mvam", uid="mvamuser", pwd="*********", believeNRows=FALSE)

#Initial SELECTION
inputDB$svyID <- 163
#INPUTS FROM DATABASE

#(1) First Input is dataframe containing cleaned observations (Respondent|Fields 1...N)
inputDB$obsDf  <- sqlQuery(channel,paste('SELECT * FROM Obs_YmnCnfl WHERe SvyID=',inputDB$svyID,sep=''))

#Ignore below lines, simply to clean up ObsDf
inputDB$obsDf$HHSize[inputDB$obsDf$HHSize>9] <- 9
inputDB$obsDf$HouseTypeGrp <- as.factor(sapply(inputDB$obsDf$HouseTypeGrp, gsub, pattern = "Staying_with_someone_for_free", replacement = "Guest", fixed = TRUE))
#rowsHHSize1 <- as.numeric(row.names(obsDf[obsDf$HHSize==1,]))
#obsDf[rowsHHSize1,'HHsize'] <- 2

#(2) Second Input (if necessary) is dataframe inidcating which administrative areas have been grouped together (not necessary here)
#obsDf$AggDf = Dataframe structured as so [ADM1_NAME, ADM2_NAME, ADM3_NAME, Aggregation] (ADM2 and 3 are optional if unused)

#(3) Third input is population information for sampling Strata (in this case it is ADM1_NAME)
inputDB$popDf <- sqlQuery(channel,paste('SELECT ADM1_NAME, SUM(StrPop) AS StrPop FROM
                                        (SELECT DISTINCT STR1_NAME AS ADM1_NAME, STR2_NAME AS IDP_YN,
                                          CASE WHEN AggCol=1 THEN ISNULL(StrPop,Population1)
                                               WHEN AggCol=2 THEN ISNULL(StrPop,Population1)
                                               WHEN AggCol=3 THEN ISNULL(StrPop,Population1)
                                               WHEN AggCol=4 THEN ISNULL(StrPop,Population1) END AS StrPop,
                                          CASE WHEN AggCol=1 THEN Aggregation1
                                               WHEN AggCol=2 THEN Aggregation2
                                               WHEN AggCol=3 THEN Aggregation3
                                               WHEN AggCol=4 THEN Aggregation4 END AS AdmStrata
                                          FROM
                                          (SELECT Strata.*,AggCol FROM Strata 
                                            JOIN SVy on Svy.Pnlid=Strata.pnlID WHERE SvyID=',inputDB$svyID ,') T0 )T1
                                        GROUP BY ADM1_NAME, AdmStrata',sep=''))

#(4) Fourth input is population level information for fields we would like to post-stratify on (in this case  IDP_YN)
#Note that the postStrDf must also be broken down by the fields we stratified on
#That is post-startification occurrs on subsets within the initial stratification sets
inputDB$postStrDf <- sqlQuery(channel,paste('SELECT DISTINCT STR1_NAME AS ADM1_NAME, STR2_NAME AS IDP_YN,
                                            CASE WHEN AggCol=1 THEN ISNULL(StrPop,Population1)
                                                 WHEN AggCol=2 THEN ISNULL(StrPop,Population2)
                                                 WHEN AggCol=3 THEN ISNULL(StrPop,Population3)
                                                 WHEN AggCol=4 THEN ISNULL(StrPop,Population4) END AS StrPop
                                            FROM
                                            (SELECT Strata.*,AggCol FROM Strata 
                                              JOIN SVy on Svy.Pnlid=Strata.pnlID WHERE SvyID=',inputDB$svyID ,') T0'
                                          ,sep=''))
#Normalize
inputDB$postStrDf$StrPop <- inputDB$postStrDf$StrPop/sum(inputDB$postStrDf$StrPop)

#(5) Fifth input are the calibration dataframes
#Note that calibration dataframes contain joint or maginal probabilities of the categories belonging to a field
#In this case the marginal distributions are HoHSex with categories M,F and HHSize with categories 1-9
inputDB$calbrTbl[[1]] <- data.frame(HoHSex = c('M','F'), Pct = c(0.922,0.078))
inputDB$calbrTbl[[2]] <- data.frame(HHSize = c(1,2,3,4,5,6,7,8,9), Pct = c(0.02,0.03,0.12,0.1,0.12,0.12,0.12,0.12,0.25))
#These would also be inputs from database but currently not present in DB

#(6) These would also be parameter inputs from database but currently not present in DB
inputDB$strUnitCols <- c('ADM1_NAME')
inputDB$adjustWghtCols <- c('SelectWt')
inputDB$fpcUnitCols <- c('StrPop')

################################CREATE SVYDESIGN OBJECT################################

#(1) Create Survey Design Object 
svyDsgnObj <- createSvyDsgn(inputDB)

#(2) Diagnostics on Weights (would ideally be displayed at time of original upload)
# It is possible that weights will fail to converge in step (3)
# Hence user should be able to view failure message and view some diagnostics
summary(weights(svyDsgnObj))

####################CONDITION SVYDESIGN OBJECT GIVEN INPUTS FROM GUI###################

#(0) INPUTS FROM GUI --> Parameters in vector are as follows:
#(A) Name of field (in ObsDf)
#(B) Threshold function (<,>,<=,>=,==)
#(C) Threshold value/comparator value
#(D) Force to be categorical (Yes/No)
#(E) Log transform variable is continuous (Yes/No) (For continuous variables only)
inputGUI <- list(depVarPrms = rep(NA,6),indVar1Prms = rep(NA,6),indVar2Prms = rep(NA,6),varTypes = rep(NA,3))

#Scenario 1: Cnt ~ Cnt+Cnt -> log(FCS) ~ rCSI+HHSize
inputGUI$depVarPrms[1:5] <- c('FCS',NA,NA,'FALSE','TRUE')
inputGUI$indVar1Prms[1:5] <- c('rCSI',NA,NA,'FALSE','FALSE')
inputGUI$indVar2Prms[1:5] <- c('HHSize',NA,NA,'FALSE','FALSE')

#Scenario 2: Cnt ~ Cnt+Dsc -> log(FCS) ~ rCSI+HouseTypeGrp 
inputGUI$depVarPrms[1:5] <- c('FCS',NA,NA,'FALSE','TRUE')
inputGUI$indVar1Prms[1:5] <- c('rCSI',NA,NA,'FALSE','FALSE')
inputGUI$indVar2Prms[1:5] <- c('HouseTypeGrp',NA,NA,'FALSE',NA)

#Scenario 3: Cnt ~ Dsc+Dsc -> log(FCS) ~ IDP_YN+ADM1_NAME
inputGUI$depVarPrms[1:5] <- c('FCS',NA,NA,'FALSE','TRUE')
inputGUI$indVar1Prms[1:5] <- c('IDP_YN',NA,NA,'FALSE',NA)
inputGUI$indVar2Prms[1:5] <- c('ADM1_NAME',NA,NA,'FALSE',NA)

#Scenario 4: Dsc ~ Cnt+Cnt -> FCG ~ rCSI+HHSize (Currently does not return ANOVA must implement multinomial model)
inputGUI$depVarPrms[1:5] <- c('FCG',NA,NA,'TRUE',NA)
inputGUI$indVar1Prms[1:5] <- c('rCSI',NA,NA,'FALSE','FALSE')
inputGUI$indVar2Prms[1:5] <- c('HHSize',NA,NA,'FALSE','FALSE')

#Scenario 5: Dsc ~ Cnt+Dsc -> FCG ~ rCSI+HouseTypeGrp (Currently does not return ANOVA must implement multinomial model)
inputGUI$depVarPrms[1:5] <- c('FCG',NA,NA,'TRUE',NA)
inputGUI$indVar1Prms[1:5] <- c('rCSI',NA,NA,'FALSE','FALSE')
inputGUI$indVar2Prms[1:5] <- c('HouseTypeGrp',NA,NA,'FALSE',NA)

#Scenario 6: Dsc ~ Dsc+Dsc -> FCG ~ IDP_YN+ADM1_NAME
inputGUI$depVarPrms[1:5] <- c('FCG',NA,NA,'TRUE',NA)
inputGUI$indVar1Prms[1:5] <- c('IDP_YN',NA,NA,'FALSE',NA)
inputGUI$indVar2Prms[1:5] <- c('ADM1_NAME',NA,NA,'FALSE',NA)

#Scenario 7: Bin ~ Cnt+Cnt -> FoodAssistance_YN ~ rCSI+HHSize
inputGUI$depVarPrms[1:5] <- c('FoodAssistance_YN',NA,NA,'FALSE',NA)
inputGUI$indVar1Prms[1:5] <- c('rCSI',NA,NA,'FALSE','FALSE')
inputGUI$indVar2Prms[1:5] <- c('HHSize',NA,NA,'FALSE','FALSE')

#Scenario 8: Bin ~ Cnt+Dsc -> FCS>=32 ~ rCSI+HouseTypeGrp
inputGUI$depVarPrms[1:5] <- c('FCS','>=',32,'FALSE',NA)
inputGUI$indVar1Prms[1:5] <- c('rCSI',NA,NA,'FALSE','FALSE')
inputGUI$indVar2Prms[1:5] <- c('HouseTypeGrp',NA,NA,'FALSE',NA)

#Scenario 9: Bin ~ Dsc+Dsc -> rCSI>=10 ~ IDP_YN+ADM1_NAME
inputGUI$depVarPrms[1:5] <- c('rCSI','>=',10,'FALSE',NA)
inputGUI$indVar1Prms[1:5] <- c('IDP_YN',NA,NA,'FALSE',NA)
inputGUI$indVar2Prms[1:5] <- c('ADM1_NAME',NA,NA,'FALSE',NA)

#Scenario 10: Cnt ~ Cnt -> log(FCS) ~ rCSI
inputGUI <- list(depVarPrms = rep(NA,6),indVar1Prms = rep(NA,6),indVar2Prms = rep(NA,6),varTypes = rep(NA,3))
inputGUI$depVarPrms[1:5] <- c('FCS',NA,NA,'FALSE','TRUE')
inputGUI$indVar1Prms[1:5] <- c('rCSI',NA,NA,'FALSE','FALSE')

#Scenario 11: Cnt ~ Dsc -> log(FCS) ~ HouseTypeGrp
inputGUI <- list(depVarPrms = rep(NA,6),indVar1Prms = rep(NA,6),indVar2Prms = rep(NA,6),varTypes = rep(NA,3))
inputGUI$depVarPrms[1:5] <- c('FCS',NA,NA,'FALSE','TRUE')
inputGUI$indVar1Prms[1:5] <- c('HouseTypeGrp',NA,NA,'FALSE',NA)

#Scenario 12: Dsc ~ Cnt -> FCG ~ rCSI
inputGUI <- list(depVarPrms = rep(NA,6),indVar1Prms = rep(NA,6),indVar2Prms = rep(NA,6),varTypes = rep(NA,3))
inputGUI$depVarPrms[1:5] <- c('FCG',NA,NA,'TRUE',NA)
inputGUI$indVar1Prms[1:5] <- c('rCSI',NA,NA,'FALSE','FALSE')

#Scenario 13: Dsc ~ Dsc -> FCG ~ HouseTypeGrp
inputGUI <- list(depVarPrms = rep(NA,6),indVar1Prms = rep(NA,6),indVar2Prms = rep(NA,6),varTypes = rep(NA,3))
inputGUI$depVarPrms[1:5] <- c('FCG',NA,NA,'TRUE',NA)
inputGUI$indVar1Prms[1:5] <- c('HouseTypeGrp',NA,NA,'FALSE',NA)

#Scenario 14: Bin ~ Cnt -> FoodAssistance_YN ~ log(FCS)
inputGUI <- list(depVarPrms = rep(NA,6),indVar1Prms = rep(NA,6),indVar2Prms = rep(NA,6),varTypes = rep(NA,3))
inputGUI$depVarPrms[1:5] <- c('FoodAssistance_YN',NA,NA,'FALSE',NA)
inputGUI$indVar1Prms[1:5] <- c('FCS',NA,NA,'FALSE','TRUE')

#Scenario 15: Bin ~ Dsc -> FCS>=32 ~ IDP_YN
inputGUI <- list(depVarPrms = rep(NA,6),indVar1Prms = rep(NA,6),indVar2Prms = rep(NA,6),varTypes = rep(NA,3))
inputGUI$depVarPrms[1:5] <- c('FCS','>=',32,'FALSE',NA)
inputGUI$indVar1Prms[1:5] <- c('IDP_YN',NA,NA,'FALSE',NA)


#########################AUTOMATICALLY EXCUTED BY GUI##########################

#(1) Create human-understandable variable names from inputs
inputGUI$depVarPrms[6] <- varNameConstructor(inputGUI$depVarPrms)
inputGUI$indVar1Prms[6] <- varNameConstructor(inputGUI$indVar1Prms)
inputGUI$indVar2Prms[6] <- varNameConstructor(inputGUI$indVar2Prms)

#(2) Condition SvyDesignObj from variables
svyDsgnObj <- conditionVars(svyDsgnObj,inputGUI)

#(3) Evaluate DataTypes of dependent and independent variables
inputGUI$varTypes <- evalVarTypes(svyDsgnObj)

################################PERFORM ANALYSIS################################

#(A) Calculate Summary Statistics
sumStatsDf <- calcSumStats(inputGUI$varTypes,svyDsgnObj)
#Note that the 1st independent variable (rCSI) is continuous 
#CalcSumStats automatically bins it into 5 groups and calculates summary statistics treating it as a categorical variable

#(B) Calculate ANOVA 
anovaList <- calcANOVA(inputGUI$varTypes,svyDsgnObj)
anovaDf <- anovaList[[1]]

#(C) Calculate Regression
rgrsDf <- anovaList[[2]]

#(D) Perform Interaction Analysis
intrAnlDf <- calcMultCmp(inputGUI$varTypes,svyDsgnObj)

#(E) Compare two periods (function must still be written)

#(F) Compare all periods (function must still be written)

############################RETURN DATAFRAME TO GUI############################
#Replaces IndVar1, IndVar2, and DepVar with original names of variables
replaceVarNames(sumStatsDf,inputGUI)
replaceVarNames(anovaDf,inputGUI)
replaceVarNames(rgrsDf,inputGUI)
replaceVarNames(intrAnlDf,inputGUI)

#################################PLOT RESULTS#################################
#Returns plots of summary stats, up to 3 can be returned
plotSumStats(inputGUI,svyDsgnObj,1)
plotSumStats(inputGUI,svyDsgnObj,2)
plotSumStats(inputGUI,svyDsgnObj,3)