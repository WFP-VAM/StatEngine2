if(!isNamespaceLoaded('survey')) library('survey') 
if(!isNamespaceLoaded('plyr')) library('plyr') 

#S3 naming convention
createSvyDsgn <- function(inputDB,...) UseMethod("createSvyDsgn", inputDB)

#Create multiple survey design object
createSvyDsgn.multi <- function(multiInputDB,stackSvy=FALSE){
 
  #Make sure list is named (change this)
  names(multiInputDB) <- sapply(multiInputDB, function(x) x$svyName)
  
  #Remove fields not common to all surveys
  cmmnFlds <- Reduce(base::intersect,lapply(multiInputDB,function(x) names(Filter(function(y) !(all(is.na(y)|is.null(y))),multiInputDB[[1]]$obsDf))))
  for(i in c(1:length(multiInputDB))){
    multiInputDB[[i]]$obsDf <- multiInputDB[[i]]$obsDf[,cmmnFlds] 
  }
  
  #Create survey designs
  multiSvyDsgnObj <- sapply(multiInputDB,FUN=createSvyDsgn.default,simplify=FALSE,USE.NAMES=TRUE)
  
  #Add stacked survey if true
  if(stackSvy){
    multiSvyDsgnObj[[length(multiSvyDsgnObj)+1]] <- createSvyDsgn.stack(multiInputDB,multiSvyDsgnObj)
  }
  
  class(multiSvyDsgnObj) <- 'multi.survey.design'
  
  return(multiSvyDsgnObj)
  
}


#Create automated way to string together input surveys into multiple survey design
createSvyDsgn.stack <- function(multiInputDB,multiSvyDsgnList){
  
  #function for checking if everything in set is equal
  all.elements.equal <- function(x) length(unique.default(x)) == 1L
  nSvys <- length(multiInputDB)
    
  #(1) Check splUnitCols -> largest set should cover all other sets
  splUnitCols <- lapply(multiInputDB, function(x) x$splUnitCols)
  
  if(all.elements.equal(splUnitCols)){
    newSplUnitCols <- splUnitCols[[1]]
  } else {
    #Find survey with largest set of sampling units
    lenSplSets <- sapply(splUnitCols, function(x) length(x))
    maxSplSet <- splUnitCols[[which(lenSplSets==max(lenSplSets))[1]]]
    #All other surveys must have same sampling units or start at the second stage of the max svy
    stopifnot(all(sapply(splUnitCols,function(x) identical(x,maxSplSet)|identical(x,maxSplSet[2:length(maxSplSet)]))))
    #For surveys where sampling units start at  second stage of max svy -> repeat stage to create a fake stage with a single observation
    splUnitCols[sapply(splUnitCols, function(x) identical(x,maxSplSet[2:length(maxSplSet)]))] <- list(c(maxSplSet[2],maxSplSet[2:length(maxSplSet)]))
    #Modify Obs Table to take this into account
    newSplUnitCols <- sapply(c(1:length(maxSplSet)),function(x) paste('SMPLUNIT_',x,sep=''))
    for(i in c(1:nSvys)){
      for(j in c(1:length(maxSplSet))){
        multiInputDB[[i]]$obsDf[,newSplUnitCols[j]] <- multiInputDB[[i]]$obsDf[,splUnitCols[[i]][j]]
      } 
    }
  }
  
  #(2) Check Strata Cols -> If not all same, then reset strata to greatest (least granular) of all strata
  strUnitCols <- lapply(multiInputDB, function(x) x$strUnitCols)
  aggDfMtrxs <- lapply(multiInputDB, function(x) x$aggDf)
  
  if(all.elements.equal(strUnitCols) & all.elements.equal(aggDfMtrxs)){
    #If all strata are same then simply choose that
    newStrUnitCols <- strUnitCols[[1]]
    #Set aggregation dataframe to the associated common one
    newAggDf <- multiInputDB[[1]]$aggDf      
    for(i in c(1:nSvys)){
      multiInputDB[[i]]$obsDf$STRTIMEPRD <- as.character(multiInputDB[[i]]$Date0)
    }
    newStrUnitCols
  } else { #Choose strata design of survey that is least granular
    #Can Max have one stratum in each survey
    stopifnot(all(!sapply(strUnitCols,function(x) is.list(x))))
    #Count number of individual groups in each strata for each survey
    unitsPerStrata <- numeric(nSvys)
    for(i in c(1:nSvys)){
      df <- multiInputDB[[i]]$obsDf
      if(!empty(multiInputDB[[i]]$aggDf) & all(!is.na(multiInputDB[[i]]$aggDf))){
        df <- merge(df,multiInputDB[[i]]$aggDf)
      }
      if(is.na(strUnitCols[[i]])){
        unitsPerStrata[i] <- 0
      } else {
        unitsPerStrata[i] <- length(levels(interaction(multiInputDB[[i]]$obsDf[,strUnitCols[[i]]])))
      }
      multiInputDB[[i]]$obsDf$STRTIMEPRD <- as.character(multiInputDB[[i]]$Date0)
    }
    #Choose strata with smallest count
    newStrUnitCols <- strUnitCols[[which(unitsPerStrata==min(unitsPerStrata))[1]]]
    #choose aggregated admin units that may correspond to above new strata
    newAggDf <- multiInputDB[[which(unitsPerStrata==min(unitsPerStrata))[1]]]$aggDf
  }
  #Combine first strata with Time Period
  if(is.na(newStrUnitCols[[1]])){
    newStrUnitCols[[1]] <- 'STRTIMEPRD'
  } else {
    newStrUnitCols[[1]] <- paste('interaction(STRTIMEPRD,',newStrUnitCols[[1]],')',sep='')
  }
  
  #(3) If sampling PPS, w/o replacement, and consistent FPCs, Strata, and Sampling Units  
  PPSYN <- sapply(multiInputDB, function(x) x$PPSYN) #Was sampling with probability proportional to size
  smplWRplcYN <- sapply(multiInputDB, function(x) x$smplWRplcYN) 
  fpcUnitCols <- sapply(multiInputDB, function(x) x$fpcUnitCols)
  splWghtCols <- sapply(multiInputDB, function(x) x$splWghtCol)
  adjWghtCols <- sapply(multiInputDB, function(x) x$adjWghtCols)
  
  #If PPSYN=TRUE, smpwWRplcYN=FALSE, splUnitCols, strUnitCols, fpcUnitCols are SAME and no weights
  #Need to specify differently to account for Brewer's estimator for variance
  
  if (all(PPSYN)==TRUE & all(smplWRplcYN)==FALSE &
      all.elements.equal(fpcUnitCols) &
      all(is.na(splWghtCols)) &
      all(is.na(adjWghtCols)) &
      is.na(sapply(multiInputDB, function(x) x$calbrDF)))
  {
    newFPCUnitCols <- fpcUnitCols[[1]]
    newSplWghtCol <- NA
    
    #combine post-stratification Dataframes if exist
    if (all(sapply(multiInputDB, function(x) is.data.frame(x$postStrDf)))){
      stopifnot(all.elements.equal(lapply(multiInputDB,function(x) names(x$postStrDf))))
      newPostStrYN <- TRUE
      newPostStrDf <- Reduce(base::rbind,lapply(multiInputDB,function(x) cbind(STRTIMEPRD=x$Date0,x$postStrDf)))
    } else {
      newPostStrYN <- FALSE
      newPostStrDf <- NA
    }
    
    #combine population Dataframes if exist
    if (all(sapply(multiInputDB, function(x) is.data.frame(x$popDf)))){
      stopifnot(all.elements.equal(lapply(multiInputDB,function(x) names(x$popDf))))
      newPopDf <- Reduce(base::rbind,lapply(multiInputDB,function(x) cbind(STRTIMEPRD=x$Date0,x$popDf)))
    } else {
      newPopDf <- NA
    }
    
    #combine post-stratification Dataframes if exist
    if (all(sapply(multiInputDB, function(x) is.data.frame(x$postStrDf)))){
      stopifnot(all.elements.equal(lapply(multiInputDB,function(x) names(x$postStrDf))))
      
      #normalize if percentages by FPC
      for(i in c(1:nSvys)){
        lastColNum <- ncol(multiInputDB[[i]]$postStrDf)
        if (all(multiInputDB[[i]]$postStrDf[,lastColNum]<1)){
          df <- multiInputDB[[i]]$obsDf
          if(!empty(newPopDf) & all(!is.na(newPopDf))){
            df <- merge(newPopDf,df)
          }
          totPop <- sum(unique(df[,newFPCUnitCols[length(newFPCUnitCols)]])) #This is extremely dangerous and should be modified
          multiInputDB[[i]]$postStrDf[,lastColNum] <- round(multiInputDB[[i]]$postStrDf[,lastColNum]*totPop,1)
        }
      }
      
      newPostStrDf <- Reduce(base::rbind,lapply(multiInputDB,function(x) cbind(STRTIMEPRD=x$Date0,x$postStrDf)))
    } else {
      newPostStrYN <- FALSE
      newPostStrDf <- NA
    }
    
    
  } else {
    newFPCUnitCols <- NA
    newPostStrDf <- NA
    newPopDf <- NA
    newPostStrYN <- FALSE
    newSplWghtCol <- 'SVYDSGNWGT'
    
    #Create and retrieve weights for each survey
    for(i in c(1:length(multiInputDB))){
      multiInputDB[[i]]$obsDf$SVYDSGNWGT <- weights(multiSvyDsgnList[[i]])
    }
  }
  
  #(4) Stack Observations to make unified observations dataframe
  cmmnFlds <- Reduce(base::intersect,lapply(multiInputDB,function(x) names(multiInputDB[[1]]$obsDf)))
  stkObsDf <- Reduce(base::rbind,lapply(multiInputDB,function(x) multiInputDB[[1]]$obsDf[,cmmnFlds]))
  
  #(5) Create input for new stacked survey design object
  inputDB = list(svyID = NA, #ID of survey you would like to load
                 splWghtCol = newSplWghtCol, #base population weight column
                 adjWghtCols = NA, #columns to multiply base weight by to adjust for different selection biases
                 PPSYN = all(sapply(multiInputDB, function(x) x$PPSYN)), #Was sampling with probability proportional to size
                 smplWRplcYN = !all(sapply(multiInputDB, function(x) !x$smplWRplcYN)), #Was sampling with or without replacement
                 rptRspYN = all(sapply(multiInputDB, function(x) x$rptRspYN)), #Are respondents repeated (this may need to be modified for rotation panels)
                 splUnitCols = newSplUnitCols, #Sampling Units in order of largest to smallest (if two stage cluster, first item would be cluster ID)
                 strUnitCols = newStrUnitCols, #Stratification Variables for above sampling units
                 fpcUnitCols = newFPCUnitCols, #NA if not PPS+Sample W/O Replacement
                 obsDf = stkObsDf, #Observations to be loaded for survey
                 aggDf = newAggDf, #NA if not PPS+Sample W/O Replacement
                 popDf = newPopDf, #NA if not PPS+Sample W/O Replacement
                 postStrYN = newPostStrYN, #TRUE ONLY IF PPS+Sample W/O Replacement
                 postStrDf = newPostStrDf, #NA if not PPS+Sample W/O Replacement
                 calbrYN = FALSE,
                 calbrTbl = NA,
                 calbrCfg = NA)

  return(createSvyDsgn.default(inputDB))
}


#Create survey Design Object (need to modify to include clusters)
# User input would be a minimum of ObsDf dataframes. An Additional 6 dataframes may also be inputs
createSvyDsgn.default <- function(inputDB){
  
  ###### INPUTS ###### 
  
  #postStrDf = Dataframe for Post-Stratification inputs structured as so:
  #            [stratification Variable 1,...,StrVarN, Marginal Variable 1, MrgVar2,...MrgVarN, Population%]
  #Post-stratification further adjusts units internal to strata to make it 'seem' like the survey was also stratified on those units too
  #Hence Post-Stratification requires full specification of joint distributions including the stratification variables as marginals
  #calbrDf = Dataframe for Calibration inputs structured as so [Marginal Variable 1, MrgVar2,...,MrgVarN, Population%]
  #Calibration  simply adjusts weights to better match known population distributions
  #Can specificy any amount of known marginal or joint distributions amongst population parameters
  
  if(all(!is.na(inputDB$calbrCfg))){
    if(all(!is.na(inputDB$calbrCfg$calfun))){
      calfun <- inputDB$calbrCfg$calfun #calibration link functions
    } else {
      calfun <- 'logit'
    }
    if(all(!is.na(inputDB$calbrCfg$bounds))){
      bounds <- inputDB$calbrCfg$bounds #for optimized calibrated weights
    } else {
      bounds <- c(0.125,8)
    }
    if(all(!is.na(inputDB$calbrCfg$trim))){
      trim <- inputDB$calbrCfg$trim #trimming values for weights that fail to converge to bounds
    } else {
      trim <- c(0.5,2.5)
    }
  } else {
    calfun <- 'logit'
    bounds <- c(0.125,8)
    trim <- c(0.5,2.5)
  }
  
  #Remove all NULL or NA columns in ObsDF
  inputDB$obsDf <- Filter(function(x) !(all(is.na(x)|is.null(x))), inputDB$obsDf)
  
  #If Administrative Districts have been aggregated together for the purpose of the survey
  #Merge Observation and Aggregation dataframes (should merge cleanly on joint administrative columns)
  if(!empty(inputDB$aggDf) & all(!is.na(inputDB$aggDf))){
    inputDB$obsDf <- merge(inputDB$obsDf,inputDB$aggDf)
  }
  
  #Populations of the entity that the sampling units belong
  #Merge Observation and population dataframes (should merge cleanly on shared columns)
  if(!empty(inputDB$popDf) & all(!is.na(inputDB$popDf))){
    inputDB$obsDf <- merge(inputDB$obsDf,inputDB$popDf)
  }
  
  #Calculate adjusted weights if provided
  wghtCol <- inputDB$splWghtCol
  adjustCols <- inputDB$adjWghtCols
  if(!is.na(wghtCol) & !is.na(adjustCols) & length(wghtCol)>0 & length(adjustCols)>0){
    for(adjustCol in adjustCols){
      inputDB$obsDf[,wghtCol] <- inputDB$obsDf[,wghtCol]*inputDB$obsDf[,adjustCol]
    }
  }
  
  #obsDf$ObsCnt <- ave(obsDf$StrPop, obsDf$StrName, FUN=length)
  #obsDf$baseWght <- obsDf$StrPop/obsDf$ObsCnt
  #obsDf[,-which(names(obsDf)=='StrName')]
  
  ###### CREATE SURVEY DESIGN OBJECT ###### 
  
  #unpack inputs for creating survey design object
  splUnitCols <- inputDB$splUnitCols #list of column names denoting the sampling units in their staging order
  strUnitCols <- inputDB$strUnitCols #list of column names denoting the the stratification columns for the above sampling units in their staging order
  fpcUnitCols <- inputDB$fpcUnitCols #list of column names denoting the the finite population columns for the above sampling units in their staging order
  
  #weight columns is NA if none specified
  if (!is.na(wghtCol) & length(wghtCol)>0){
    fmlaWght <- as.formula(paste('~',wghtCol))
  } else {
    fmlaWght <- NULL
  }
  
  #finite population columns is NA if none specified
  fmlaPPS <- FALSE
  if (!is.na(fpcUnitCols) & length(fpcUnitCols)>0){
    #String together finite population counts of units into equation
    fmlaFPC <- as.formula(paste('~' ,paste(fpcUnitCols, collapse='+')))
    #can only specify PPS w/o replacement if FPC and NO baseweights
    if (inputDB$PPSYN==TRUE & inputDB$smplWRplc==FALSE & is.na(wghtCol)){
      fmlaPPS <-'brewer'
    }
  } else {
    fmlaFPC <- NULL
  }
  
  #String together sampling units into equation
  if(!is.na(splUnitCols) & length(splUnitCols)>0){
    fmlaSplUnit <- as.formula(paste('~' ,paste(splUnitCols, collapse='+')))
  } else {
    fmlaSplUnit <- as.formula('~1')
  }
  
  #String together stratification entities into equation
  if(!is.na(strUnitCols) & length(strUnitCols)>0){
    
    #Must combine instances where a single strata is defined by two columns
    #multiStrIndx <- sapply(strUnitCols,FUN=length)>1
    #strUnitCols[multiStrIndx] <- sapply(strUnitCols[multiStrIndx],function(x) paste('interaction(',paste(x,sep='',collapse=','),')',sep='') )
    strUnitCols <- as.list(strUnitCols)
    for(colIndx in which(sapply(strUnitCols,FUN=length)>1)){
      colSet <- unlist(strUnitCols[colIndx])
      newColName <- paste(colSet,collapse='_')
      inputDB$obsDf[,newColName] <- interaction(inputDB$obsDf[,colSet])
      strUnitCols[colIndx] <- newColName
    }
    strUnitCols <- unlist(strUnitCols)
    
    #If sampling unit is not stratified, must specify with 'NULL'
    strUnitCols[is.na(strUnitCols)] <- 'NULL'
    if (length(strUnitCols) < length(splUnitCols)) {
      fmlaStr[length(strUnitCols)+1:length(splUnitCols)] <- 'NULL'
    }
    fmlaStr <- as.formula(paste('~' ,paste(strUnitCols, collapse='+')))
    
  } else {
    fmlaStr <- NULL
  }
  
  #Create initial survey design object
  svyDsgnObj <- svydesign(id=fmlaSplUnit,strata=fmlaStr,weights=fmlaWght,fpc=fmlaFPC,pps=fmlaPPS,data=inputDB$obsDf,nest=TRUE)
  totPop <- sum(weights(svyDsgnObj))
  
  #Adjust weights if adjustment column specified and no baseweights supplied earlier
  if (is.na(wghtCol) & !is.na(adjustCols) & length(adjustCols)>0){
    for(adjustCol in adjustCols){
      svyDsgnObj$prob <- svyDsgnObj$prob/inputDB$obsDf[,adjustCol]
    }
  }
  
  ###### POST STRATIFY SVYDSGNOBJ ###### 
  postStrDsgn <- function(svyDsgnObj,postStrDf,totPop){
    #Only last column of dataframe contains population percentages
    lastColNum <- ncol(postStrDf)
    
    #Convert percentages to population count
    if (all(inputDB$postStrDf[lastColNum]<1)){
      postStrDf[,lastColNum] <- round(postStrDf[,lastColNum]*totPop,1)
    }
    
    #Column Names of dataframe denote post-stratification variables 
    fmlaPostStr <- as.formula(paste('~',paste(colnames(postStrDf)[-lastColNum],collapse='+')))
    
    #Post Stratifiy survey design object
    return(postStratify(svyDsgnObj, fmlaPostStr, postStrDf))
  }
  
  #Don't Post-Stratify if False
  if(inputDB$postStrYN==FALSE){
    inputDB$postStrDf <- NA
  }
  
  if(!empty(inputDB$postStrDf) & all(!is.na(inputDB$postStrDf))){
    #Post Stratify using provided post stratification  dataframe
    svyDsgnObj <- postStrDsgn(svyDsgnObj,inputDB$postStrDf,totPop)
  } else if (is.na(wghtCol) & !is.na(adjustCols) & length(adjustCols)>0){
    #do something here if adjust cols but no baseweights
    svyDsgnObj <- postStrDsgn(svyDsgnObj,postStrDf,totPop)
  }
  
  totPop <- sum(weights(svyDsgnObj))
  
  ##### PERFORM CALIBRATION #####
  
  #Function to build terms for calibration from auxilary calibration dataframes
  createCalibrateInpt <- function(calbrDf,totPop){
    #Only last column of dataframe contains population percentages
    lastColNum <- ncol(calbrDf)
    
    #Convert percentages to population count
    calbrDf[,lastColNum] <- round(calbrDf[,lastColNum]*totPop,0)
    
    #Convert Dataframe to Cross-Table, store in list
    fmlaXtbl <- as.formula(paste(colnames(calbrDf)[lastColNum],'~',paste(colnames(calbrDf)[-lastColNum],collapse='+')))
    calbrPopDist <- xtabs(fmlaXtbl,calbrDf)
    
    #Column Names of dataframe denote calibration variables, convert to formula, store in list
    calbrFmla <- as.formula(paste('~',paste(colnames(calbrDf)[-lastColNum],collapse='+')))
    
    #Store Variable Names
    varNames <- colnames(calbrDf)[-lastColNum]
    
    #return in named list
    return(list(calbrPopDist = calbrPopDist,
                calbrFmla = calbrFmla,
                varNames = varNames
    ))
  }
  
  # run function on calibration dataframes
  calbrPopDist = list()
  calbrFmla = list()
  varNames = c()
  
  if(inputDB$calbrYN==TRUE){
    for(i in c(1:length(inputDB$calbrTbl))){
      calbrDf <- inputDB$calbrTbl[[i]]#eval(parse(text=paste('inputDB$calbrDf',i,sep='')))
      if(!empty(calbrDf) & all(!is.na(calbrDf))){
        rslt <- createCalibrateInpt(calbrDf,totPop)
        calbrPopDist[[i]] <- rslt$calbrPopDist
        calbrFmla[[i]] <- rslt$calbrFmla
        varNames <- c(varNames,rslt$varNames)
      }
    }
  }
  #Perform Calibration
  if(length(calbrPopDist)>0 & length(calbrFmla)>0 & length(varNames)>0){
    
    #Determine numeric variables to be converted to factors for calibration    
    varNames <- varNames[as.logical(colwise(is.numeric)(svyDsgnObj$variables[,varNames,drop=FALSE]))]
    
    #Convert to factors
    for(var in varNames){
      svyDsgnObj$variables[,var] <- as.factor(svyDsgnObj$variables[,var])
    }
    
    #Calibrate
    svyDsgnObj <- calibrate(svyDsgnObj,formula=calbrFmla,population=calbrPopDist,calfun=calfun,bounds=bounds,trim=trim)
    
    #Re-convert to numeric
    for(var in varNames){
      svyDsgnObj$variables[,var] <- as.numeric(svyDsgnObj$variables[,var])
    }
  }
  
  #Finally append if repeat respondents to survey design object
  svyDsgnObj$rptRspYN <- inputDB$rptRspYN
  # if(!empty(inputDB$aggDf) & all(!is.na(inputDB$aggDf))){
  #   svyDsgnObj$aggDf <- inputDB$aggDf
  # }
  # 
  return(svyDsgnObj)
}