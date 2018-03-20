if(!isNamespaceLoaded('survey')) library('survey') 
if(!isNamespaceLoaded('plyr')) library('plyr') 

calcSumStats <- function(svyDsgnObj,...) UseMethod('calcSumStats',svyDsgnObj)

#For multiple surveys simultaneously
calcSumStats.multi.survey.design <- function(multiSvyDsgnObj,numBins=5,binMethod='wtd.quantile',rplcVarNames=FALSE,Alpha=0.95,BYadjust=FALSE){
  
  #If independent variables are continuous must bin -->helper function for binning
  calcBinnedGrps <- function(multiSvyDsgnObj,varName,returnObs=FALSE,numBins=5,binMethod='wtd.quantile'){
    
    obsVec <- Reduce(base::rbind,lapply(multiSvyDsgnObj, function(x) cbind(weights(x),x$variables[,varName])))
    binVec <- binning(obsVec[,2],bins=numBins,method=binMethod,weights=obsVec[,1])
    
    #unstack binVec & return results
    svyIndx <- cumsum(c(1,sapply(multiSvyDsgnObj, function(x) length(x$variables[,varName]))))
    
    if(returnObs){ #if only returning observations
      binnedObs = lapply(c(1:length(multiSvyDsgnObj)), function(x) binVec[svyIndx[x]:svyIndx[x+1]-1])
      return(binnedObs)
    
    } else { #return multiSvyDsgnObj
      newVarName <- paste(varName,'_Original',sep='')
      for (i in c(1:length(multiSvyDsgnObj))){
        multiSvyDsgnObj[[i]]$variables[,newVarName] <- multiSvyDsgnObj[[i]]$variables[,varName]
        multiSvyDsgnObj[[i]]$variables[,varName] <- binVec[svyIndx[i]:(svyIndx[i+1]-1)]
        multiSvyDsgnObj[[i]]$selectVars$varTypes[varName] <- 'Dsc'
      }
      return(multiSvyDsgnObj)
    }
  }
  
  varTypes <- multiSvyDsgnObj[[1]]$selectVars$varTypes
  
  if (varTypes[2]=='Cnt'){
    multiSvyDsgnObj <- calcBinnedGrps(multiSvyDsgnObj,'indVar1',numBins=numBins,binMethod=binMethod)
  }
  if (!is.na(varTypes[3]) & varTypes[3]=='Cnt'){
    multiSvyDsgnObj <- calcBinnedGrps(multiSvyDsgnObj,'indVar2',numBins=numBins,binMethod=binMethod)
  }

  df <- Reduce(rbind.fill,lapply(c(1:length(multiSvyDsgnObj)),function(i) cbind(Svy=names(multiSvyDsgnObj[i]),calcSumStats.default(multiSvyDsgnObj[[i]],Alpha=Alpha,BYadjust=BYadjust))))
  
  if (varTypes[2]=='Cnt'){
    for (i in c(1:length(multiSvyDsgnObj))){
      multiSvyDsgnObj[[i]]$variables$indVar1 <- multiSvyDsgnObj[[i]]$variables$indVar1_Original
      multiSvyDsgnObj[[i]]$variables$indVar1_Original <- NULL
      multiSvyDsgnObj[[i]]$selectVars$varTypes['indVar1'] <- 'cnt'
    }
  }
  if (!is.na(varTypes[3]) & varTypes[3]=='Cnt'){
    for (i in c(1:length(multiSvyDsgnObj))){
      multiSvyDsgnObj[[i]]$variables$indVar2 <- multiSvyDsgnObj[[i]]$variables$indVar2_Original
      multiSvyDsgnObj[[i]]$variables$indVar2_Original <- NULL
      multiSvyDsgnObj[[i]]$selectVars$varTypes['indVar2'] <- 'cnt'
    }
  }
  
  if(rplcVarNames){
    df <- replaceVarNames(df,multiSvyDsgnObj[[1]]$selectVars$varNames)
  }
  return(df)
}

#For a single survey
calcSumStats.survey.design <- function(svyDsgnObj,numBins=5,binMethod='wtd.quantile',rplcVarNames=FALSE,Alpha=0.95,BYadjust=FALSE){
  multiSvyDsgnObj <- list(svyDsgnObj)
  names(multiSvyDsgnObj) <- 'G$'
  df <- calcSumStats.multi.survey.design(multiSvyDsgnObj,numBins=numBins,binMethod=binMethod,Alpha=Alpha,BYadjust=BYadjust)
  df$Svy <- NULL
  if(rplcVarNames){
    df <- replaceVarNames(df,multiSvyDsgnObj[[1]]$selectVars$varNames)
  }
  return(df)
}

#To compute summary stats (M_Huber Estimator, Average (or Prevalence), Median, Confidence Interval, Variance, on cross sections of data)
calcSumStats.default <- function(svyDsgnObj,Alpha=0.95,BYadjust=FALSE){
  
  varTypes <- svyDsgnObj$selectVars$varTypes
  
  #Helper function for Huber-M robust estimator of central tendency
  svyHuber <- function(myDepVar,mySvyDsgn,deff){
    # deff isn't to be used, but needed for compatibility
    
    k = 1.5
    tol = 1.0e-6
    y <- eval(parse(text=paste('mySvyDsgn$variables$',gsub('~, ','',toString(myDepVar)))))
    
    mu <- NA
    if (length(na.omit(y))>=3) {
      
      mu <-svyquantile(myDepVar,mySvyDsgn,quantiles = 0.5)[1]
      mySvyDsgn$variables[['DepAdj']]<- eval(parse(text=paste('mySvyDsgn$variables$',gsub('~, ','',toString(myDepVar)),'-mu',sep='')))
      mad <- svyquantile(~DepAdj,mySvyDsgn,quantiles = 0.5)[1]
      
      if (mad>tol) {
        for (i in 1:1000){
          mySvyDsgn$variables[['DepAdj']] <- pmin(pmax(mu-k*mad,y),mu+k*mad)
          mu1 <- unname(svymean(~DepAdj,mySvyDsgn,na.rm=TRUE)[1],force=TRUE)
          if(abs(mu-mu1) < tol*mad) break
          mu <- mu1
        }
      }
    }
    
    rval <- mu
    names(rval)<-"HuberMAvg"
    attr(rval,"var")<-matrix(0,1,1)
    attr(rval,"statistic")<-"HuberMAvg"
    if (inherits(mySvyDsgn,"svyrep.design"))
      class(rval)<-"svrepstat"
    else
      class(rval)<-"svystat"
    return(rval)
  }
  
  #Construct formula for independendent variables
  if (varTypes[2]=='Cnt'){
    fmlaStrng <- '~Hmisc::cut2(indVar1,g=5)'
  } else {
    fmlaStrng <- '~indVar1'
  }
  if (!is.na(varTypes[3])){
    if (varTypes[3]=='Cnt'){
      fmlaStrng <- paste(fmlaStrng,'+Hmisc::cut2(indVar2,g=5)',sep='')
    } else {
      fmlaStrng <- paste(fmlaStrng,'+indVar2',sep='')
    }
  }
  indVarFmla <- as.formula(fmlaStrng)
  
  #Clean Column Names Helper function
  clnColNames <- function(df){
    df <- df[,!grepl("FALSE",colnames(df))]
    colnames(df) <- gsub("TRUE", "", colnames(df))
    colnames(df) <- gsub("I\\(","", colnames(df))
    colnames(df) <- gsub(")", "", colnames(df))
    colnames(df) <- gsub("Hmisc::cut2\\(", "", colnames(df))
    colnames(df) <- gsub(", g = 5", "", colnames(df))
    return(df)
  }
  
  #Compute observation counts
  dfN <- clnColNames(as.data.frame(svyby(~depVar,indVarFmla,svyDsgnObj,unwtd.count)))
  dfN <- subset(dfN,select=-se)
  colnames(dfN)[ncol(dfN)] <- "NumObs"
  
  if (varTypes[1]=='Cnt'){ #if dependent variable continuous
    #Compute Mean
    dfM <- clnColNames(as.data.frame(svyby(~depVar,indVarFmla,svyDsgnObj,svymean,vartype='ci',level=Alpha)))
    #Compute Quantiles
    dfQ <- clnColNames(as.data.frame(svyby(~depVar,indVarFmla,svyDsgnObj,svyquantile,quantiles=c(0.05,0.25,0.5,0.75,0.95),keep.var=FALSE)))
    #Compute Huber-M Estimator of Central Tendency
    dfH <- clnColNames(as.data.frame(svyby(~depVar,indVarFmla,svyDsgnObj,svyHuber)))
    
    #Reformat Tables
    colnames(dfQ)[grepl("statistic",colnames(dfQ))] <- c("5pctl","25pctl","50pctl","75pctl","95pctl")
    dfH <- dfH[,c(1,2,3)]
    #Replace Confidence Interval Names
    colnames(dfM)[grepl("ci_l",colnames(dfM))] <- "CnfIntvLo"
    colnames(dfM)[grepl("ci_u",colnames(dfM))] <- "CnfIntvHi"
    #Combine dataframes -> final product
    df <- merge(merge(merge(dfM,dfN),dfH),dfQ)
    
  } else { 
    #Compute percentages
    if (is.factor(svyDsgnObj$variables$depVar)){ #if dependent variable discrete
      dfM <- data.frame()
      i <- 0
      for (lvl in levels(svyDsgnObj$variables$depVar)) {
        #Turn dependent variable to multiple binary variables, ie: I(depVar==lvl_A)
        if (typeof(lvl)=='character'){
          depVarfmla <- as.formula(paste('~I(depVar=="',lvl ,'")',sep=''))
        } else {
          depVarfmla <- as.formula(paste('~I(depVar==',lvl,')',sep=''))
          lvl <- toString(lvl)
        }
        #calculate results
        #Benjamini & Yekutieli adjustment for multiple hypothesis testing
        if(BYadjust){
          alphaDsc <- 1-p.adjust(rep(1-Alpha,length(levels(svyDsgnObj$variables$depVar))-1),method='BY')[1]
        } else {
          alphaDsc <- Alpha
        }
        
        dfm <- as.data.frame(svyby(depVarfmla,indVarFmla,svyDsgnObj,svyciprop,vartype='ci',method='beta',level=alphaDsc))
        
        #format data frame
        colnames(dfm)[ncol(dfm)-2] <- lvl
        cnfPctColName <- paste(lvl,'_CnfIntvLen',sep='')
        dfm[,cnfPctColName] <- ifelse(dfm[,lvl]==0, NA, (dfm$ci_u-dfm$ci_l)/2)#dfm[,lvl])
        dfm <- subset(dfm, select=-c(ci_l,ci_u))
        
        #merge with previous result
        if (i==0){
          dfM <- dfm
          i <- 1
        } else {
          dfM <- merge(dfM,dfm,all=TRUE)
        }
        
      }
    } else if (is.logical(svyDsgnObj$variables$depVar)){#if binary
      #calculate results
      dfM <- clnColNames(as.data.frame(svyby(~depVar,indVarFmla,svyDsgnObj,svyciprop,vartype='ci',method='beta',level=Alpha)))
      
      #format data frame
      colnames(dfM)[grepl("ci_l",colnames(dfM))] <- "CnfIntvLo"
      colnames(dfM)[grepl("ci_u",colnames(dfM))] <- "CnfIntvHi"
      #dfM[dfM$depVar>0,'CnfIntvPct'] <- ifelse(dfM$depVar==0,NA,(dfM$CnfIntvHi-dfM$CnfIntvLo)/dfM$depVar)
      
    }
    df <- merge(dfN,clnColNames(dfM))
  }
  
  return(df)
}


#########################################################
# Rattle: A GUI for Data Mining in R
#
# BIN DATA
#
# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2014-09-05 21:27:32 gjw>
#
# Copyright (c) 2009-2014 Togaware Pty Ltd
#
# This files is part of Rattle.


binning <- function (x, bins=4,
                     method=c("quantile", "wtd.quantile", "kmeans"),
                     labels=NULL, ordered=TRUE,
                     weights=NULL)
{
  # Set ordered to FALSE in Rattle since randomForests don't work when
  # the factor is ordered, for some reason (080406).
  
  # Best k for natural breaks
  
  varkmeans <- function (x, centers, iter.max=10, num.seeds=bins)
  {
    if (mode(x) == "numeric")
    {
      x <- data.frame(new.x=x)
    }
    KM <- kmeans(x=x, centers=centers, iter.max=iter.max)
    for (i in seq_len(num.seeds))
    {
      newKM <- kmeans(x=x, centers=centers, iter.max=iter.max)
      if (sum(newKM$withinss) < sum(KM$withinss))
      {
        KM <- newKM
      }
    }
    KM$tot.withinss <- sum(KM$withinss)
    xmean <- apply(x, 2, mean)
    centers <- rbind(KM$centers, xmean)
    bss1 <- as.matrix(dist(centers)^2)
    KM$betweenss <- sum(as.vector(bss1[nrow(bss1), ]) * c(KM$size, 0))
    return(KM)
  }
  
  method <- match.arg(method)
  if(is.factor(x)) stop(Rtxt("This variable is already a factor."))
  if (is.data.frame(x)) stop(Rtxt("An object of class data.frame is required."))
  if (length(x) < bins) stop(Rtxt("There are more bins than observations."))
  #if (method == "wtd.quantile" &&
  #    ! packageIsAvailable("Hmisc", Rtxt("weighted quantile binning")))
  #  stop(Rtxt("wtd.quantile requires the Hmisc package."))
  
  # Binning
  
  x <- if (method == "quantile")
  {
    breaks <- c(quantile(x, probs = seq(0, 1, 1/bins), na.rm = TRUE, type=8))
    breaks <- unique(breaks)
    breaks[1] <- min(x, na.rm=TRUE)
    breaks[length(breaks)] <- max(x, na.rm=TRUE)
    # quantiles from quantile() can be non-unique, which cut() doesn't
    # like. This is handled above through unique(). The function
    # cut2() in Hmisc handles this situation gracefully and it could
    # be used, but it is not necessary.
    if(length(breaks) >= 2)
    {
      cut(x, breaks, include.lowest = TRUE, labels = labels)
    }
    else
    {
      cat(Rtxt("Warning: the variable is not considered.\n"))
      return(NULL)
    }
  }
  else if (method == "wtd.quantile")
  {
    breaks <- c(Hmisc::wtd.quantile(x, weights=weights, probs=seq(0, 1, 1/bins),
                                    na.rm=TRUE, type="quantile"))
    breaks <- unique(breaks)
    breaks[1] <- min(x, na.rm=TRUE)
    breaks[length(breaks)] <- max(x, na.rm=TRUE)
    # quantiles from quantile() can be non-unique, which cut() doesn't
    # like. This is handled above through unique(). The function
    # cut2() in Hmisc handles this situation gracefully and it could
    # be used, but it is not necessary.
    if(length(breaks) >= 2)
    {
      cut(x, breaks, include.lowest = TRUE, labels = labels)
    }
    else
    {
      cat(Rtxt("Warning: the variable is not considered.\n"))
      return(NULL)
    }
  }
  else if (method == "kmeans")
  {
    xx <- na.omit(x)
    maxbins <-nlevels(as.factor(xx))
    if(maxbins < bins)
    { 
      bins <-maxbins
    }
    breaks <- c(min(xx), tapply(xx, varkmeans(xx, bins)$cluster, max))
    if (length(unique(breaks)) >= 2)
    {
      cut(x, unique(breaks), include.lowest = TRUE, labels = labels)	
    }
    else
    {
      cat(Rtxt("Warning: the variable is not considered.\n"))
      return(NULL)	
    }
  }
  
  if(ordered == TRUE)
    result <- ordered(factor(x))
  else
    result <- factor(x)
  
  attr(result, "breaks") <- breaks
  return(result)
}