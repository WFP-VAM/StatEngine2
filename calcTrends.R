if(!isNamespaceLoaded('survey')) library('survey') 
if(!isNamespaceLoaded('plyr')) library('plyr') 

calcTrends <- function(obj,...) UseMethod('calcTrends',obj)

calcTrends.multi.survey.design <- function(multiSvyDsgnObj){
  sumStatsDf <- calcSumStats(multiSvyDsgnObj,BYadjust = TRUE)
  return(calcTrends.data.frame(sumStatsDf))
}

calcTrends.data.frame <- function(sumStatsDf){
  
  #must have multiple surveys
  stopifnot('Svy' %in% names(sumStatsDf))
  
  #compute period
  sumStatsDf$Date <- do.call('c',lapply(as.character(sumStatsDf[,'Svy']),function(x) as.Date(substring(x,nchar(x)-9,nchar(x)))))
  sumStatsDf$prd <- as.numeric(round((sumStatsDf$Date-min(sumStatsDf$Date))/30)+1)
  sumStatsDf$Svy <- NULL
  
  #must have multiple surveys over sequenial dates
  stopifnot(max(sumStatsDf$prd)>1)
  
  #Get Original Column Names of sumStatsDf
  orgCols <- names(sumStatsDf)
  
  #Depending on variable type, compute model, and retrieve results
  if(any(grepl('pctl',names(sumStatsDf)))){ #If continuous dependent variable
    
    #compute inverse-variance weights
    sumStatsDf$wgt <- 1/(sqrt(sumStatsDf$NumObs)*(sumStatsDf$CnfIntvHi-sumStatsDf$CnfIntvLo)/3.92)^2
    sumStatsDf[is.na(sumStatsDf$wgt),'wgt'] <- 1
    
    #Compute model
    if(any(grepl('indVar2',names(sumStatsDf)))){ # If 2 indepedent variables
      mdl <- lm(depVar~prd:indVar1:indVar2+indVar1:indVar2,sumStatsDf,weights=wgt)
    } else { # If 1 independent Variable
      mdl <- lm(depVar~prd:indVar1+indVar1,sumStatsDf,weights=wgt)
    }
    
    #retrieve results (coefficients, t value, and p value)
    rsltDf <- as.data.frame(summary(mdl)$coefficients)
    rsltDf$vars <- rownames(rsltDf)
    rsltDf <- rsltDf[grepl('prd:',rsltDf$vars),]
    rsltDf$vars <- gsub('indVar2','',gsub('prd:indVar1','',rsltDf$vars))
    varNames <- do.call(rbind,strsplit(as.character(rsltDf$vars),':'))
    rsltDf$`Std. Error` <- NULL
    
    #Formate estimates to Percent change per month
    colnames(rsltDf)[1] <- 'Dezlta/Month' #misspelling is a trick to get the column ordering right
    
    #Add variable name columns
    if(dim(varNames)[2]==2){
      rsltDf <- cbind(indVar1 = varNames[,1],indVar2 = varNames[,2],rsltDf[,1:(dim(rsltDf)[2]-1)])
    } else{
      rsltDf <- cbind(indVar1 = varNames[,1],rsltDf[,1:(dim(rsltDf)[2]-1)])
    }
    
  } else if(any(grepl('_CnfIntv',names(sumStatsDf)))){ #If discrete dependent variable
    
    cnfIntvText <- regexpr('_CnfIntv',names(sumStatsDf)) #Columns containing '_CnfIntv'
    cnfIntvCols <- names(sumStatsDf)[cnfIntvText>1] #Confidence intervals columns
    cnfIntvText <- cnfIntvText[cnfIntvText>1] #Below we get names of dependent variables
    depVarNames <- sapply(c(1:length(cnfIntvCols)),function(i) substring(cnfIntvCols[i],1,cnfIntvText[i]-1))
    rm(cnfIntvText)
    rm(cnfIntvCols)
    
    for(v in depVarNames){
      newDepVar <- paste(v,'_LO',sep='')
      #Compute Log-Odds of dependent Variable
      sumStatsDf[,newDepVar]  <- log(sumStatsDf[,v]/(1-sumStatsDf[,v]))
      sumStatsDf[is.infinite(sumStatsDf[,newDepVar]),newDepVar] <- sign(sumStatsDf[is.infinite(sumStatsDf[,newDepVar]),newDepVar])*6.5
      #Calculate inverse variance weight
      sumStatsDf[,paste(v,'_wgt',sep='')] <- 1/(ifelse(sumStatsDf[,v]>0 & sumStatsDf[,v]<1, sumStatsDf[,v]*(1-sumStatsDf[,v]), 0.99*0.01)/sumStatsDf$NumObs)
    }
    
    #Compute combined weights
    wgtCols <- paste(depVarNames,'_wgt',sep='')
    prvlDf <- sumStatsDf[,depVarNames]
    prvlDf[prvlDf==0] <- 0.01
    prvlDf[prvlDf==1] <- 0.99
    sumStatsDf$wgt <- 1/rowSums(((sumStatsDf$NumObs*prvlDf-1)/sumStatsDf[,wgtCols])/(sumStatsDf$NumObs-length(depVarNames)))
    rm(prvlDf)
    
    #formulate model
    newDepVars <- paste(depVarNames,'_LO',sep='')
    fmlaStrng <- paste('cbind(`',paste(newDepVars,collapse='`,`',sep=''),'`) ~ prd',sep='')
    if(any(grepl('indVar2',names(sumStatsDf)))){ # If 2 indepedent variables
      fmlaStrng <- paste(fmlaStrng,':indVar1:indVar2+indVar1:indVar2',sep='')
      mdl <- lm(as.formula(fmlaStrng),sumStatsDf,weights=wgt)
    } else { # If 1 independent Variable
      fmlaStrng <- paste(fmlaStrng,':indVar1+indVar1',sep='')
      mdl <- lm(as.formula(fmlaStrng),sumStatsDf,weights=wgt)
    }
    
    #retrieve results (coefficients, t value, and p value)
    mdlSumm <- summary(mdl)
    rsltDf <- Reduce(rbind.fill,lapply(c(1:length(mdlSumm)),function(i) cbind(rsp=depVarNames[i],as.data.frame(mdlSumm[[i]]$coefficients),vars=rownames(mdlSumm[[i]]$coefficients))))
    rsltDf <- rsltDf[grepl('prd:',rsltDf$vars),]
    rsltDf$vars <- gsub('indVar2','',gsub('prd:indVar1','',rsltDf$vars))
    varNames <- do.call(rbind,strsplit(as.character(rsltDf$vars),':'))
    rsltDf$`Std. Error` <- NULL
    
    #Formate estimates to Percent change per month
    rsltDf$Estimate <- exp(rsltDf$Estimate)-1
    colnames(rsltDf)[2] <- 'Pct Dezlta/Month' #misspelling is a trick to get the column ordering right
    rsltCols <- colnames(rsltDf)[2:(length(colnames(rsltDf))-1)]
    
    #Convert dataframe from long to wide
    if(dim(varNames)[2]==2){
      rsltDf <- cbind(indVar1 = varNames[,1],indVar2 = varNames[,2],rsltDf[,1:(dim(rsltDf)[2]-1)])
      rsltDf <- reshape(rsltDf, idvar = c('indVar1','indVar2'), timevar = "rsp", direction = "wide",sep="|")
      names(rsltDf)[3:length(names(rsltDf))] <- c(t(outer(depVarNames, rsltCols, FUN = "paste",sep='|')))
    } else{
      rsltDf <- cbind(indVar1 = varNames[,1],rsltDf[,1:(dim(rsltDf)[2]-1)])
      rsltDf <- reshape(rsltDf, idvar = 'indVar1', timevar = "rsp", direction = "wide",sep="|")
      names(rsltDf)[2:length(names(rsltDf))] <- c(t(outer(depVarNames, rsltCols, FUN = "paste",sep='|')))
    }
    
  } else if(any(grepl('depVar',names(sumStatsDf)))){ #If binary dependent variable
    
    #Compute Log-Odds (replace with 6.5 if infinity (99%))
    sumStatsDf$depVar_LO <- log(sumStatsDf$depVar/(1-sumStatsDf$depVar))
    sumStatsDf[is.infinite(sumStatsDf$depVar_LO),'depVar_LO'] <- sign(sumStatsDf[is.infinite(sumStatsDf$depVar_LO),'depVar_LO'])*6.5
    
    #Calculate inverse variance Weight
    sumStatsDf$wgt <- 1/(ifelse(sumStatsDf$depVar>0 & sumStatsDf$depVar<1, sumStatsDf$depVar*(1-sumStatsDf$depVar), 0.99*0.01)/sumStatsDf$NumObs)
    
    #Compute model
    if(any(grepl('indVar2',names(sumStatsDf)))){ # If 2 indepedent variables
      mdl <- lm(depVar_LO~prd:indVar1:indVar2+indVar1:indVar2,sumStatsDf,weights=wgt)
    } else { # If 1 independent Variable
      mdl <- lm(depVar_LO~prd:indVar1+indVar1,sumStatsDf,weights=wgt)
    }
    
    #retrieve results (coefficients, t value, and p value)
    rsltDf <- as.data.frame(summary(mdl)$coefficients)
    rsltDf$vars <- rownames(rsltDf)
    rsltDf <- rsltDf[grepl('prd:',rsltDf$vars),]
    rsltDf$vars <- gsub('indVar2','',gsub('prd:indVar1','',rsltDf$vars))
    varNames <- do.call(rbind,strsplit(as.character(rsltDf$vars),':'))
    rsltDf$`Std. Error` <- NULL
    
    #Formate estimates to Percent change per month
    rsltDf$Estimate <- exp(rsltDf$Estimate)-1
    colnames(rsltDf)[1] <- 'Pct Dezlta/Month' #misspelling is a trick to get the column ordering right
    
    #Add variable name columns
    if(dim(varNames)[2]==2){
      rsltDf <- cbind(indVar1 = varNames[,1],indVar2 = varNames[,2],rsltDf[,1:(dim(rsltDf)[2]-1)])
    } else{
      rsltDf <- cbind(indVar1 = varNames[,1],rsltDf[,1:(dim(rsltDf)[2]-1)])
    }
  }
  
  #return sumStatsDf to its original state
  sumStatsDf <- sumStatsDf[,c(orgCols,'wgt')]
  
  #remove all confidence intervals, date, percentiles, HuberMAvg, and weight from sumStats -> create dataframe to reshape
  rshpDf <- sumStatsDf[sumStatsDf$prd %in% c(1,max(sumStatsDf$prd)),Filter(function(x) !any(grepl('(CnfIntv|Date|wgt|pctl|HuberMAvg)',x)),names(sumStatsDf))]
  rshpDf[rshpDf$prd==1,'prd'] <- 't0'
  rshpDf[rshpDf$prd!='t0','prd'] <- 'tT'
  
  #merge reshaped dataframe with results
  if(any(grepl('indVar2',names(rshpDf)))){ # If 2 indepedent variables
    #reshape so first and last period results in separate columns by indVar1 and indVar2
    rshpDf <- reshape(rshpDf[order(rshpDf$prd),],idvar = c('indVar1','indVar2'), timevar = "prd", direction = "wide",sep=' |')
    rsltDf <- merge(rsltDf,rshpDf,by=c(1,2))
    #return merged results in right order
    cols2order <- Filter(function(x) !any(grepl('(indVar|NumObs)',x)),names(rsltDf))
    rsltDf <- rsltDf[,c('indVar1','indVar2','NumObs |t0','NumObs |tT',cols2order[order(cols2order)])]
  } else {
    rshpDf <- reshape(rshpDf[order(rshpDf$prd),],idvar = 'indVar1', timevar = "prd", direction = "wide",sep=' |')
    rsltDf <- merge(rsltDf,rshpDf,by=1)
    #return merged results in right order
    cols2order <- Filter(function(x) !any(grepl('(indVar|NumObs)',x)),names(rsltDf))
    rsltDf <- rsltDf[,c('indVar1','NumObs |t0','NumObs |tT',cols2order[order(cols2order)])]
  }
  
  #Remove spaces
  names(rsltDf) <- gsub(' \\|' ,'\\|',names(rsltDf))
  #fix mispelling
  names(rsltDf) <- gsub('Dezlta','Delta',names(rsltDf))
  
  #drop unnecessary columns in sumStats
  sumStatsDf$prd <- NULL

  #return result
  return(list(rslt=rsltDf,sumStats4Plot=sumStatsDf))
  
}