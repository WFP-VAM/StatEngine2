if(!isNamespaceLoaded('survey')) library('survey') 
if(!isNamespaceLoaded('plyr')) library('plyr') 
if(!isNamespaceLoaded('splitstackshape')) library('splitstackshape') 
if(!isNamespaceLoaded('phia')) library('phia') 


#Performs post-hoc interaction analysis to uncover relationships between specific categories amongst variables
calcMultCmp <- function(varTypes,svyDsgnObj) {
  
  #Helper function for formatting output
  fmtTestDf <- function(df1,varType){
    statType <- colnames(df1)[ncol(df1)-1]
    colnames(df1)[ncol(df1)] <- 'P Val'
    colnames(df1)[ncol(df1)-1] <- 'TestStat'
    df1 <- df1[,!grepl("depVar",colnames(df1))]
    df1 <- df1[,!grepl("indVar",colnames(df1))]
    colnames(df1) <- gsub("Value","Difference", colnames(df1))
    df1$StatTest <- statType
    fctrNames <- cSplit(as.data.table(rownames(df1)),'V1',':')
    if (ncol(fctrNames)==1){
      fctrNames <- cSplit(fctrNames,'V1_1','-')
      fctrNames <- as.data.frame(t(t(fctrNames)))
      colnames(fctrNames) <- c('SubsetA','SubSetB')
      fctrNames$Variable <- NA
      fctrNames <- fctrNames[,c(3,1,2)]
    } else {
      fctrNames <- cSplit(fctrNames,'V1_2','-')
      colnames(fctrNames) <- c('SuperSet','SubsetA','SubSetB')
      fctrNames <- as.data.frame(t(t(fctrNames)))
      fctrNames$Variable <- 'IndVar1'
      fctrNames$SubVariable <- 'IndVar2'
      fctrNames <- fctrNames[,c(4,5,1,2,3)]
    }
    
    if(varType=='Cnt'){
      fctrNames$SubVariable <- fctrNames$Variable
      fctrNames$Variable <- NA
      fctrNames$SuperSet <- 'Slope'
      fctrNames <- fctrNames[,c(1,4,5,2,3)]
    }
    
    df1 <- cbind(fctrNames,df1)[rownames(df1)!='Residuals',]
    return(df1)
  }
  
  dfA <- NA
  dfB <- NA
  dfC <- NA
  mdlFmla <- NA
  
  if (varTypes[1]!='Dsc') { #if dependent variable not discrete!
    if (is.na(varTypes[3]) && varTypes[2]!='Cnt') {
      #if only 1 independent variable and indvar NOT continuous
      mdlFmla <- as.formula('depVar~indVar1')
    } else if (!(varTypes[2]=='Cnt' && varTypes[3]=='Cnt')){
      #both ind vars cannot be continuous!
      if (varTypes[2]=='Cnt'){ #fist indVar continuous
        mdlFmla <- as.formula('depVar~indVar2+indVar2:indVar1-1')
      } else if (varTypes[3]=='Cnt'){ #second indVar continuous
        mdlFmla <- as.formula('depVar~indVar1+indVar1:indVar2-1')
      } else { #Both are discrete
        mdlFmla <- as.formula('depVar~indVar1*indVar2-1')
      }
    } 
  } else {
    if (is.na(varTypes[3]) && varTypes[2]!='Cnt'){
      #Discrete models  with one discrete independent variable
      mdlFmla <- as.formula('~depVar+indVar1')
    } else if (varTypes[2]!='Cnt' && varTypes[3]!='Cnt') {
      #Discrete models with two discrete independent variabls
      mdlFmla <- as.formula('~depvar*indVar1+depVar*indVar2+indVar1*indVar2')
    }
  }
  
  if (!is.na(mdlFmla)){
    
    #run appropriate model    
    if (varTypes[1]=='Bin'){ #if dependent variable Binomial
      mdlSat <- svyglm(mdlFmla,family=binomial(link="logit"),svyDsgnObj)
    } else if (varTypes[1]=='Dsc') {#if discrete model
      mdlSat <-svyloglin(mdlFmla,svyDsgnObj)$model
    } else  {
      mdlSat <- svyglm(mdlFmla,svyDsgnObj)
    }
    
    #perform tests of hypotheses
    if (is.na(varTypes[3])){
      #if only one independent variable
      dfA <- testInteractions(mdlSat,pairwise='indVar1')
      
      dfA <- fmtTestDf(dfA,'Dsc')
      dfA$Variable <- 'IndVar1'
      
    } else if (varTypes[2]=='Dsc' && varTypes[3]=='Cnt'){
      # if Dsc+Cont independent variable
      dfA <- testInteractions(mdlSat,pairwise='indVar1')
      dfB <- testInteractions(mdlSat,pairwise='indVar1',slope='indVar2')
      
      dfA <- fmtTestDf(dfA,'Dsc')
      dfB <- fmtTestDf(dfB,'Cnt')
      dfA$Variable <- 'IndVar1'
      dfB$Variable <- 'IndVar2'
      dfB$SubVariable <-'IndVar1'
      
    } else if (varTypes[3]=='Dsc' && varTypes[2]=='Cnt'){
      # if Cnt+Dsc independent variable
      dfA <- testInteractions(mdlSat,pairwise='indVar2')
      dfB <- testInteractions(mdlSat,pairwise='indVar2',slope='indVar1')
      
      dfA <- fmtTestDf(dfA,'Dsc')
      dfB <- fmtTestDf(dfB,'Cnt')
      dfA$Variable <- 'IndVar2'
      dfB$Variable <- 'IndVar1'
      dfB$SubVariable <-'IndVar2'
      
    } else {
      # if Dsc+Dsc independent variable
      if (varTypes[1]=='Dsc'){
        #for discrete dependent variable
        dfA <- try(testInteractions(mdlSat,pairwise='indVar1',across='depVar'),silent=TRUE)
        dfB <- try(testInteractions(mdlSat,pairwise='indVar2',across='depVar'),silent=TRUE)
        
      } else {
        #For continuous dependent variable
        dfA <- try(testInteractions(mdlSat,pairwise='indVar1',across='indVar2'),silent=TRUE)
        dfB <- try(testInteractions(mdlSat,pairwise='indVar2',across='indVar1'),silent=TRUE)
        
        #Below code tests nested interaction terms
        #Create matrix of interactions for below linear hypotheses
        fctLvls <- levels(svyDsgnObj$variables$indVar1)
        fctFixMat <- diag(length(fctLvls))
        rownames(fctFixMat) <- fctLvls
        colnames(fctFixMat) <- fctLvls
        
        #Create matrix of linear hypotheses to be tested
        fctLvls <- levels(svyDsgnObj$variables$indVar2)
        nFctr <- length(fctLvls)
        fctCmbns <- combn(c(1:nFctr),2)
        fctCmbns <- rbind(fctCmbns,c(1:dim(fctCmbns)[2]))
        fctCmpMat <- matrix(0,nrow=nFctr,ncol=dim(fctCmbns)[2])
        fctCmpMat[t(fctCmbns[c(1,3),])] <- 1/sqrt(2)
        fctCmpMat[t(fctCmbns[c(2,3),])] <- -1/sqrt(2)
        rownames(fctCmpMat) <- fctLvls
        colnames(fctCmpMat) <- apply(combn(fctLvls,2),2,FUN=paste,collapse='-')
        
        fctContrasts <- list(fctFixMat,fctCmpMat)
        names(fctContrasts) <- c('indVar1','indVar2')
        
        dfC <- testInteractions(mdlSat,custom=fctContrasts)
        dfC <- fmtTestDf(dfC,'Dsc')
      }
      
      dfA <- fmtTestDf(dfA,'Dsc')
      dfB <- fmtTestDf(dfB,'Dsc')
      dfA$Variable <- 'IndVar1'
      dfB$Variable <- 'IndVar2'
    } 
    #Format results
  }  
  
  df <- NA
  
  if (any(!is.na(dfA))){
    df <- dfA
  }
  if (any(!is.na(dfB))){
    n <- ncol(df)
    df <- plyr::rbind.fill(df,dfB)
    if (n<ncol(dfB)){
      df <- df[,c(1,9,10,2,3,4,5,6,7,8)]
    }
  }
  if (any(!is.na(dfC))){
    n <- ncol(df)
    df <- plyr::rbind.fill(df,dfC)
    if (n<ncol(dfC)){
      df <- df[,c(1,9,10,2,3,4,5,6,7,8)]
    }
  }
  
  return(df)   
}