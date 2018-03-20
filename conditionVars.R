#Condition svy design object variables to be used in computations
#Based off of the following users inputs, this function attaches three new variables to the svydesignobject:
#DepVar,IndVar1,indVar2

conditionVars <- function(svyDsgnObj,inputGUI) UseMethod("conditionVars", svyDsgnObj)

conditionVars.multi.survey.design <- function(multiSvyDsgnObj,inputGUI){
  #condition multiple survey designs
  for(i in c(1:length(multiSvyDsgnObj))){
    multiSvyDsgnObj[[i]] <- conditionVars.default(multiSvyDsgnObj[[i]],inputGUI)
  }
  return(multiSvyDsgnObj)
}

conditionVars.default <- function(svyDsgnObj,inputGUI){
  #The Variable Parameters should be passed in as lists (depVarPrms,indVar1Prms,indVar2Prms)
  #Each list should be structured as so: list(
  #(1)_name_of_variable_ is string
  #(2)_threshold_function_ is string belonging to set {'<','<=','==','>=','>'}
  #(3)_threshold_value_ is string corresponding to RHS of threshold function
  #(4)_is_discrete is variable categorical and not numeric
  #(5)_is_log is string corresponding to {TRUE,FALSE}
  #(6) custom override evaluation string written in R syntax
  
  
  #Helper function to create evaluator strings
  evalStrConstructor <- function(paramList){
    varName <- paramList[1]
    thrFunc <- paramList[2]
    thrVal  <- paramList[3]
    makeDsc <- paramList[4]
    makeLog <- paramList[5]
    custTrnsf <- paramList[6]
    
    if(!is.na(custTrnsf)){ #if custom transform
      
      if(grepl(custTrnsf,'<-')){ #remove the assignment variable name
        evalStr <- gsub("\\s","",substring(custTrnsf,regexpr('<-|=',custTrnsf)+2))
      } else if(grepl(custTrnsf,'=')){
        evalStr <- gsub("\\s","",substring(custTrnsf,regexpr('=',custTrnsf)+1))
      }
      
    } else { #If no custom evaluation string
      
      evalStr <- varName
      
      if(!is.na(thrFunc) && !is.na(thrVal)){ #if comparator/threshold
        evalStr <- paste('I(',evalStr,thrFunc,sep='')
      
        if (paramList[2]=='==' && !is.numeric(thrVal)){ #if comparator is discreet
          evalStr <- paste(evalStr,"'",thrVal,"')",sep='')
        } else {
          evalStr <- paste(evalStr,thrVal,')',sep='')
        }
      
      } else if (makeDsc=='TRUE'){ #if forced to be discrete
        evalStr <- paste('as.factor(',evalStr,')',sep='')
      }
    }
    return(evalStr)
  }
  
  #Helper function to convert binary variables to logical
  convert2LogicalorLog <- function(VarVec,logTrnsf){
    if (is.factor(VarVec)) {
      lvls = levels(na.omit(VarVec))
      v = c('Yes','No','True','False','Y','N','YES','NO','TRUE','FALSE','T','F',1,0)
      if (length(lvls)==2 && lvls %in% v) {
        VarVec <- I(VarVec==lvls[2])
      }
    } else if (is.numeric(VarVec)) {
      lvls = unique(na.omit(VarVec))
      if (length(lvls)==2 && lvls %in% v) {
        VarVec <- I(VarVec==lvls[2])
      } else if (length(lvls)>10 && logTrnsf=='TRUE') {
        if (0 %in% lvls) {
          VarVec <- log(VarVec+1)
        } else {
          VarVec <- log(VarVec)
        }
      }
    }
    return(VarVec)
  }
  
  #Helper function to evaluate variable types
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
    
    return(c('depVar'=depVarType,'indVar1'=indVar1Type,'indVar2'=indVar2Type))
  }
  
  #helper function to take parameter inputs from GUI and create human-understandable variable name
  varNameConstructor <- function(paramList){
    
    varName <- paramList[1]
    thrFunc <- paramList[2]
    thrVal  <- paramList[3]
    makeDsc <- paramList[4]
    makeLog <- paramList[5]
    custTrnsf <- paramList[6]
    
    if(!is.na(custTrnsf)){ #if custom transform
      rplStr <- gsub("\\s","",substring(custTrnsf,1,regexpr('<-|=',custTrnsf)-1))
    } else {
      rplStr <- varName
      if(!is.na(thrFunc) && !is.na(thrVal)){
        rplStr <- paste(rplStr,thrFunc,thrVal,sep='')
      } else if (!is.na(makeDsc) && makeDsc=='TRUE'){
        rplStr <- paste('fctr[',rplStr,']',sep='')
      } else if (!is.na(makeLog) && makeLog=='TRUE'){
        rplStr <- paste('log[',rplStr,']',sep='')
      }
    }
    return(rplStr)
  }
  
  depVarPrms <- inputGUI$depVarPrms
  indVar1Prms <- inputGUI$indVar1Prms
  indVar2Prms <- inputGUI$indVar2Prms
  
  #For DepVar
  svyDsgnObj$variables$depVar <- convert2LogicalorLog(with(svyDsgnObj$variables,eval(parse(text=evalStrConstructor(depVarPrms)))),depVarPrms[5])
  #For IndVar1
  svyDsgnObj$variables$indVar1 <- convert2LogicalorLog(with(svyDsgnObj$variables,eval(parse(text=evalStrConstructor(indVar1Prms)))),indVar1Prms[5])
  #For IndVar2
  if (!is.na(indVar2Prms[1])){
    svyDsgnObj$variables$indVar2 <- convert2LogicalorLog(with(svyDsgnObj$variables,eval(parse(text=evalStrConstructor(indVar2Prms)))),indVar2Prms[5])
  } else {
    svyDsgnObj$variables$indVar2 <- NA
  }
  
  #Variable types of selected variables survey design object
  svyDsgnObj$selectVars$varTypes <- evalVarTypes(svyDsgnObj)
  svyDsgnObj$selectVars$varNames <- c('depVar'=varNameConstructor(depVarPrms),
                                      'indVar1'=varNameConstructor(indVar1Prms),
                                      'indVar2'=varNameConstructor(indVar2Prms))
  
  return(svyDsgnObj)
}