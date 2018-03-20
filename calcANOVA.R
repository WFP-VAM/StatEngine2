if(!isNamespaceLoaded('survey')) library('survey') 
if(!isNamespaceLoaded('plyr')) library('plyr') 
if(!isNamespaceLoaded('splitstackshape')) library('splitstackshape') 

#To analyze indepdence of variables and regression tables (performs both ANOVA and regression analysis)
#Results are in two separate dataframes df, and dfRgr
calcANOVA <- function(varTypes,svyDsgnObj){
  
  df <- NA #This dataframe is for ANOVA analysis
  dfRgr <- NA #This dataframe is for Regression analysis
  
  if (is.na(varTypes[3])){ #only one independent variable
    if (varTypes[1]=='Bin'){ # Binary ~ Continuous or Binary ~ Discrete
      
      if(varTypes[2]=='Cnt'){
        mdl <- svyranktest(indVar1~depVar,svyDsgnObj,test='wilcoxon')
        #unlist results & persuade to dataframe
        rslt <- list(mdl$statistic[1],mdl$parameter[1],mdl$p.value[1])
        df <- do.call(rbind, lapply(rslt, data.frame, stringsAsFactors=FALSE))
        rownames(df)[c(2,3)] <- c('Rank-t','Rank-p')
      } else { #if discrete
        #if discrete and only 1 indp variable, perform ChiSq test
        mdl <- svychisq(~depVar+indVar1,svyDsgnObj)
        #unlist results & persuade to dataframe
        rslt <- list(mdl$statistic,mdl$parameter['ndf'],mdl$parameter['ddf'],mdl$p.value)
        df <- do.call(rbind, lapply(rslt, data.frame, stringsAsFactors=FALSE))
        rownames(df)[4] <- 'p'
      }
      
      mdl <- svyglm(depVar~indVar1,svyDsgnObj,family=binomial(link="logit"))
      rslt <- summary(mdl,correlation=TRUE)
      mdlTest <- regTermTest(mdl,fmlaTest)
      
      df['F-Stat',] <- mdlTest$Ftest[1,1]
      df['Wald P',] <- mdlTest$p[1,1]
      
      dfRgr <- as.data.frame(coef(rslt))[,c(1,2,4)]
      rownames(dfRgr) <- gsub("depVar", "", rownames(dfRgr))
      rownames(dfRgr) <- gsub("indVar1", "", rownames(dfRgr))
          
    } else if(varTypes[1]=='Dsc' && varTypes[2]=='Dsc'){ #Discrete~Discrete
      
      #if discrete and only 1 indp variable, perform ChiSq test
      mdl <- svychisq(~depVar+indVar1,svyDsgnObj)
      
      #unlist results & persuade to dataframe
      rslt <- list(mdl$statistic,mdl$parameter['ndf'],mdl$parameter['ddf'],mdl$p.value)
      df <- do.call(rbind, lapply(rslt, data.frame, stringsAsFactors=FALSE))
      rownames(df)[4] <- 'p'
      
      #Perform regression analysis
      mdl <- svyloglin(~depVar+indVar1,svyDsgnObj)
      
      dfRgr <- as.data.frame(anova(mdl$model,test='Rao'))[3,]
      
    } else if (varTypes[1]=='Cnt' && varTypes[2]=='Cnt') { #Continuous~Continuous
      
      mdl <- svyglm(depVar~indVar1,svyDsgnObj)
      rslt <- summary(mdl,correlation=TRUE)
      
      dfRgr <- as.data.frame(coef(rslt))[,c(1,2,4)]
      
      mdlTest <- regTermTest(mdl,~indVar1)
      df <- data.frame()
      df['Correlation','indVar1'] <- rslt$correlation[1,2]
      df['DF','indVar1'] <- mdlTest$df
      df['DDF','indVar1'] <- mdlTest$ddf
      df['F-Stat','indVar1'] <- mdlTest$Ftest[1,1]
      df['P','indVar1'] <- mdlTest$p[1,1]
      
    } else { #Continuous~Discrete or Discrete~Continuous
      
      if (varTypes[1]=='Cnt') {
        fmla <- as.formula('depVar~indVar1')
        fmlaTest <- as.formula('~indVar1')
        colName <- 'indVar1'
        if (sum(is.na(svyDsgnObj$variables$indVar1)>0)){
          svyDsgnObj$variables$indVar1 <- as.character(svyDsgnObj$variables$indVar1)
          svyDsgnObj$variables$indVar1[is.na(svyDsgnObj$variables$indVar1)] <- '?'
          svyDsgnObj$variables$indVar1 <- as.factor(svyDsgnObj$variables$indVar1)
        }
      } else {
        fmla <- as.formula('indVar1~depVar')
        fmlaTest <- as.formula('~depVar')
        colName <- 'depVar'
        if (sum(is.na(svyDsgnObj$variables$depVar)>0)){
          svyDsgnObj$variables$depVar <- as.character(svyDsgnObj$variables$depVar)
          svyDsgnObj$variables$depVar[is.na(svyDsgnObj$variables$depVar)] <- '?'
          svyDsgnObj$variables$depVar <- as.factor(svyDsgnObj$variables$depVar)
        }
      }
      
      mdl <- svyranktest(fmla,svyDsgnObj,test='KruskalWallis')
      #unlist results & persuade to dataframe
      
      if (names(mdl$statistic)=='t') {#if binary
        rslt <- list(mdl$statistic[1],mdl$parameter[1],mdl$p.value[1])
        df <- do.call(rbind, lapply(rslt, data.frame, stringsAsFactors=FALSE))
        rownames(df)[c(2,3)] <- c('Rank-t','Rank-p')
        
      } else { #if not binary
        rslt <- list(mdl$statistic[1],mdl$parameter[1,1],mdl$p.value[1,1])
        df <- do.call(rbind, lapply(rslt, data.frame, stringsAsFactors=FALSE))
        rownames(df)[c(2,3)] <- c('Rank-ChiSq','Rank-p')
      }
      
      colnames(df) <- colName
      
      mdl <- svyglm(fmla,svyDsgnObj)
      rslt <- summary(mdl,correlation=TRUE)
      mdlTest <- regTermTest(mdl,fmlaTest)
      
      df['F-Stat',] <- mdlTest$Ftest[1,1]
      df['Wald P',] <- mdlTest$p[1,1]
      
      dfRgr <- as.data.frame(coef(rslt))[,c(1,2,4)]
      rownames(dfRgr) <- gsub("depVar", "", rownames(dfRgr))
      rownames(dfRgr) <- gsub("indVar1", "", rownames(dfRgr))
    }
  } else { #2 independent variables
    
    if (varTypes[1]!='Dsc'){ #continuous or binary dependent variable
      
      if (varTypes[1]=='Bin'){
        mdl <- svyglm(depVar~indVar1+indVar2,family=binomial(link="logit"),svyDsgnObj)
      } else {
        mdl <- svyglm(depVar~indVar1+indVar2,svyDsgnObj)
      }
      
      #produce dataframe of results
      rslt <- anova(mdl)
      cols <- c('test.terms','chisq','df','ddf','p')
      z <- list(unlist(rslt[1])[cols],unlist(rslt[2])[cols],unlist(rslt[3])[cols])
      df <- do.call(rbind, lapply(z, data.frame, stringsAsFactors=FALSE))
      colnames(df)[1] <- 'Variable'
      colnames(df)[2] <- 'F-Stat'
      
      #results are wrong! replace with correct results!
      mdltest1 <- regTermTest(mdl,~indVar1)
      df[1,2] <- mdltest1$Ftest[1,1]
      df[1,5] <- mdltest1$p[1,1]
      
      mdltest2 <- regTermTest(mdl,~indVar2)
      df[2,2] <- mdltest2$Ftest[1,1]
      df[2,5] <- mdltest2$p[1,1]
      
      if (varTypes[2]!='Cnt' | varTypes[3]!='Cnt'){
        #Do not include interaction term if both variables continuous
        mdl <- update(mdl,~.+indVar1:indVar2)
        mdltest3 <- regTermTest(mdl,~indVar1:indVar2)
        df[3,1] <- 'indVar1:indVar2'
        df[3,2] <- mdltest3$Ftest[1,1]
        df[3,3] <- mdltest3$df
        df[3,4] <- mdltest3$ddf
        df[3,5] <- mdltest3$p[1,1]
      }
      
      dfRgr <- as.data.frame(coef(summary(mdl)))[,c(1,2,4)]
      dfRgr$ExplDev <- 1-mdl$deviance/mdl$null.deviance
      
    } else if (varTypes[2]!='Cnt' && varTypes[3]!='Cnt'){ #Discrete dependent and independent variables
      
      #Semi-saturated model
      mdlSat <- svyloglin(~depVar*indVar1+depVar*indVar2+indVar1*indVar2,svyDsgnObj)
      df <- as.data.frame(anova(mdlSat$model,test='Rao'))[2:7,]
      
      #Reduced Models remove 1st, 2nd, and 3rd terms respectively from semi-saturated model
      mdl0 <- svyloglin(~depVar+indVar1+indVar2)
      mdl1 <- svyloglin(~depVar*indVar1+depVar*indVar2,svyDsgnObj)
      mdl2 <- svyloglin(~indVar1*indVar2+depVar*indVar1,svyDsgnObj)
      mdl3 <- svyloglin(~indVar1*indVar2+depVar*indVar2,svyDsgnObj)
      
      #test reduced models above against semi-saturated model by adding missing term
      test1 <- try(anova(mdl0,update(mdl1,~.+indVar1*indVar2)),TRUE)
      test2 <- try(anova(mdl0,update(mdl1,~.+depVar*indVar1)),TRUE)
      test3 <- try(anova(mdl0,update(mdl1,~.+depVar*indVar2)),TRUE)
      test4 <- try(anova(mdl1,update(mdl1,~.+indVar1*indVar2)),TRUE)
      test5 <- try(anova(mdl2,update(mdl2,~.+depVar*indVar2)),TRUE)
      test6 <- try(anova(mdl3,update(mdl3,~.+depVar*indVar1)),TRUE)
      
      #if no error then insert into dataframe
      if (is.list(test1)) {
        df$`Pr(>Chi)`[1] <- test1$score$p[2]
        df$Deviance[1] <- test1$dev$dev[1]
        df$Rao[1] <- test1$score$chisq[1]
      }
      if (is.list(test2)) {
        df$`Pr(>Chi)`[2] <- test2$score$p[2]
        df$Deviance[2] <- test2$dev$dev[1]
        df$Rao[2] <- test2$score$chisq[1]
      }
      if (is.list(test3)) {
        df$`Pr(>Chi)`[3] <- test3$score$p[2]
        df$Deviance[3] <- test3$dev$dev[1]
        df$Rao[3] <- test3$score$chisq[1]
      }
      if (is.list(test4)) {
        df$`Pr(>Chi)`[4] <- test4$score$p[2]
        df$Deviance[4] <- test4$dev$dev[1]
        df$Rao[4] <- test4$score$chisq[1]
      }
      if (is.list(test5)) {
        df$`Pr(>Chi)`[5] <- test5$score$p[2]
        df$Deviance[5] <- test5$dev$dev[1]
        df$Rao[5] <- test5$score$chisq[1]
      }
      if (is.list(test6)) {
        df$`Pr(>Chi)`[6] <- test6$score$p[2]
        df$Deviance[6] <- test6$dev$dev[1]
        df$Rao[6] <- test6$score$chisq[1]
      }
    }
    
  }
  
  #return(df)
  return(list(df,dfRgr)) #returns a list that needs to be decomposed
  
} 