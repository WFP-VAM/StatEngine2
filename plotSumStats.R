if(!isNamespaceLoaded('survey')) library('survey') 
if(!isNamespaceLoaded('plyr')) library('plyr') 
if(!isNamespaceLoaded('RColorBrewer')) library('RColorBrewer') 

plotSumStats <- function(inputGUI,svyDsgnObj,plotNumber){

  #Change binary variable indicator to display dscrete variable because binary=discrere for following purpose
  if(inputGUI$varTypes[1]=='Bin'){
    inputGUI$varTypes[1] <- 'Dsc'
  }
  
  #Function to Create Mosaic Plot
  plotMosaic <- function(inputGUI,svyDsgnObj){
    
    varTypes <- inputGUI$varTypes
    
    #Construct formula from variables, turning continuous ones to discrete
    if (varTypes[1]=='Cnt'){
      fmlaStrng <- '~Hmisc::cut2(depVar,g=5)'
    } else {
      fmlaStrng <- '~depVar'
    }
    if (varTypes[2]=='Cnt'){
      fmlaStrng <- paste(fmlaStrng,'+Hmisc::cut2(indVar1,g=5)',sep='')
    } else {
      fmlaStrng <- paste(fmlaStrng,'+indVar1',sep='')
    }
    if (!is.na(varTypes[3])){
      if (varTypes[3]=='Cnt'){
        fmlaStrng <- paste(fmlaStrng,'+Hmisc::cut2(indVar2,g=5)',sep='')
      } else {
        fmlaStrng <- paste(fmlaStrng,'+indVar2',sep='')
      }
    }
    
    #Mosaic Plot
    par(mar=c(5, 4, 4, 2) + 0.1,las=0)
    plot(svytable(as.formula(fmlaStrng),svyDsgnObj),shade=TRUE,
         main='Mosaic Plot',xlab=inputGUI$depVarPrms[6],ylab=inputGUI$indVar1Prms[6])
    mtext(inputGUI$indVar2Prms[6],side=3,line=0)
  }
  
  #Function to create Scatter Plot
  plotScatter <- function(inputGUI,svyDsgnObj){
    
    varTypes <- inputGUI$varTypes
    rghtMarginShift <- 7
    
    if(varTypes[1]=='Cnt'){
      #If only 1 continuous independent variable
      if(varTypes[2]=='Cnt' & is.na(varTypes[3])){
        fmlaStrng <- 'depVar~indVar1'
        colorAssignVec <- NA
        rghtMarginShift <- 0
        xlbl <- inputGUI$indVar1Prms[6]
        
      #If both variables continuous
      } else if(varTypes[2]=='Cnt' & varTypes[3]=='Cnt'){
        fmlaStrng <- 'indVar1~depVar'
        indVar2Grp <- Hmisc::cut2(svyDsgnObj$variables$indVar2,g=5)
        colorDf <- data.frame(indVar2Grp = levels(indVar2Grp),
                              color = I(brewer.pal(nlevels(indVar2Grp), name = 'Set3')))
        colorAssignVec <- colorDf$color[match(indVar2Grp,colorDf$indVar2Grp)]
        xlbl <- inputGUI$indVar1Prms[6]
        
      #If 1st variable continuous, 2nd discrete
      } else if(varTypes[2]=='Cnt' & varTypes[3]=='Dsc'){
        fmlaStrng <- 'depVar~indVar1'
        nLvls <- nlevels(as.factor(svyDsgnObj$variables$indVar2))
        colorDf <- with(svyDsgnObj$variables,data.frame(indVar2 = levels(as.factor(indVar2)),
                                                       color = I(brewer.pal(nlevels(as.factor(indVar2)), name = 'Set3')[1:nLvls])))
        colorAssignVec <- colorDf$color[match(svyDsgnObj$variables$indVar2,colorDf$indVar2)]
        xlbl <- inputGUI$indVar1Prms[6]
                                        
      #If 2nd variable continuous, 1st discrete
      } else if(varTypes[2]=='Dsc' & varTypes[3]=='Cnt'){
        fmlaStrng <- 'depVar~indVar2'
        nLvls <- nlevels(as.factor(svyDsgnObj$variables$indVar1))
        colorDf <- with(svyDsgnObj$variables,data.frame(indVar1 = levels(as.factor(indVar1)),
                                                       color = I(brewer.pal(nlevels(as.factor(indVar1)), name = 'Set3')[1:nLvls])))
        colorAssignVec <- colorDf$color[match(svyDsgnObj$variables$indVar1,colorDf$indVar1)]
        xlbl <- inputGUI$indVar2Prms[6]
      }
      ylbl <- inputGUI$depVarPrms[6]
      
    } else if(varTypes[1]!='Cnt' & varTypes[2]=='Cnt' & varTypes[3]=='Cnt'){
      fmlaStrng <- 'indVar2~indVar1'
      nLvls <- nlevels(as.factor(svyDsgnObj$variables$depVar))
      colorDf <- with(svyDsgnObj$variables,data.frame(depVar = levels(as.factor(depVar)),
                                                      color = I(brewer.pal(nlevels(as.factor(depVar)), name = 'Set3')[1:nLvls])))
      colorAssignVec <- colorDf$color[match(svyDsgnObj$variables$depVar,colorDf$depVar)]
      xlbl <- inputGUI$indVar1Prms[6]
      ylbl <- inputGUI$indVar2Prms[6]
    }
    
    #Scatter Plot
    par(xpd = T, mar = par()$mar + c(0,0,0,rghtMarginShift))
    svyplot(as.formula(fmlaStrng),svyDsgnObj,bg = colorAssignVec,
            main='Scatter Plot',ylab=ylbl,xlab=xlbl,cex.lab=0.75,cex.axis=0.75)
    if(!is.na(varTypes[3])){
      legend("topright",inset=c(-0.08,0),col=colorDf[,2], pch=16,legend=colorDf[,1],cex=0.5)
    }
    par(mar=c(5, 4, 4, 2) + 0.1)
    
  }

  #Function to Create Box Plot
  plotBox <- function(inputGUI,svyDsgnObj){
   
    varTypes <- inputGUI$varTypes
    
    #Construct formula from variables, turning continuous ones to discrete
    fmlaStrng <- 'depVar~'
    if (!is.na(varTypes[3])){
      if (varTypes[2]=='Cnt'){
        fmlaStrng <- paste(fmlaStrng,'interaction(Hmisc::cut2(indVar1,g=5),',sep='')
      } else {
        fmlaStrng <- paste(fmlaStrng,'interaction(indVar1,',sep='')
      }
      if (varTypes[3]=='Cnt'){
        fmlaStrng <- paste(fmlaStrng,'Hmisc::cut2(indVar2,g=5))',sep='')
      } else {
        fmlaStrng <- paste(fmlaStrng,'indVar2)',sep='')
      }
    } else {
      if (varTypes[2]=='Cnt'){
        fmlaStrng <- paste(fmlaStrng,'Hmisc::cut2(indVar1,g=5)',sep='')
      } else {
        fmlaStrng <- paste(fmlaStrng,'indVar1',sep='')
      }
    }
    
    #Box Plot
    par(las=2,mar = par()$mar + c(3,0,0,0))
    svyboxplot(as.formula(fmlaStrng),svyDsgnObj,main='Box Plot',all.outliers = TRUE,
               ylab=inputGUI$depVarPrms[6],cex.axis=0.75,cex.lab=0.75,yaxt='n')
    x <- pretty(svyDsgnObj$variables$depVar)
    axis(side=2,sort(append(x,filter(x,rep(1/2,2),sides=2))),cex.axis=0.75)
    Hmisc::minor.tick(ny=4,tick.ratio=0.75)
    mtext(paste(inputGUI$indVar1Prms[6],'-X-',inputGUI$indVar2Prms[6],sep=''),
          side=1,line=7,cex=0.75,las=0)
    par(mar=c(5, 4, 4, 2) + 0.1,las=0)
  }
  
  #Create Bar Plot
  plotBar <- function(inputGUI,svyDsgnObj){
    
    varTypes <- inputGUI$varTypes
    
    #Construct formula from variables, turning continuous ones to discrete
    fmlaStrng <- '~'
    if (!is.na(varTypes[3])){
      if (varTypes[2]=='Cnt'){
        fmlaStrng <- paste(fmlaStrng,'Hmisc::cut2(indVar1,g=5)',sep='')
      } else {
        fmlaStrng <- paste(fmlaStrng,'indVar1',sep='')
      }
      if (varTypes[3]=='Cnt'){
        fmlaStrng <- paste(fmlaStrng,'+Hmisc::cut2(indVar2,g=5)',sep='')
      } else {
        fmlaStrng <- paste(fmlaStrng,'+indVar2',sep='')
      }
    } else {
      if (varTypes[2]=='Cnt'){
        fmlaStrng <- paste(fmlaStrng,'Hmisc::cut2(indVar1,g=5)',sep='')
      } else {
        fmlaStrng <- paste(fmlaStrng,'indVar1',sep='')
      }
    }
    
    #Bar Plot
    nLvls <- nlevels(as.factor(svyDsgnObj$variables$depVar))
    colorDf <- with(svyDsgnObj$variables,data.frame(depVar = levels(as.factor(depVar)),
                                                    color = I(brewer.pal(nlevels(as.factor(depVar)), name = 'Set3')[1:nLvls])))
    par(las=2,mar = par()$mar + c(3,0,0,4),xpd=TRUE)
    barplot(svyby(~depVar,as.formula(fmlaStrng),svyDsgnObj,svymean),
                  ylab=inputGUI$depVarPrms[6],col=colorDf$color,
                  cex.axis=0.75,cex.lab=0.75)
    Hmisc::minor.tick(ny=4,tick.ratio=0.75)
    mtext(paste(inputGUI$indVar1Prms[6],'-X-',inputGUI$indVar2Prms[6],sep=''),
          side=1,line=7,cex=0.75,las=0)
    legend("topright",inset=c(-0.02,0),col=colorDf[,2], pch=15,legend=colorDf[,1],cex=0.5)
    par(mar=c(5, 4, 4, 2) + 0.1,las=0)
    
  }


  if(plotNumber==1){
    plotMosaic(inputGUI,svyDsgnObj)
  } else if(plotNumber==2){
    if(inputGUI$varTypes[1]=='Cnt'){
      plotBox(inputGUI,svyDsgnObj)
    } else {
      plotBar(inputGUI,svyDsgnObj)
    }
  } else {
    if(sum(inputGUI$varTypes=='Cnt',na.rm=TRUE)>=2){
      plotScatter(inputGUI,svyDsgnObj)
    } else {
      print('No other plots available')
    }
  }
  
}
