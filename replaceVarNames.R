replaceVarNames <- function(df,rplcVarNames){
  
  depVarRplc <- rplcVarNames['depVar']
  indVar1Rplc <- rplcVarNames['indVar1']
  
  rowLbls <- rownames(df)
  textCols <- sapply(df,is.factor)|sapply(df,is.character)
  
  df[,textCols] <- as.data.frame(sapply(df[,textCols], gsub, pattern='depVar|DepVar', replacement=depVarRplc, fixed=FALSE))
  df[,textCols] <- as.data.frame(sapply(df[,textCols], gsub, pattern='indVar1|IndVar1', replacement=indVar1Rplc, fixed=FALSE))
  gsub("TRUE", "", colnames(df))
  rownames(df) <- rowLbls
  
  colnames(df) <- gsub('depVar|DepVar',depVarRplc,colnames(df), fixed=FALSE)
  colnames(df) <- gsub('indVar1|IndVar1',indVar1Rplc,colnames(df), fixed=FALSE)
  
  rownames(df) <- gsub('depVar|DepVar',depVarRplc,rownames(df), fixed=FALSE)
  rownames(df) <- gsub('indVar1|IndVar1',indVar1Rplc,rownames(df), fixed=FALSE)
  
  if (!is.na(inputGUI$indVar2Prms[1])){
    indVar2Rplc <- rplcVarNames['indVar2']
    rowLbls <- rownames(df)
    df[,textCols] <- as.data.frame(sapply(df[,textCols], gsub, pattern='indVar2|IndVar2', replacement=indVar2Rplc, fixed=FALSE))
    rownames(df) <- rowLbls
    colnames(df) <- gsub('indVar2|IndVar2',indVar2Rplc,colnames(df), fixed=FALSE)
    rownames(df) <- gsub('indVar2|IndVar2',indVar2Rplc,rownames(df), fixed=FALSE)
  }
 
  return(df) 
}
  
  
  
  
  
  
  
  
  
  
  
  
  
