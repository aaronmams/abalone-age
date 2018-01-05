library(dplyr)

#a function to convert depth from meters to fathoms
depth.to.fathoms <- function(var.name,df){
  idx <- which(colnames(df)==var.name)
  df$depth_fm <- df[,idx]*0.54
  return(df)
}


#a function to transform a continuous variable to a categorical one
continuous.to.factor <- function(var.name,cuts,df){
  names <- names(df)
  ncats <- length(cuts)-1
  df$cat <- cut(df[,which(colnames(df)==var.name)],cuts,labels=1:ncats)
  names(df) <- c(names,paste(var.name,"_","cat",sep=""))
  return(df)
}
