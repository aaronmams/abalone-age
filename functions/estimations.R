library(tree)


#===================================================
treeclass.est <- function(df){
  ab.tree <- tree(factor(ring)~.,data=df)
  return(ab.tree)
}

model.prune <- function(tree.obj,prune){
  ab.prune <- prune.tree(tree.obj,best=prune)
  return(ab.prune)
}
#=================================================


#=================================================
#Poisson estimator
poisson.fn <- function(y.name,x.name,data){
  formula <- paste(y.name,"~",paste(x.name, collapse='+'))
  poi.reg <- glm(paste(formula),data=abalone,family='poisson')
  return(poi.reg)
}



#=================================================

#================================================
#Cross Validation Functions
cvtree.fn <- function(train.pct,data,best,formula){
  train.idx <- sample(1:nrow(data),train.pct*nrow(data))
  train.df <- data[train.idx,]
  
  test.df <- abalone[-train.idx,]
  model <- tree(paste(formula),data=train.df)
  pruned.tree <- prune.tree(model,best=best)
  
  y.hat <- predict(pruned.tree,test.df,type="class")  
  
  pct.correct <- sum(as.numeric(test.df$ring==y.hat))/nrow(test.df)
return(pct.correct)  
}


#cross validate the poisson regressions
cvpoi.fn <- function(y.name,x.name,data,train.pct){

  train.idx <- sample(1:nrow(data),train.pct*nrow(data))
  train.df <- data[train.idx,]
  test.df <- abalone[-train.idx,]
  
  poi.model <-  poisson.fn(y.name=y.name,x.name=x.name,data=train.df)
  y.hat <- round(predict(poi.model,test.df,type='response'),digits=0)
  pct.correct <- sum(as.numeric(test.df$ring==y.hat))/nrow(test.df)

  return(pct.correct)  
} 
#================================================