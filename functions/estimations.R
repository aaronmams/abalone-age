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
  glm(paste(formula),data=abalone,family='poisson')
  
}

ab.poi <- glm(as.integer(ring)~.,data=abalone,family='poisson')


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



#================================================