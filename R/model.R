rm(list=ls())
library(dplyr)
library(data.table)

#----------------------------------
source('functions/transform_fns.R')
source('functions/estimations.R')
#----------------------------------

#----------------------------------
#get data
abalone <- readRDS('data/abalone.RDA')
#----------------------------------

#----------------------------------
# transform data

abalone <- depth.to.fathoms(var.name='depth_meters',df=abalone)

abalone <- continuous.to.factor(var.name='lat',cuts=seq(38,39.06,by=0.06),df=abalone)
#----------------------------------

#----------------------------------
#model data
library(tree)

model1 <- treeclass.est(df=abalone)
size <- summary(model1)$size

#cross-validate some tree-based classifiers
tree.results <- data.frame(rbindlist(lapply(c(0:3),function(i){
   term.nodes <- size - i
   pct.correct <- cvtree.fn(formula=paste("factor(ring)~."),best=term.nodes,
                            train.pct=0.8,data=abalone)
   return(data.frame(tree_size=term.nodes,pct.correct=pct.correct))
})
))

#compare the tree based classifier to a poisson regression

#----------------------------------







