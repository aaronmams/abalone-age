library(tree)

abalone.tree <- tree(ring~.,data=abalone)
ab.tree.class <- tree(factor(ring) ~.,data=abalone)
ab.poi <- glm(as.integer(ring)~.,data=abalone,family='poisson')

https://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeCrm7.htmlTable?topo[(34):1:(44)][(-127.0):1:(-117.0)]