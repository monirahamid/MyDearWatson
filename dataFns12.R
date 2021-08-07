orderTable <- function(dataTable,nVar,incDec,orderFixed) {
  # Create cyclic permutation in order to sort all columns
  perm=c(nVar,1:(nVar-1))
  # Create string to reference columns
  commaString = strrep(",",(nVar-1))
  # Put all variables in decreasing order
  for (jj in 1:nVar){
    # Sort rows if there's more than 1
    if (nrow(dataTable)>1 & orderFixed[1]==FALSE){
      # Sum over all columns but the first
      sumsTmp = rowSums(dataTable)
      # Sort the results
      orderTmp=order(incDec[1]*sumsTmp)
      # Reorder the table
      ## Note "drop=FALSE' prevents the removal of a dimension if there's only 1 level
      commandString = paste(
        "dataTable=dataTable[orderTmp",commaString,",drop=FALSE]")
      eval(parse(text=commandString))
    }
    # Continue cycling to do all columns one by one
    dataTable=aperm(dataTable,perm)
    orderFixed=orderFixed[perm]
    incDec = incDec[perm]
  }
  return(dataTable)
}


# Original TSP function
TSPorderOrig <- function(dataTablePct) {
  dists=dist(t(dataTablePct), method="manhattan")
  distMx=as.matrix(dists)
  # Create TSP object
  tsp=TSP(dists)
  bestLen = 1000000000
  # solve TSP
  nData=ncol(dataTablePct)
  for (jj in 1:nData){
    tmp=solve_TSP(tsp,method="cheapest_insertion",start=jj) # Also can use NN
    tmpLabels=labels(tmp)
    tmpLen= tour_length(tmp)-distMx[tmpLabels[1],tmpLabels[nData]]
    if (tmpLen<bestLen) {
      tour=tmp
      bestLen=tmpLen
    }
  }
  # New order of bars according to TSP
  return(labels(tour))
}


#Modified TSP function--finds shortest linear tour. Uses random insertion method.
TSPorder <- function(dataTablePct) {
  dists=dist(t(dataTablePct), method="manhattan")
  distMx=as.matrix(dists)
  addCol = matrix(0L, nrow = dim(distMx)[1], ncol = 1)
  distMx = cbind(distMx,addCol)
  addRow = matrix(0L, nrow = 1, ncol = dim(distMx)[2])
  distMx = rbind(distMx,addRow)
  tsp=TSP(distMx)
  tour=solve_TSP(tsp,method="arbitrary_insertion",repetitions=ncol(distMx),two_opt=TRUE)
  # bestLen = tour_length(tour)
  # # Try to improve it
  # for (jj in 1:dim(distMx)[1]){
  #   tmp=solve_TSP(tsp,method="arbitrary_insertion",start=ncol(distMx))
  #   tmpLen = tour_length(tmp)
  #   if (tmpLen<bestLen) {
  #     tour=tmp
  #     bestLen=tmpLen
  #   }
  # }
  tourLabels = labels(tour)
  ixTmp = match("",tourLabels)
  if (ixTmp>1){
    tourLabels=append(tourLabels[ixTmp:length(tourLabels)],tourLabels[1:ixTmp-1])
  }
  # New order of bars according to TSP
  return(tourLabels[-1])
}

## Create legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat,diag=TRUE)] <- 0
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat,diag=TRUE)]<- 0
  return(cormat)
}
