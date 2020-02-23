library(tidyverse)
library(purrr)
rm(list=ls())



# nvar: number of endogenous variables
# nex:  number of exogenous variables in a SVAR model
# cte:  constant (1 if included, 0 otherwise)
# nlag: number of lags
# k:    dimension of matrix C in SVAR(p) notation is (k x nvar), last column is column of ones if constant is included
# m:    dimension of matrix Aplus in structural representation is (m x nvar)
infoVar <- list(nvar=4,nex=1,cte=1,nlag=2,k=nex+cte,m=nvar*nlag+k,T=70)

# maxDraws:      maximum number of importance sampling orthogonal-reduced-form (B,Sigma,Q) draws
# acceptedDraws: total number of accepted importance sampling draws
# total number of final importance sampling draws (effective size)
# iterShow: show drawing results after iterShow draws
# prior: put 'nwp' for normal-wishart prior or 'inwp' for independent normal-wishart prior 
# use 'inwp' prior for additional restrictions: Minnesota and block-exogeneity
infoSim <- list(prior='inwp',maxDraws=10000,acceptedDraws=1500,finalDraws=300,iterShow=50) 

# lambda1,lambda2,lambda3 and lamda4: hyperparameters for covariance matrix of reduced-form coefficients
# lambda5: hyperparameter for block-exogeneity
# standDev: calculate OLS residual variance of the auto-regressive models estimated for each variable
# firstLagCoef: mean prior, put 0.8 for stationary variables
infoMinnesota <- list(lambda1=0.1,lambda2=0.5,lambda3=1,lambda4=100,lambda5=0.001,standDev=0.2,firstLagCoef=0.8)

# zero and sign restrictions
impact <- list();
impact[["0"]]=matrix(c(1,1,0,0,1,-1,0,0,1,1,1,0,1,1,-1,0),nrow=infoVar$nvar,ncol=infoVar$nvar,byrow=TRUE)
impact[["long_run"]]=matrix(c(0,9,9,9,9,9,9,9,9,9,9,0,0,0,9,9),nrow=infoVar$nvar,ncol=infoVar$nvar,byrow=TRUE)
shockNames=c("a","b","c","d")
selectionRestr <- create_SZ(impact,shocks)
infoRestrictions <- list(shocks=shockNames,impacts=names(impact),signs=selectionRestr$sign,zeros=selectionRestr$zero)

# block exogeneity
# definition is to put 1 if there is restriction imposed (the same notation is used for selection matrices for zero restrictions)
make_BlockExoMatrix(info_var,2) 
userBlockExoMatrix <- matrix (c(0,0,1,1,0,0,1,1,0,0,0,0,0,0,0,0),nrow=infoVar$nvar,ncol=infoVar$nvar,byrow=TRUE) %>% t(.)




make_BlockExoMatrix <- function (infoVar,numFor) {
  output <- matrix(0,infoVar$nvar,infoVar$nvar)
  insert <- matrix(1,numFor,infoVar$nvar-numFor)
  rows <- c(((infoVar$nvar-numFor)+1):infoVar$nvar)
  cols <- c(1:(infoVar$nvar-numFor))
  output[rows,cols] <- insert;
  return(output)
}

create_SZ <- function (impact,shocks) {
  impacts <- impact %>% 
    do.call(cbind,.) %>% 
    t(.)
  sign_condition <- c(1,-1)
  SS <- apply(impacts,2,function(x) column_to_matrix(x,sign_condition)) 
  names(SS) <- shocks
  zero_condition <- c(0)
  ZZ <- apply(impacts,2,function(x) column_to_matrix(x,zero_condition)) 
  names(ZZ) <- shocks
  output <- list(SS,ZZ)
  names(output) <- c("sign","zero")
  return(output)
}
  
  
column_to_matrix <- function (x,condition) {
  # values: vector containing all sign restrictions (1 or -1) for a given vector (shock)
  values <- x[x %in% condition]
  size <- length(values)
  # col_positions: vector containing column positions to fill in matrix with corresponding values
  col_positions <- which(x %in% condition)
  # row_positions: vector containing row positions to fill in matrix with corresponding values 
  row_positions <- c(1:size)
  output_matrix <- matrix(0,size,length(x))
  if (identical(condition,c(1,-1))) {
    if (size>1) {
        diag(output_matrix[row_positions,col_positions]) <- values 
    }
    if (size==1) {
      output_matrix[row_positions,col_positions] <- values 
    }
  }
  
  if (identical(condition,c(0))) {
    if (size>1) {
      diag(output_matrix[row_positions,col_positions]) <- replicate(size,1) 
    }
    if (size==1) {
      output_matrix[row_positions,col_positions] <- replicate(size,1) 
    }
  };
  return(output_matrix)
}



 



            