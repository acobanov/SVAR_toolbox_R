library(tidyverse)
library(purrr)

# set working directory
setwd("C:/Users/Ante/Documents/work/SVAR_r")

# load functions
source("C:/Users/Ante/Documents/work/SVAR_r/helpfunctions/functions.R")

##--------------------------------------------------------------------------------------------------------------------------------------------------------------
## MODEL SETUP: 
##--------------------------------------------------------------------------------------------------------------------------------------------------------------
## Notation: Write data similar as in Rubio, Waggoner, and Zha (RES 2010)'s notation with modification: vector of exogenous variables z(t)  
## Label:  't' in xt,yt,et,zt,Aplust,etc means transponse
## Label: '(t)' in xt(t),yt(t),et(t),zt(t),etc means time
##
## SVAR(p):                    yt(t) A0 = yt(t-1) A1 + ... +yt(t-nlag) Anlag +zt(t) C + et(t)  for t=1...,T;
## structural rep.(AO,Aplus):  yt(t) A0 = xt(t) Aplus + et(t)  for t=1...,T;
## reduced-form rep.(B,Sigma): yt(t)= xt(t) B + ut(t)          for t=1...,T;
## where:                      xt(t) = [yt(t-1), ... , yt(t-nlag),zt(t)];
##                             Aplust(t)=[A1t, ... , Anlagt, Ct]
##                             B=Aplus(A0)^(-1) &  Sigma=(A0 A0t)^(-1)
## matrix notation:            Y = X*B + U;
## where:                      Y= [yt(nlag+1);yt(nlag+2); ... ;yt(end)]
##---------------------------------------------------------------------------------------------------------------------------------------------------------------
maxDraws       <-   10000  # maximum number of importance sampling orthogonal-reduced-form (B,Sigma,Q) draws
acceptedDraws  <-   1500   # total number of accepted importance sampling draws
iter_show      <-   50    # display iteration every iter_show draws
index          <-   40    # define  horizons for IRFs 
nlag           <-   2     # number of lags in a SVAR model
cte            <-   1     # set equal to 1 if a constant is included; 0 otherwise
##-----------------------------------------------------------------------------------------------------------------------------------------------------------------
## DEFINE ZERO AND SIGN RESTRICTIONS:
##-----------------------------------------------------------------------------------------------------------------------------------------------------------------
##  Define impact patern matrix (impact) and selection matrices: zero (Z) and sign (S) 
##  To define impact matrix you can define patern for: 'irf', 'structural' or 'both' 
##  If 'irf', user should define restrictions to any horizon of IRFs, use strings : '0', '1', '2', etc or 'long_run' 
##  If 'structural', user should define restrictions to structural matrices,use strings : 'A0','Aplus'
##----------------------------------------------------------------------------------------------------------------------------------------------------------------
## IMPORTANT!!!!!!!!!!!!
## Take a look at the F_map function, restrictions in vector of restrictions need  to be in this order: A0, Aplus, IRF
  
type_of_restrictions <- 'irf'    # set the type of restrictions: 'irf', 'structural' or 'both'
shocks <- c('HR demand','HR supply','EA demand', 'EA supply') # define shocks names
  
## number of objects in F(THETA) to which we impose sign and zero restrictios
num_of_restr <- length(restrictions)
## initialize impact pattern:
impact <- list()
  
## define restrictions (in matrix form) for each element in vector of restrictions
  
# set the restrictions  
impact[['0']] <- matrix(c(1,1,0,0,1,-1,0,0,9,9,1,1,9,9,1,-1),nrow=4,ncol=4,byrow=TRUE)
impact[['long_run']] <- matrix(c(0,9,9,9,9,9,9,9,9,9,0,9,9,9,9,9),nrow=4,ncol=4,byrow=TRUE) 

restrictions <- names(impact) 

# put all restrictions into one matrix (impacts)  
impacts <- impact  %>% 
           do.call(cbind,.) %>% 
           t(.)
            
Y <-impacts


lapply(Y,function(x) (x==1 | x==-1))



% calculate selection matrices: ZZ (zero) and SS (sign)
[SS,ZZ]=get_SZ(impacts);
  

  



