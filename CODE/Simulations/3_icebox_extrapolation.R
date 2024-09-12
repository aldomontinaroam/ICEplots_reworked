################################################################################
########## Trying to plot more interesting insights about extrapolation ########
################################################################################


### load R packages
library(ICEbox)
library(randomForest)
library(gbm)


#################### Extrapolation Detection ##########################
#################### (from the paper) #################################

#function that generates simulated data:
extrap_ex_sim = function(N,seednum=NULL){
  
  if(!is.null(seednum)){
    set.seed(seednum)
  }
  
  #helper that simulates one observation:
  sim1 = function(){
    s = runif(1)
    if(s <  (1/3)){
      x1 = runif(1,min=-1,max=0)
      x2 = runif(1,min=-1,max=0)
    }
    else{
      if( s < (2/3)){
        x1 = runif(1,min=-1,max=0)
        x2 = runif(1,min=0,max=1)
      }
      else{
        x1 = runif(1,min=0,max=1)
        x2 = runif(1,min=-1,max=0)
      }
    }
    return(cbind(x1,x2))
  } #end of helper fcn.
  
  #parameters:	
  b1=10; b2=1; sd=.1
  X = matrix(NA,ncol=2,nrow=N)
  
  for(i in 1:N){X[i,] = sim1()}
  
  Y = b1*X[,1]^2 + b2*(X[,2]>0)+rnorm(N,sd=sd)
  df = as.data.frame(cbind(Y,X))
  names(df) = c("Y","x1","x2")
  df
}

# generate data:
extrap_ex_data = extrap_ex_sim(1000)
X = extrap_ex_data[,2:3] #predictors are second and third columns
# fit a rf:
rf_mod = randomForest(Y~.,extrap_ex_data)

# Create an ICE object:
rf.ice_extrap = ice(rf_mod, X = X, predictor="x1", frac_to_build=1)

# Set up 'color_by' to visualize extrapoloations in X-space.
# x2_indic = 1 <==> no extrapolation 
rf.ice_extrap$Xice$x2_indic = ifelse(rf.ice_extrap$Xice$x2>0,0,1) 

# full ICE plot:
plot(rf.ice_extrap, plot_pdp=FALSE, color_by="x2_indic") 

###################### GBM for comparison ############################
###################### (my code) #####################################

# fitting a gbm

gbm_mod = gbm(Y~., data = extrap_ex_data, n.tree = 500, 
              interaction.depth = 3, shrinkage = 0.1, cv.folds = 5)
ntree = gbm.perf(gbm_mod, method = "cv") #498

# Create an ICE object:
rf.ice_extrap = ice(gbm_mod, X = X, predictor="x1", frac_to_build=1)

# Set up 'color_by' to visualize extrapoloations in X-space.
# x2_indic = 1 <==> no extrapolation 
rf.ice_extrap$Xice$x2_indic = ifelse(rf.ice_extrap$Xice$x2>0,0,1) 

# full ICE plot:
plot(rf.ice_extrap, plot_pdp=FALSE, color_by="x2_indic") 







