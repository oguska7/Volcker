########################
#        SCEN          #
########################
# Function to perform synthetic control estimation using elastic net regression
# with Breiman lambda corection

# Inputs: Treat - a vector of values for one treat (t by 1) 
#         Control - a matrix of values (t by k) for k controls
#         the stacking of data for Treat and Control: Y_pre, X1_pre, X2_pre..., Y_post, X1_post...
#         pre - index indicating last day of pretreat period (i.e 67th day from start)
#         alphaGrid - grid for alpha, can set it to 1 or 0 to get special case (LASSO/Ridge)
#         ncov - number of covariates (X's); excludes the outcome of interest (Y)
# Output: Alpha - hyperparameter in Elastic net as defined by Tibshirani
#         Lambda - hyperparameter in Elastic net as defined by Tibshirani
#         weights - weights used in a linear combination of control units to define SC
#         y_hat - predicted values of treat outcomes (Y) for all (pre and post) dates
#         placebo_gaps - gaps between predicted and true values of Y for the control units (placebo study)
#         P_value - P value (the treatment) as defined in Abadie (2010)



scen = function( Treat, Control, pre,ncov, alphaGrid = seq(0.1, 0.9, 0.1), nfolds = 5){
  set.seed(12345) 
  pre=pre*(1+ncov)
  Y_pre = as.vector(Treat[1:pre])
  X_pre = as.matrix(Control[1:pre, ])
  checker = as.data.frame(alphaGrid)
  for (i in 1:length(alphaGrid)) {
    fit     = cv.glmnet(x = X_pre, y = Y_pre,
                        alpha = alphaGrid[i],
                        standardize = FALSE,
                        intercept = TRUE,
                        nfolds = nfolds)
    checker$CV[i] = fit$cvm[fit$lambda == fit$lambda.1se] #- 1se - BC
    checker$la[i]=fit$lambda.1se
  }
  
  alpha = checker$alphaGrid[checker$CV == min(checker$CV)]
  CV = checker$CV_SE[checker$alphaGrid == alpha]
  lambda=checker$la[checker$alphaGrid == alpha]
  #Y_pre = as.vector(Treat[1:pre])
  Y_post = as.vector(Treat[-c(1:pre)])
  Y=c(Y_pre, Y_post)
  #X_pre = as.matrix(Control[1:pre, ])
  X_post = as.matrix(Control[-c(1:pre), ])
  X=rbind(X_pre, X_post)
 
  fit = glmnet(x = X_pre, y = Y_pre,
                       alpha = alpha,
                       lambda = lambda,
                       standardize = FALSE,
                       intercept = TRUE
                       #lower.limits = lower_limit_weights,
                       #upper.limits = upper_limit_weights)
  )
  w  = as.matrix(fit$beta)[, 1]
  o=cbind(Y[c(1:(pre/(ncov+1)), (pre+1):((pre+(length(Treat)-pre)/(ncov+1))))], predict(fit,newx=X,s=lambda)[c(1:(pre/(ncov+1)), (pre+1):((pre+(length(Treat)-pre)/(ncov+1))))])
#  matplot(1:199, o, type="l")

  ot=(predict(fit,newx=X,s=lambda)[c(1:(pre/(ncov+1)), (pre+1):((pre+(length(Treat)-pre)/(ncov+1))))] - Y[c(1:(pre/(ncov+1)), (pre+1):((pre+(length(Treat)-pre)/(ncov+1))))])
  # val$month=monthly
  # val$group="treat"
  # val$item="big" #colnames(OM_t)[2] ### fix
  # colnames(val)=c("val", "month", "group", "item")
  #ot=val-Treat[(1:(length(Treat)/(ncov+1)))] ##gap
  rmspe_treat=(sum((ot[((pre/(ncov+1))+1):((length(Treat)/(ncov+1)))])^2)/((length(Treat)/(ncov+1))-(pre/(ncov+1))))/((sum((ot[1:(pre/(ncov+1))])^2))/(pre/(ncov+1)))
  
  Alpha=alpha
  Lambda=lambda
  weights=w
  y_hat=o[, 1]
  
 #placebo study 
  o=matrix(0, nrow=(length(Treat)/(ncov+1)), ncol=ncol(Control))
   
  for (j in 1:ncol(Control)){
    set.seed(12345) #- to avoid variability in the results
    Y_pre = as.vector(Control[1:pre, j])
    X_pre = as.matrix(Control[1:pre, -c(j)])
    checker = as.data.frame(alphaGrid)
    for (i in 1:length(alphaGrid)) {
    fit     = cv.glmnet(x = X_pre, y = Y_pre,
                          alpha = alphaGrid[i],
                          standardize = FALSE,
                          intercept = TRUE,
                          nfolds = nfolds)
      checker$CV[i] = fit$cvm[fit$lambda == fit$lambda.1se] #- 1se 
      checker$la[i]=fit$lambda.1se
    }
    
    alpha = checker$alphaGrid[checker$CV == min(checker$CV)]
    CV = checker$CV_SE[checker$alphaGrid == alpha]
    lambda=checker$la[checker$alphaGrid == alpha]
    #Y_pre = as.vector(Control[1:pre, j])
    Y_post = as.vector(Control[-c(1:pre), j])
    Y=c(Y_pre, Y_post)
    #X_pre = as.matrix(Control[1:pre, -c(j)])
    X_post = as.matrix(Control[-c(1:pre), -c(j)])
    X=rbind(X_pre, X_post)
    #2. choose alpha based on minimum CVm
    
    
    fit = glmnet(x = X_pre, y = Y_pre,
                         alpha = alpha,
                         lambda = lambda,
                         standardize = FALSE,
                         intercept = TRUE#,
                         #pmax = max_number_units#,
                         #lower.limits = lower_limit_weights,
                         #upper.limits = upper_limit_weights)
    )
    w           = as.matrix(fit$beta)[, 1]
    o[, j] = predict(fit,newx=X,s=lambda)[c(1:(pre/(ncov+1)), (pre+1):((pre+(length(Treat)-pre)/(ncov+1))))]
    #matplot(1:199, o, type="l")
    
    
    
  }
  co=o-Control[c(1:(pre/(ncov+1)), (pre+1):((pre+(length(Treat)-pre)/(ncov+1)))), ]
  rmspe=matrix(0, nrow=ncol(co), ncol=1)
  for (i in 1:ncol(co)){
    rmspe[i]=(sum((co[((pre/(ncov+1))+1):((length(Treat)/(ncov+1))),i])^2)/((length(Treat)/(ncov+1))-(pre/(ncov+1))))/((sum((co[1:(pre/(ncov+1)),i])^2))/(pre/(ncov+1)))
    
  }
  rmspe=as.data.frame(rmspe)
  rmspe$I=ifelse(rmspe$V1>=rmspe_treat, 1, 0)
  rmspe_treat_p= (sum(rmspe$I)+1)/(length(rmspe$V1)+1)
  placebo_gaps=co
  P_value=rmspe_treat_p
  result=list(Alpha=Alpha, Lambda=Lambda, weights=weights, y_hat=y_hat,placebo_gaps=placebo_gaps,P_value=P_value)
  
  
}