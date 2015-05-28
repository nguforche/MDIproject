#' Train, Validate and Test various models  
#' 
#' Functions for training, validation and test a number of machine 
#' learning classification models with or without parameter tuning 
#' @name Train 
#' @param classifier character list of classification models. See names(TrainAllModels()). 
#' @param  X.trn,X.val,X.tst matrix of predictors 
#' @param  Y.trn,Y.val,Y.tst matrix of true binary 0,1 class
#' @param form formula 
#' @param  res MTL model  
#' @param  para,STL.para named list of model parameters 
#' @param  opt.para logical for parameter tuning. defaul to FALSE
#' @param  return.model logical to return trained model or not 
#' @param \dots further arguments passed to or from other methods.
#' @return list of functions for training various algorithms 
#' @author  Che Ngufor <Ngufor.Che@@mayo.edu>
#' @import ELR caret randomGLM
#' @importFrom PresenceAbsence optimal.thresholds
#' @importFrom PresenceAbsence presence.absence.accuracy
#'
NULL 
#' @rdname Train 
#' @export
#' @examples
#' TrainModels <- lapply(TrainAllModels(), function(x) x) 
#'
TrainAllModels <- function(){
res = list(
ELR = function(X, Y, X.val, X.tst, para, opt.para = FALSE, ...){ 
	result <- vector("list", length = 5)
	names(result) <- c("trn", "val", "tst", "main.model", "para")
	class(result) <- "ELR"	
	result$para <- para 
	ELR.para = para$ELR 		
	rhs.vars <- colnames(X)
	resp <- colnames(Y) 		
        dt <- cbind.data.frame(resp= Y[,1], X)
	colnames(dt) <- c(resp, rhs.vars) 
        form <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+"))) 
                
		if(opt.para) {  		   				
		result$main.model <- ELRgridsearch(form, dt,para=ELR.para)
		result$para$ELR <- result$main.model$para 
		} else {
		result$main.model <- ELR(form, dt,  para=ELR.para)
		}
		pp <- predict(result$main.model, X)$prob[, 2]	
		result$trn <- pp		
		pp <- predict(result$main.model, X.val)$prob[, 2]
		result$val <- pp		
		pp <- predict(result$main.model, X.tst)$prob[, 2]
		result$tst <- pp 			
        return(result)
},
RUSBoostELR = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <-c("trn", "val", "tst", "main.model", "para") 
	class(result) <- "RUSBoostELR"
	result$para <- para 		
	rhs.vars <- colnames(X)
	resp <- colnames(Y)

	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      
        dt <- cbind.data.frame(resp= Y[,1], X)
	dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, 1, 0))
	colnames(dt) <- c(resp, rhs.vars)
        iters = para$RUSBoost$iters
        	
	if(para$ELR.para$opt.para) { 
	ix <- sample(nrow(dt), floor(nrow(dt)*0.75))
        dat.trn <- dt[ix, , drop = FALSE]
        dat.val <- dt[-ix, , drop = FALSE]
        idx <- dat.trn[, resp] == 0
        xx <- para$RUSBoost$grid
        error <- c()
        mod <- list()
        for(ii in 1:length(xx)){
	mod[[ii]] <- RUSBoostELR(form=formula.string, data = dat.trn, boot = FALSE, 
	             iters = iters, coeflearn = "Breiman", sampleFraction = xx[ii], 
	             para=para$ELR.para, idx=idx)
	error <- c(error, predict(mod[[ii]], newdata = dat.val)$accuracy$AUC)
	}	
	ix <- which.max(error)
	para$RUSBoost$sampleFraction <- xx[ix]
        idx <- dt[, resp] == 0        
        result$main.model <- RUSBoostELR(form=formula.string, data = dt, boot = FALSE, 
                              iters =iters, coeflearn = "Breiman", sampleFraction = xx[ix], 
                              para=para$ELR.para, idx=idx)	
        } else {
         idx <- dt[, resp] == 0
        result$main.model <- RUSBoostELR(form=formula.string, data = dt, boot = FALSE, 
                              iters =iters, coeflearn = "Breiman", 
                              sampleFraction = para$RUSBoost$sampleFraction, 
                              para=para$ELR.para, idx=idx)
         }	
        pp <-  predict(result$main.model, newdata = X)$stack.prob[,2]        
        pp <- ifelse( abs(pp -1) <= 1e-16, pp-1e-16, ifelse(pp <= 1e-16, pp+1e-16, pp))                 
        result$trn <- pp          
        pp <-  predict(result$main.model, newdata = X.val)$stack.prob[,2]        
        pp <- ifelse( abs(pp -1) <= 1e-16, pp-1e-16, ifelse(pp <= 1e-16, pp+1e-16, pp))                 
        result$val <- pp                                                           
        pp <-  predict(result$main.model, newdata = X.tst)$stack.prob[,2] 
        result$tst <- pp    
        pp <- ifelse( abs(pp -1) <= 1e-16, pp-1e-16, ifelse(pp <= 1e-16, pp+1e-16, pp))                         
	return(result)
},
GLM = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <-c("trn", "val", "tst", "main.model", "para") 
	class(result) <- "GLM"
	result$para <- para 
			
	rhs.vars <- colnames(X)
	resp <- colnames(Y)	
	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      

        fitControl <- trainControl(method = "none", classProbs = TRUE)
	dt <- cbind.data.frame(resp= Y[,1], X)
	dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, "Yes", "No"))
	colnames(dt) <- c(resp, rhs.vars)
        result$main.model <-  train(formula.string, data = dt, method = "glm", family = "binomial", 
                                     trControl = fitControl, metric = "ROC")     
        pp <-  predict(result$main.model, newdata = X, type = "prob")[,2]        
        result$trn <- pp          
        pp <-  predict(result$main.model, newdata = X.val, type = "prob")[,2]        
        result$val <- pp                                                           
        pp <-  predict(result$main.model, newdata = X.tst, type = "prob")[,2] 
        result$tst <- pp    
	return(result)
},
RUSBoostGLM = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <-c("trn", "val", "tst", "main.model", "para") 
	class(result) <- "RUSBoostGLM"
	result$para <- para 		
	rhs.vars <- colnames(X)
	resp <- colnames(Y)

	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      

        dt <- cbind.data.frame(resp= Y[,1], X)
	dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, 1, 0))
	colnames(dt) <- c(resp, rhs.vars)
        iters = para$RUSBoost$iters
        	
	if(para$boot.opt.para) { 
	ix <- sample(nrow(dt), floor(nrow(dt)*0.75))
        dat.trn <- dt[ix, , drop = FALSE]
        dat.val <- dt[-ix, , drop = FALSE]
        idx <- dat.trn[, resp] == 0
        xx <- para$RUSBoost$grid
        error <- c()
        mod <- list()
        for(ii in 1:length(xx)){
	mod[[ii]] <- RUSBoostGLM(formula=formula.string, data = dat.trn, boot = para$boot, 
	             iters = iters, coeflearn = "Breiman", sampleFraction = xx[ii], idx=idx)
	error <- c(error, predict(mod[[ii]], newdata = dat.val)$accuracy$AUC)
	}	
	ix <- which.max(error)
	para$RUSBoost$sampleFraction <- xx[ix]
        idx <- dt[, resp] == 0        
        result$main.model <- RUSBoostGLM(formula=formula.string, data = dt, boot = para$boot, 
                              iters =iters, coeflearn = "Breiman", sampleFraction = xx[ix], idx=idx)	
        } else {
         idx <- dt[, resp] == 0
        result$main.model <- RUSBoostGLM(formula=formula.string, data = dt, boot = para$boot, 
                              iters =iters, coeflearn = "Breiman", 
                              sampleFraction = para$RUSBoost$sampleFraction, 
                              idx=idx)
         }	
        pp <-  predict(result$main.model, newdata = X)$w.prob[,2]        
        result$trn <- pp          
        pp <-  predict(result$main.model, newdata = X.val)$w.prob[,2]        
        result$val <- pp                                                           
        pp <-  predict(result$main.model, newdata = X.tst)$w.prob[,2] 
        result$tst <- pp    
	return(result)
},
#### random forest  
RF = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <- c("trn", "val", "tst", "main.model", "para")
	class(result) <- "RF"	
	rhs.vars <- colnames(X)
	resp <- colnames(Y) 
	result$para <- para 	
	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      

	dt <- cbind.data.frame(resp= Y[,1], X)
	dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, "Yes", "No"))	
	colnames(dt) <- c(resp, rhs.vars)	
	if(opt.para) { 
	fitControl <- trainControl(method = para$method, number = para$number,classProbs = TRUE, 
	                           verboseIter = FALSE, summaryFunction = twoClassSummary)	
	result$main.model <-  train(X, dt[, resp],  method = "rf", trControl = fitControl,
	                      tuneLength = para$tuneLength, metric = "ROC", ntree = para$RF$rf.ntree,
	                      importance=FALSE)                                                     
        result$para$RF$rf.mtry  <-  result$main.model$bestTune$mtry                 
        } else{
	fitControl <- trainControl(method = "none", classProbs = TRUE)
	result$main.model <-  train(X, dt[, resp],  method = "rf", trControl = fitControl,
                                    verboseIter = FALSE, tuneGrid = data.frame(mtry = para$RF$rf.mtry), 
                                    metric = "ROC")        
          }    
        pp <-  predict(result$main.model, newdata = X, type = "prob")[,2] 
        result$trn <- pp                       
        pp <-  predict(result$main.model, newdata = X.val, type = "prob")[,2] 
        result$val <- pp
        pp <- predict(result$main.model, newdata =X.tst, type = "prob")[,2] 
        result$tst <- pp  
	return(result)
},
#### random generalized linea Models  
RGLM = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <- c("trn", "val", "tst", "main.model", "para")
	class(result) <- "RGLM"	
	rhs.vars <- colnames(X)
	resp <- colnames(Y) 
	result$para <- para 	
	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      	
	Y <- factor(ifelse(Y[, 1] == 1, "Yes", "No"))		 
	result$main.model <-  randomGLM(x=X, y=Y, classify=TRUE, nBags = para$RGLM$nBags, 
	                       keepModels=TRUE, maxInteractionOrder = para$RGLM$ninteract, 
	                       nCandidateCovariates = ncol(X))                
 
        pp <-  predict(result$main.model, newdata = X, type = "response")[,2] 
        result$trn <- pp                       
        pp <-  predict(result$main.model, newdata = X.val, type = "response")[,2] 
        result$val <- pp
        pp <- predict(result$main.model, newdata =X.tst, type = "response")[,2] 
        result$tst <- pp  
	return(result)
},

#### GBM   
GBM = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <- c("trn", "val", "tst", "main.model", "para")
	class(result) <- "GBM"	
	rhs.vars <- colnames(X)
	resp <- colnames(Y) 
	result$para <- para 	
	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      

	dt <- cbind.data.frame(resp= Y[,1], X)
	dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, "Yes", "No"))	
	colnames(dt) <- c(resp, rhs.vars)	
	if(opt.para) { 
#	fitControl <- trainControl(method = para$method, number = para$number,classProbs = TRUE, 
#	                           verboseIter = FALSE, summaryFunction = twoClassSummary)
	fitControl <- trainControl(method = para$method, number =  para$number)                           	
	result$main.model <-  train(X, dt[, resp],  method = "gbm", trControl = fitControl,
                                    verbose = FALSE, tuneLength = para$tuneLength) 
                                                                                        
        result$para$GBM$gbm.n.trees  <-  result$main.model$bestTune$n.trees
        result$para$GBM$interaction.depth  <-  result$main.model$bestTune$interaction.depth 
        result$para$GBM$shrinkage  <-  result$main.model$bestTune$shrinkage
                        
        } else{
	fitControl <- trainControl(method = "none", classProbs = TRUE)
	result$main.model <-  train(X, dt[, resp],  method = "gbm", trControl = fitControl,
                                    verbose = FALSE, tuneGrid = data.frame(n.trees = para$GBM$gbm.n.trees, 
                                    interaction.depth=para$GBM$interaction.depth, shrinkage=para$GBM$shrinkage), 
                                    metric = "ROC")
         } 
        pp <-  predict(result$main.model, newdata = X, type = "prob")[,2] 
        result$trn <- pp                          
        pp <-  predict(result$main.model, newdata = X.val, type = "prob")[,2] 
        result$val <- pp
        pp <- predict(result$main.model, newdata =X.tst, type = "prob")[,2] 
        result$tst <- pp  
        result$main.model$task.type <- "class"
	return(result)
},
#### RUSBoost with GBM 
RUSBoostGBM = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <-c("trn", "val", "tst", "main.model", "para") 
	class(result) <- "RUSBoostGBM"
	result$para <- para 		
	rhs.vars <- colnames(X)
	resp <- colnames(Y)

	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+"))) 
        dt <- cbind.data.frame(resp= Y[,1], X)
	dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, 1, 0))
	colnames(dt) <- c(resp, rhs.vars)
        iters = para$RUSBoost$iters
        	
	if(para$boot.opt.para) { 
	ix <- sample(nrow(dt), floor(nrow(dt)*0.75))
        dat.trn <- dt[ix, , drop = FALSE]
        dat.val <- dt[-ix, , drop = FALSE]
        idx <- dat.trn[, resp] == 0
        xx <- para$RUSBoost$grid
        error <- c()
        mod <- list()
        for(ii in 1:length(xx)){
	mod[[ii]] <- RUSBoostGBM(formula=formula.string, data = dat.trn, boot = para$boot, 
	             iters = iters, coeflearn = "Breiman", sampleFraction = xx[ii], 
	             para=para$GBM.para, idx=idx)
	error <- c(error, predict(mod[[ii]], newdata = dat.val)$accuracy$AUC)
	}	
	ix <- which.max(error)
	para$RUSBoost$sampleFraction <- xx[ix]
        idx <- dt[, resp] == 0        
        result$main.model <- RUSBoostGBM(formula=formula.string, data = dt, boot = para$boot, 
                              iters =iters, coeflearn = "Breiman", sampleFraction = xx[ix], 
                              para=para$GBM.para, idx=idx)	
        } else {
         idx <- dt[, resp] == 0
        result$main.model <- RUSBoostGBM(formula=formula.string, data = dt, boot = para$boot, 
                              iters =iters, coeflearn = "Breiman", 
                              sampleFraction = para$RUSBoost$sampleFraction, 
                              para=para$GBM.para, idx=idx)
         }	
        pp <-  predict(result$main.model, newdata = X)$stack.prob[,2]        
        result$trn <- pp          
        pp <-  predict(result$main.model, newdata = X.val)$stack.prob[,2]        
        result$val <- pp                                                           
        pp <-  predict(result$main.model, newdata = X.tst)$stack.prob[,2] 
        result$tst <- pp    
	return(result)
},
#### FDA  
FDA = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
  result <- vector("list", length = 5)
  names(result) <- c("trn", "val", "tst", "main.model", "para")
  class(result) <- "FDA"	
  rhs.vars <- colnames(X)
  resp <- colnames(Y) 
  result$para <- para 	
  formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      
  
    dt <- cbind.data.frame(resp= Y[,1], X)
    dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, "Yes", "No"))	
    colnames(dt) <- c(resp, rhs.vars)	
    if(opt.para) { 
      fitControl <- trainControl(method = para$method, number = para$number,classProbs = TRUE, 
                           	       verboseIter = FALSE, summaryFunction = twoClassSummary)
      fitControl <- trainControl(method = para$method, number =  para$number)                           	
                                 	
      result$main.model <-  train(X, dt[, resp],  method = "fda", trControl = fitControl,
                                  tuneLength = para$tuneLength)       
      result$para$FDA$nprune  <-  result$main.model$bestTune$nprune
      result$para$FDA$degree  <-  result$main.model$bestTune$degree             
    } else{
      fitControl <- trainControl(method = "none", classProbs = TRUE)
      result$main.model <-  train(X, dt[, resp],  method = "fda", trControl = fitControl,
                                  tuneGrid = data.frame(nprune = para$FDA$nprune, 
                                                        degree=para$FDA$degree), 
                                  metric = "ROC")
    } 
    pp <-  predict(result$main.model, newdata = X, type = "prob")[,2] 
    result$trn <- pp                          
    pp <-  predict(result$main.model, newdata = X.val, type = "prob")[,2] 
    result$val <- pp
    pp <- predict(result$main.model, newdata =X.tst, type = "prob")[,2] 
    result$tst <- pp  
    return(result)
},

#### FDA  
bagFDA = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
  result <- vector("list", length = 5)
  names(result) <- c("trn", "val", "tst", "main.model", "para")
  class(result) <- "bagFDA"	
  rhs.vars <- colnames(X)
  resp <- colnames(Y) 
  result$para <- para 	
  formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      
  
    dt <- cbind.data.frame(resp= Y[,1], X)
    dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, "Yes", "No"))	
    colnames(dt) <- c(resp, rhs.vars)	
    if(opt.para) { 
#      fitControl <- trainControl(method = para$method, number = para$number,classProbs = TRUE, 
#                                 verboseIter = FALSE, summaryFunction = twoClassSummary)
      fitControl <- trainControl(method = para$method, number =  para$number)                           	
                                 	
      result$main.model <-  train(X, dt[, resp],  method = "bagFDA", trControl = fitControl,
                                  tuneLength = para$tuneLength) 
      
      result$para$bagFDA$nprune  <-  result$main.model$bestTune$nprune
      result$para$bagFDA$degree  <-  result$main.model$bestTune$degree       
      
    } else{
      fitControl <- trainControl(method = "none", classProbs = TRUE)
      result$main.model <-  train(X, dt[, resp],  method = "bagFDA", trControl = fitControl,
                                  tuneGrid = data.frame(nprune = para$bagFDA$nprune, 
                                                        degree=para$bagFDA$degree), 
                                  metric = "ROC")
    } 
    pp <-  predict(result$main.model, newdata = X, type = "prob")[,2] 
    result$trn <- pp                          
    pp <-  predict(result$main.model, newdata = X.val, type = "prob")[,2] 
    result$val <- pp
    pp <- predict(result$main.model, newdata =X.tst, type = "prob")[,2] 
    result$tst <- pp  
    result$main.model$task.type <- "class"
  return(result)
},
#### neural networks  
NNET = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <- c("trn", "val", "tst", "main.model", "para")
	class(result) <- "NNET"	
	rhs.vars <- colnames(X)
	resp <- colnames(Y) 
	result$para <- para 	
	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      

	dt <- cbind.data.frame(resp= Y[,1], X)
	dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, "Yes", "No"))	
	colnames(dt) <- c(resp, rhs.vars)	
	if(opt.para) { 
	fitControl <- trainControl(method = para$method, number = para$number,classProbs = TRUE, 
	              verboseIter = FALSE, summaryFunction = twoClassSummary)	
	result$main.model <-  train(X, dt[, resp],  method = "nnet", 
	                       trControl = fitControl,tuneLength = para$tuneLength, 
	                       metric = "ROC",  maxit = para$NNET$maxit,  
                                    MaxNWts = 10000, trace = FALSE)                          
                           
        result$para$NNET$nnet.size  <-  result$main.model$bestTune$size 
        result$para$NNET$nnet.decay <-  result$main.model$bestTune$decay                
        } else{
	fitControl <- trainControl(method = "none", classProbs = TRUE)
	result$main.model <-  train(X, dt[, resp],  method = "nnet", 
	                       trControl = fitControl,
                               verbose = FALSE, tuneGrid = 
                               data.frame(size = para$NNET$nnet.size, 
                               decay=para$NNET$nnet.decay),metric = "ROC", 
                               maxit = para$NNET$maxit, MaxNWts = 10000, 
                               trace = FALSE)                          	
	}                   
        pp <-  predict(result$main.model, newdata = X, type = "prob")[,para$cls] 
        result$trn <- pp
        pp <- predict(result$main.model, newdata =X.val, type = "prob")[,para$cls] 
        result$val <- pp  
        pp <- predict(result$main.model, newdata =X.tst, type = "prob")[,para$cls] 
        result$tst <- pp          
	return(result)
},
#### support vector machines 
SVM = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <-  c("trn", "val", "tst", "main.model", "para")
	class(result) <- "SVM"	
	rhs.vars <- colnames(X)
	resp <- colnames(Y) 
	result$para <- para 
	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+")))      

	dt <- cbind.data.frame(resp= Y[,1], X)
	dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, "Yes", "No"))	
	colnames(dt) <- c(resp, rhs.vars)
	
	if(opt.para) { 
	fitControl <- trainControl(method = para$method, 
	number = para$number,classProbs = TRUE, verboseIter = FALSE, 
	summaryFunction = twoClassSummary)	
	result$main.model <-  train(formula.string, data = dt,  
	method = para$svm.type, trControl = fitControl, verbose = FALSE, 
	                           tuneLength = para$tuneLength,
	                           metric = "ROC", scaled = TRUE)                                                                    
        result$para$SVM$svm.C <-  result$main.model$bestTune$C 
        result$para$SVM$svm.sigma <- result$main.model$bestTune$sigma  
                      
        } else{
	fitControl <- trainControl(method = "none", classProbs = TRUE)
	result$main.model <-  train(formula.string, data = dt,  
	method = para$svm.type, trControl = fitControl,
        verbose = FALSE, tuneGrid = data.frame(C = para$SVM$svm.C, 
        sigma=para$SVM$svm.sigma),metric = "ROC", scaled = FALSE)                          	
	}                   
        pp <-  predict(result$main.model, newdata = X, type = "prob")[,2] 
        result$trn <- pp
        pp <- predict(result$main.model, newdata =X.val, type = "prob")[,2] 
        result$val <- pp
        pp <- predict(result$main.model, newdata =X.tst, type = "prob")[,2] 
        result$tst <- pp          
	return(result)
}, 
#### RUSBoost with bagFDA 
RUSBoostFDA = function(X, Y, X.val, X.tst, para, opt.para = FALSE){
	result <- vector("list", length = 5)
	names(result) <-c("trn", "val", "tst", "main.model", "para") 
	class(result) <- "RUSBoostFDA"
	result$para <- para 		
	rhs.vars <- colnames(X)
	resp <- colnames(Y)

	formula.string <- as.formula(paste0(paste0(resp, " ~"), paste0(rhs.vars, collapse= "+"))) 
	     
        dt <- cbind.data.frame(resp= Y[,1], X)
	dt[, "resp"] <- factor(ifelse(dt[, "resp"] == 1, 1, 0))
	colnames(dt) <- c(resp, rhs.vars)
        iters = para$RUSBoost$iters
        	
	if(para$boot.opt.para) { 
	ix <- sample(nrow(dt), floor(nrow(dt)*0.75))
        dat.trn <- dt[ix, , drop = FALSE]
        dat.val <- dt[-ix, , drop = FALSE]
        idx <- dat.trn[, resp] == 0
        xx <- para$RUSBoost$grid
        error <- c()
        mod <- list()
        for(ii in 1:length(xx)){
	mod[[ii]] <- RUSBoostFDA(formula=formula.string, data = dat.trn, boot = para$boot, 
	             iters = iters, coeflearn = "Breiman", sampleFraction = xx[ii], 
	             para = para$FDA.para, idx=idx)
	error <- c(error, predict.RUSBoostFDA(mod[[ii]], newdata = dat.val)$accuracy$AUC)
	}	
	ix <- which.max(error)
	para$RUSBoost$sampleFraction <- xx[ix]
        idx <- dt[, resp] == 0        
        result$main.model <- RUSBoostFDA(formula=formula.string, data = dt, boot = para$boot, 
                              iters =iters, coeflearn = "Breiman", sampleFraction = xx[ix], 
                              para = para$FDA.para, idx=idx)	
        } else {
         idx <- dt[, resp] == 0
        result$main.model <- RUSBoostFDA(formula=formula.string, data = dt, boot = para$boot, 
                              iters =iters, coeflearn = "Breiman", 
                              sampleFraction = para$RUSBoost$sampleFraction, 
                              para = para$FDA.para, idx=idx)
         }	
        pp <-  predict.RUSBoostFDA(result$main.model, newdata = X)$w.prob[,2]        
        result$trn <- pp          
        pp <-  predict.RUSBoostFDA(result$main.model, newdata = X.val)$w.prob[,2]        
        result$val <- pp                                                           
        pp <-  predict.RUSBoostFDA(result$main.model, newdata = X.tst)$w.prob[,2] 
        result$tst <- pp    
	return(result)
}
)
return(res)
}
#' @rdname Train 
#' @export 
Train.Validate.Test <- function(classifier, X.trn, Y.trn, X.val, Y.val, X.tst, Y.tst,  
                           para, opt.para = FALSE, return.model = FALSE){
                           
Train.Models <- lapply(TrainAllModels(), function(x) x)                           
mod <- Train.Models[[classifier]](X.trn, Y.trn, X.val, X.tst, para, opt.para)
collect.garbage()
model <- mod$main.model
para =  mod$para
trn.pred <- mod$trn
tst.pred <- mod$tst
val.pred <- mod$val

val.obs <- ifelse(Y.val[, 1] == 1, 1, 0)
tst.obs <-  ifelse(Y.tst[, 1] == 1, 1, 0)
### get optimal threshold using pedicted training probabilities and training performance 
val.perf <- Performance(pred=val.pred, obs=val.obs, prevalence = para$prior)
### get optimal thresholds
thresh <- as.numeric(val.perf$threshold)
para$thresh = thresh 

#### performance on test set using optimal thresholds 
tst.perf <-  Performance(pred=tst.pred, obs=tst.obs, prevalence = para$prior, 
                         threshold=thresh)                         
#### performance using percentiles of training set                          
pct = quantile(trn.pred, para$percentile)

tst.perf.pct <- do.call(rbind.data.frame, lapply(pct, function(x) 
Performance(pred=tst.pred, obs=tst.obs,prevalence = val.perf$Obs.Prevalence, 
                         threshold=x)))
tst.perf.pct <- cbind.data.frame(percentile = names(pct), tst.perf.pct) 
if(return.model)                        
res <- list(model = model, para = mod$para, pred.prob = tst.pred, true = tst.obs, 
           perf = list(val.perf = val.perf, tst.perf=tst.perf, tst.perf.pct=tst.perf.pct))
else 
res <- list(para = mod$para, perf = list(val.perf = val.perf, tst.perf=tst.perf, tst.perf.pct=tst.perf.pct))
return(res)
}
#' @rdname Train 
#' @export 
Train.Validate.Test.Boot <- function(classifier, X.trn, Y.trn, X.val, Y.val, X.tst, Y.tst,  
                           STL.para, opt.para = FALSE, return.model = FALSE){

Train.Models <- lapply(TrainAllModels(), function(x) x)                           
mod.para <- STL.para$para
mod <- Train.Models[[classifier]](X.trn, Y.trn, X.val, X.tst, mod.para, opt.para)
collect.garbage()
model <- mod$main.model
trn.pred <- mod$trn
tst.pred <- mod$tst
val.pred <- mod$val

val.obs <- ifelse(Y.val[, 1] == 1, 1, 0)
tst.obs <-  ifelse(Y.tst[, 1] == 1, 1, 0)

### get optimal threshold using pedicted training probabilities and training performance
val.perf <- Performance(pred=val.pred, obs=val.obs, prevalence = mod.para$prior)
### get optimal thresholds
thresh <- as.numeric(val.perf$threshold)
#### performance on test set using optimal thresholds 
tst.perf <-  Performance(pred=tst.pred, obs=tst.obs, prevalence = mod.para$prior, 
                         threshold=thresh) 
#### performance using percentiles of training set                          
pct = quantile(trn.pred, mod.para$percentile)
tst.perf.pct <- do.call(rbind.data.frame, lapply(pct, function(x) 
Performance(pred=tst.pred, obs=tst.obs,prevalence = val.perf$Obs.Prevalence, 
                         threshold=x)))                    
tst.perf.pct <- cbind.data.frame(percentile = names(pct), tst.perf.pct) 
if(return.model)                        
res <- list(model = model, para = mod$para, pred.prob = tst.pred, true = tst.obs, 
           perf = list(val.perf = val.perf, tst.perf=tst.perf, tst.perf.pct=tst.perf.pct))
else 
res <- list(tst.perf=tst.perf, tst.perf.pct=tst.perf.pct)

return(res)
}
#' @rdname Train 
approxMultiTaskELR.BigV2.tune <- function(form, ...){} 
#' @rdname Train
approxMultiTaskELR.BigV2 <- function(form, ...) {}
#' @rdname Train
Performance.MultiTaskELR <- function(res, ...) {}

























