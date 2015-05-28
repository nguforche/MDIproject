#' RUSBoostModels   
#' 
#' Randon undersampling with boosting for various learners. 
#' Learner does not have to be a weak learner as in standard boosting 
#' algorithm  
#' @name RUSBoostModels
#' @param data,x data matrix    
#' @param form,formula A formula
#' @param boot logical 
#' @param iters number of bootstrap replicates 
#' @param idx indices of majority class 
#' @param coeflearn from \code{adabag}
#' @param sampleFraction undersampling ratio
#' @param object  trained model of appropriate class 
#' @param newdata,newmfinal newdata 
#' @param para A named parameter list of model parameters 
#' @param \dots Further arguments passed to or from other methods.
#' @return Trained undersample bootstrap models 
#' @author  Che Ngufor Ngufor.Che@@mayo.edu
#' @import ELR caret 
#'
NULL 
#' @rdname RUSBoostModels
#' @export
RUSBoostELR <- function(x, ...) UseMethod("RUSBoostELR")
#' @rdname RUSBoostModels
#' @export
RUSBoostELR.default <- function(x, ...) {}
#' @rdname RUSBoostModels
#' @export
RUSBoostELR.formula <- function (form, data, boot = FALSE, iters = 100, 
              coeflearn = "Breiman", sampleFraction, para, idx, ...)
{     
    if (!(as.character(coeflearn) %in% c("Freund", "Breiman",
        "Zhu"))) {
        stop("coeflearn must be 'Freund', 'Breiman' or 'Zhu' ")
    }
    form <- as.formula(form)
    resp <- as.character(form[[2]])
    data[, resp] <- factor(data[, resp])
    vardep <- data[, resp]
    vardep <- vardep[[1]]            
    n <- nrow(data)
    indices <- 1:n
    n.negative <- sum(idx)
    negatives <- data[idx, ]
    nclasses <- nlevels(vardep)
    class.levels <- levels(vardep)    
    trees <- list()
    mweights <- rep(0, iters)
    w <- rep(1/n, n)
    matrix.weights <- array(0, c(n, iters))
        
    for (m in 1:iters) {    
        subset.index <- c(sample(indices[idx], n.negative * sampleFraction,
            replace = FALSE), indices[!idx])
        tmp.sample <- data[subset.index, ]
        tmp.weights <- abs(w[subset.index])
        t.s.l <- length(tmp.sample[, 1])
        
        if (boot == TRUE) {
           inner.tmp.weights <<- tmp.weights
           bootstrap <- sample(1:t.s.l, replace = TRUE, prob = tmp.weights)            
           fit <- ELR(form, tmp.sample[bootstrap, ],  para=para)                 
           prob <- predict(fit, data)$prob[, 2]                      
           thresh <- opt.thresh(prob, data[, resp])
           flearn <- factor(ifelse(prob >= thresh, 1, 0))
           ind <- as.numeric(vardep != flearn)
           err <- as.numeric(w %*% ind)        
        }
        if (boot == FALSE) {
            inner.tmp.weights <<- tmp.weights
           fit <- ELR(form, tmp.sample,  para=para)                 
           prob <- predict(fit, data)$prob[, 2]                      
           thresh <- opt.thresh(prob, data[, resp])
           flearn <-factor(ifelse(prob >= thresh, 1, 0))
           ind <- as.numeric(vardep != flearn)
           err <- as.numeric(w %*% ind)                   
           }
          
        c <- log((1 - err)/err)
        if (coeflearn == "Breiman") {
            c <- (1/2) * c
        }
        if (coeflearn == "Zhu") {
            c <- c + log(nclasses - 1)
        }
        matrix.weights[, m] <- w
        update.vector <- w * exp(c * ind)
        w[ind == 1] <- update.vector[ind == 1]
        w <- w/sum(w)
        maxerror <- 0.5
        eac <- 0.001
        if (coeflearn == "Zhu") {
            maxerror <- 1 - 1/nclasses
        }
        if (err >= maxerror) {
            weights <- rep(1/n, n)
            maxerror <- maxerror - eac
            c <- log((1 - maxerror)/maxerror)
            if (coeflearn == "Breiman") {
                c <- (1/2) * c
            }
            if (coeflearn == "Zhu") {
                c <- c + log(nclasses - 1)
            }
        }
        if (err == 0) {
            c <- log((1 - eac)/eac)
            if (coeflearn == "Breiman") {
                c <- (1/2) * c
            }
            if (coeflearn == "Zhu") {
                c <- c + log(nclasses - 1)
            }
        }
        trees[[m]] <- fit
        mweights[m] <- c
    }
    mweights <- mweights/sum(mweights)
    pred <- data.frame(rep(0, n))
    prob <- c() 
    nvar <- length(data[1, ]) - 1
    imp <- array(0, c(iters, nvar))
    for (m in 1:iters) {
        if (m == 1) {
        
            pb <-  predict(trees[[m]], data)$prob[, 2] 
            thresh <- opt.thresh(pb, data[, resp])
            pred <- factor(ifelse(pb >= thresh, 1, 0))
            prob = pb 
            
        }
        else {        
             pb <-  predict(trees[[m]], data)$prob[, 2] 
             thresh <- opt.thresh(pb, data[, resp])             
             pred <- data.frame(pred, factor(ifelse(pb >= thresh, 1, 0)))
             prob <- cbind(prob, pb)
        }
    }
    classfinal <- array(0, c(n, nlevels(vardep)))
    
    for (i in 1:nlevels(vardep)) {
        classfinal[, i] <- matrix(as.numeric(pred == levels(vardep)[i]), 
        nrow = n) %*% as.vector(mweights)
    }
    
    w.prob <- prob%*%as.vector(mweights)
    w.prob <- cbind(1-w.prob, w.prob)
    votes <- classfinal/apply(classfinal, 1, sum)
    predclass <- rep("O", n)
    for (i in 1:n) {
        predclass[i] <- as.character(levels(vardep)[(order(classfinal[i,], 
        decreasing = TRUE)[1])])
    }
### build model on probabilities
	rhs <- paste0("V", 1:ncol(prob)) 
	colnames(prob) <- rhs     
	dt <- cbind.data.frame(resp = data[, resp], prob) 
	colnames(dt) <- c(resp, rhs) 
	stack.form <- as.formula(paste0(paste0(resp, " ~"), 
	paste0(rhs, collapse= "+"))) 
	stack.fit <- ELR(stack.form, dt,  para=para)                 
	stack.prob <-  predict(stack.fit, dt)$prob                      
	thresh <- opt.thresh(stack.prob[,2], data[, resp])
	stack.class <-ifelse(stack.prob[,2] >= thresh, 1, 0)

        para$stack.form <- stack.form
        para$stack.thresh <- thresh 
    
        ans <- list(form = form, trees = trees, weights = mweights,
        votes = classfinal, prob = votes, class = predclass, 
        class.levels = class.levels, 
        w.prob=w.prob, stack.fit = stack.fit, stack.class = stack.class,
         stack.prob = stack.prob, 
        para=para)
    class(ans) <- "RUSBoostELR"
    ans
}
#' @rdname RUSBoostModels
#' @export
predict.RUSBoostELR <- function (object, newdata, newmfinal = length(object$trees), 
    ...) 
{  
error <- NULL 
stack.error <- NULL
tab <- NULL 
accuracy<- NULL

    if (newmfinal > length(object$trees) | newmfinal < 1) 
        stop("newmfinal must be 1<newmfinal<mfinal")
    form <- object$form
    
    n <- nrow(newdata)  
    class.levels <- object$class.levels 
    nclases <- length(class.levels)
    pesos <- rep(1/n, n)
    newdata <- data.frame(newdata, pesos)
    pond <- object$weights[1:newmfinal]
    pred <- data.frame(rep(0, n))
    prob <- c()  
    for (m in 1:newmfinal) {
        if (m == 1) {        
            pb <-  predict(object$trees[[m]], newdata)$prob[, 2] 
            pred <- factor(ifelse(pb >= object$para$stack.thresh, 1, 0))
            prob = pb         
        }
        else {        
             pb <-  predict(object$trees[[m]], newdata)$prob[, 2]            
             pred <- data.frame(pred, 
             factor(ifelse(pb >= object$para$stack.thresh, 1, 0)))
             prob <- cbind(prob, pb)
        }
    }               
    classfinal <- array(0, c(n, nclases))
    for (i in 1:nclases) {
        classfinal[, i] <- matrix(as.numeric(pred == class.levels[i]), 
        nrow = n) %*% pond
    }       
    w.prob <-   prob%*%pond   
    w.prob <- cbind(1-w.prob, w.prob)      
    predclass <- rep("O", n)
    for (i in 1:n) {
        predclass[i] <- as.character(class.levels[(order(classfinal[i, ], 
        decreasing = TRUE)[1])])
    }
### predict stack model 
	rhs <- paste0("V", 1:ncol(prob)) 
	colnames(prob) <- rhs  	
	stack.prob <- predict(object$stack.fit, prob)$prob                      
	stack.class <-ifelse(stack.prob[,2] >= object$para$stack.thresh, 1, 0)

   resp <- as.character(object$form[[2]])
   if(resp%in%colnames(newdata)) {
    vardep <- newdata[, resp]        
    tab <- table(predclass, vardep, dnn = c("Predicted Class",  "Observed Class"))
    error <- 1 - sum(predclass == vardep)/n
    stack.error <- 1 - sum(stack.class == vardep)/n
    accuracy <- Performance(pred=stack.prob[, 2], obs=vardep, 
    theshold=object$para$stack.thresh)
    }        
    votosporc <- classfinal/apply(classfinal, 1, sum)       
      output <- list(form = form, votes = classfinal, votes.prob = votosporc, 
        class = predclass, confusion = tab, error = error, w.prob=w.prob, 
        stack.prob = stack.prob, stack.class = stack.class, 
        stack.error = stack.error, 
        accuracy=accuracy)
    return(output)
}

#' @rdname RUSBoostModels
#' @export
RUSBoostGLM <- function(x, ...) UseMethod("RUSBoostGLM")
#' @rdname RUSBoostModels
#' @export
RUSBoostGLM.default <- function(x,...) {}
#' @rdname RUSBoostModels
#' @export
RUSBoostGLM.formula <- function (formula, data, boot = FALSE, iters = 100, 
             coeflearn = "Breiman", sampleFraction, idx, ...)
{   
stack.prob = stack.class=stack.form= stack.thresh=NULL
stack.fit=NULL
    if (!(as.character(coeflearn) %in% c("Freund", "Breiman",
        "Zhu"))) {
        stop("coeflearn must be 'Freund', 'Breiman' or 'Zhu' ")
    }
    formula <- as.formula(formula)
    resp <- as.character(formula[[2]])
    data[, resp] <- factor(ifelse(data[, resp] == 1, "Yes", "No"))        
    vardep <- data[, resp]
    vardep <- vardep[[1]]
    n <- nrow(data)
    indices <- 1:n
    n.negative <- sum(idx)
    negatives <- data[idx, ]
    nclasses <- nlevels(vardep)
    class.levels <- levels(vardep)    
    trees <- list()
    mweights <- rep(0, iters)
    w <- rep(1/n, n)
    matrix.weights <- array(0, c(n, iters))
    fitControl <- trainControl(method = "none", classProbs = TRUE)    
    for (m in 1:iters) {    
        subset.index <- c(sample(indices[idx], n.negative * sampleFraction,
            replace = FALSE), indices[!idx])
        tmp.sample <- data[subset.index, ]
        tmp.weights <- abs(w[subset.index])
        t.s.l <- length(tmp.sample[, 1])        
        if (boot == TRUE) {
            inner.tmp.weights <<- tmp.weights
            bootstrap <- sample(1:t.s.l, replace = TRUE, prob = tmp.weights)        
            fit <-  train(formula, data = tmp.sample[bootstrap, ], 
                           method = "glm", 
                          family = "binomial", trControl = fitControl, 
                          metric = "ROC", weights = inner.tmp.weights)     
           flearn <- predict(fit, newdata = data, type = "raw")
           ind <- as.numeric(vardep != flearn)
           err <- as.numeric(w %*% ind)        
        }
        if (boot == FALSE) {
            inner.tmp.weights <<- tmp.weights
            fit <-  train(formula, data = tmp.sample, method = "glm", 
                          family = "binomial", trControl = fitControl, 
                          metric = "ROC", weights = inner.tmp.weights)     
           flearn <- predict(fit, newdata = data, type = "raw")
           ind <- as.numeric(vardep != flearn)
           err <- as.numeric(w %*% ind)        
          }          
        c <- log((1 - err)/err)
        if (coeflearn == "Breiman") {
            c <- (1/2) * c
        }
        if (coeflearn == "Zhu") {
            c <- c + log(nclasses - 1)
        }
        matrix.weights[, m] <- w
        update.vector <- w * exp(c * ind)
        w[ind == 1] <- update.vector[ind == 1]
        w <- w/sum(w)
        maxerror <- 0.5
        eac <- 0.001
        if (coeflearn == "Zhu") {
            maxerror <- 1 - 1/nclasses
        }
        if (err >= maxerror) {
            weights <- rep(1/n, n)
            maxerror <- maxerror - eac
            c <- log((1 - maxerror)/maxerror)
            if (coeflearn == "Breiman") {
                c <- (1/2) * c
            }
            if (coeflearn == "Zhu") {
                c <- c + log(nclasses - 1)
            }
        }
        if (err == 0) {
            c <- log((1 - eac)/eac)
            if (coeflearn == "Breiman") {
                c <- (1/2) * c
            }
            if (coeflearn == "Zhu") {
                c <- c + log(nclasses - 1)
            }
        }
        trees[[m]] <- fit
        mweights[m] <- c
    }
    mweights <- mweights/sum(mweights)
    pred <- data.frame(rep(0, n))
    prob <- c() 
    nvar <- length(data[1, ]) - 1
    imp <- array(0, c(iters, nvar))
    for (m in 1:iters) {
        if (m == 1) {
            pred <- predict(trees[[m]], data, type = "raw") 
            prob <- predict(trees[[m]], data, type = "prob")[,2]
        }
        else {
            pred <- data.frame(pred, predict(trees[[m]], data, type = "raw"))
            prob <- cbind(prob, predict(trees[[m]], data, type = "prob")[,2])
        }
    }
    classfinal <- array(0, c(n, nlevels(vardep)))
    for (i in 1:nlevels(vardep)) {
    classfinal[, i] <- matrix(as.numeric(pred == levels(vardep)[i]), 
    nrow = n) %*% as.vector(mweights)
    }    
    w.prob <- prob%*%as.vector(mweights)
    w.prob <- cbind(1-w.prob, w.prob)
    votes <- classfinal/apply(classfinal, 1, sum)
    predclass <- rep("O", n)
    for (i in 1:n) {
        predclass[i] <- as.character(levels(vardep)[(order(classfinal[i,
            ], decreasing = TRUE)[1])])
    }
### build model on probabilities
#	rhs <- paste0("V", 1:ncol(prob)) 
#	colnames(prob) <- rhs     
#	dt <- cbind.data.frame(resp = data[, resp], prob) 
#	colnames(dt) <- c(resp, rhs)
#	 
#	stack.form <- as.formula(paste0(paste0(resp, " ~"), 
#            paste0(rhs, collapse= "+"))) 	
#        stack.fit <-  train(stack.form, data = dt, method = "glm", 
#                          family = "binomial", trControl = fitControl, 
#          metric = "ROC")                               
#	stack.prob <- predict(stack.fit, newdata = dt, type = "prob")	
        stack.thresh <- opt.thresh(w.prob[,2], as.numeric(data[, resp])-1)       
#        stack.class <-ifelse(stack.prob[,2] >= stack.thresh, 1, 0)        	        
        ans <- list(formula = formula, trees = trees, stack.fit=stack.fit,
                      weights = mweights,votes = classfinal, prob = votes,
                       class = predclass, class.levels = class.levels, 
                       w.prob=w.prob, stack.prob = stack.prob, 
                     stack.thresh=stack.thresh, stack.class=stack.class, 
                     stack.form = stack.form)
    class(ans) <- "RUSBoostGLM"
return(ans)
}
#' @rdname RUSBoostModels
#' @export
predict.RUSBoostGLM <- function (object, newdata, newmfinal = length(object$trees), 
          ...) 
{  
error <- NULL 
stack.error <- NULL
tab = NULL
stack.prob = stack.class= NULL
accuracy.votes = accuracy.stack = accuracy.weighted.probs = NULL 

    if (newmfinal > length(object$trees) | newmfinal < 1) 
        stop("newmfinal must be 1<newmfinal<mfinal")
    formula <- object$formula    
    n <- nrow(newdata)  
    class.levels <- object$class.levels 
    nclases <- length(class.levels)
    pesos <- rep(1/n, n)
    newdata <- data.frame(newdata, pesos)
    pond <- object$weights[1:newmfinal]
    pred <- data.frame(rep(0, n))
    prob <- c()  
    for (m in 1:newmfinal) {
        if (m == 1) {
            pred <- predict(object$trees[[m]], newdata, type = "raw")
            prob <- predict(object$trees[[m]], newdata, type = "prob")[,2]
        }
        else {
            pred <- data.frame(pred, predict(object$trees[[m]],
             newdata, type = "raw"))
            prob <- cbind(prob, predict(object$trees[[m]], 
            newdata, type = "prob")[,2])
        }
    }               
    classfinal <- array(0, c(n, nclases))
    for (i in 1:nclases) {
        classfinal[, i] <- matrix(as.numeric(pred == class.levels[i]), 
        nrow = n) %*% pond
    }       
    w.prob <-   prob%*%pond   
    w.prob <- cbind(1-w.prob, w.prob)      
    predclass <- rep("O", n)
    for (i in 1:n) {
        predclass[i] <- as.character(class.levels[(order(classfinal[i, ], 
        decreasing = TRUE)[1])])
    }    
### predict stack model 
if(class(object) != "RUSBoostGLM"){
	rhs <- paste0("V", 1:ncol(prob)) 
	colnames(prob) <- rhs  	
	stack.prob <- predict(object$stack.fit, prob, type = "prob")                      
	stack.class <- ifelse(stack.prob[,2] >= object$stack.thresh, 1, 0)  
}	     
   votosporc <- classfinal/apply(classfinal, 1, sum) 
        
   resp <- as.character(object$formula[[2]])
   if(resp%in%colnames(newdata)) {
    vardep <- newdata[, resp]        
    tab <- table(predclass, vardep, dnn = c("Predicted Class", 
    "Observed Class"))
    error <- 1 - sum(predclass == vardep)/n
    accuracy.weighted.probs <- Performance(w.prob[, 2], 
    vardep, threshold=object$stack.thresh)
    
    if(class(object) != "RUSBoostGLM"){
    stack.error <- 1 - sum(stack.class == vardep)/n
    accuracy.votes <- Performance(votosporc[, 2], vardep)
    accuracy.stack <- Performance(stack.prob[, 2], 
    vardep, threshold=object$stack.thresh)
    }
    }           
    output <- list(formula = formula, votes = classfinal, 
    votes.prob = votosporc, class = predclass, confusion = tab, error = error, 
    w.prob=w.prob, stack.prob=stack.prob,stack.class=stack.class, 
    accuracy.votes=accuracy.votes, accuracy.stack = accuracy.stack, 
    accuracy.weighted.probs=accuracy.weighted.probs,
        stack.error = stack.error)
  return(output)
}
#' @rdname RUSBoostModels
#' @export
RUSBoostGBM <- function(formula, ...) UseMethod("RUSBoostGBM")
#' @rdname RUSBoostModels
#' @export
RUSBoostGBM <- function (formula, data, boot = FALSE, iters = 100, 
     coeflearn = "Breiman", sampleFraction, para, idx)
{    
    if (!(as.character(coeflearn) %in% c("Freund", "Breiman",
        "Zhu"))) {
        stop("coeflearn must be 'Freund', 'Breiman' or 'Zhu' ")
    }
    formula <- as.formula(formula)
    resp <- as.character(formula[[2]])
    data[, resp] <- factor(ifelse(data[, resp] == 1, "Yes", "No"))        
    vardep <- data[, resp]
    vardep <- vardep[[1]]
    n <- nrow(data)
    indices <- 1:n
    n.negative <- sum(idx)
    negatives <- data[idx, ]
    nclasses <- nlevels(vardep)
    class.levels <- levels(vardep)
    
    trees <- list()
    mweights <- rep(0, iters)
    w <- rep(1/n, n)
    matrix.weights <- array(0, c(n, iters))
    fitControl <- trainControl(method = "none")    
    for (m in 1:iters) {    
        subset.index <- c(sample(indices[idx], n.negative * sampleFraction,
            replace = FALSE), indices[!idx])
        tmp.sample <- data[subset.index, ]
        tmp.weights <- abs(w[subset.index])
        t.s.l <- length(tmp.sample[, 1])        
        if (boot == TRUE) {
            inner.tmp.weights <<- tmp.weights
            bootstrap <- sample(1:t.s.l, replace = TRUE, prob = tmp.weights)        
            fit <-  train(formula, data = tmp.sample[bootstrap, ], method = "gbm", 
                          trControl = fitControl, verbose = FALSE,
                          weights = inner.tmp.weights, 
                          tuneGrid = data.frame(n.trees = para$n.trees,
                          interaction.depth= para$interaction.depth, 
                          shrinkage= para$shrinkage, n.minobsinnode = 
                          para$n.minobsinnode))                                
           flearn <- predict(fit, newdata = data, type = "raw")
           ind <- as.numeric(vardep != flearn)
           err <- as.numeric(w %*% ind)        
        }
        if (boot == FALSE) {
            inner.tmp.weights <<- tmp.weights
            fit <-  train(formula, data = tmp.sample, method = "gbm", 
                          trControl = fitControl, verbose = FALSE,
                          weights = inner.tmp.weights, 
                          tuneGrid = data.frame(n.trees = para$n.trees,
                          interaction.depth= para$interaction.depth, 
                          shrinkage= para$shrinkage, n.minobsinnode = 
                          para$n.minobsinnode))                                                         
           flearn <- predict(fit, newdata = data, type = "raw")
           ind <- as.numeric(vardep != flearn)
           err <- as.numeric(w %*% ind)        
          }          
        c <- log((1 - err)/err)
        if (coeflearn == "Breiman") {
            c <- (1/2) * c
        }
        if (coeflearn == "Zhu") {
            c <- c + log(nclasses - 1)
        }
        matrix.weights[, m] <- w
        update.vector <- w * exp(c * ind)
        w[ind == 1] <- update.vector[ind == 1]
        w <- w/sum(w)
        maxerror <- 0.5
        eac <- 0.001
        if (coeflearn == "Zhu") {
            maxerror <- 1 - 1/nclasses
        }
        if (err >= maxerror) {
            weights <- rep(1/n, n)
            maxerror <- maxerror - eac
            c <- log((1 - maxerror)/maxerror)
            if (coeflearn == "Breiman") {
                c <- (1/2) * c
            }
            if (coeflearn == "Zhu") {
                c <- c + log(nclasses - 1)
            }
        }
        if (err == 0) {
            c <- log((1 - eac)/eac)
            if (coeflearn == "Breiman") {
                c <- (1/2) * c
            }
            if (coeflearn == "Zhu") {
                c <- c + log(nclasses - 1)
            }
        }
        trees[[m]] <- fit
        mweights[m] <- c
    }
    mweights <- mweights/sum(mweights)
    pred <- data.frame(rep(0, n))
    prob <- c() 
    nvar <- length(data[1, ]) - 1
    imp <- array(0, c(iters, nvar))
    for (m in 1:iters) {
        if (m == 1) {
            pred <- predict(trees[[m]], data, type = "raw") 
            prob <- predict(trees[[m]], data, type = "prob")[,2]
        }
        else {
            pred <- data.frame(pred, predict(trees[[m]], data, type = "raw"))
            prob <- cbind(prob, predict(trees[[m]], data, type = "prob")[,2])
        }
    }
    classfinal <- array(0, c(n, nlevels(vardep)))
    for (i in 1:nlevels(vardep)) {
        classfinal[, i] <- matrix(as.numeric(pred == levels(vardep)[i]), 
        nrow = n) %*% as.vector(mweights)
    }    
    w.prob <- prob%*%as.vector(mweights)
    w.prob <- cbind(1-w.prob, w.prob)
    votes <- classfinal/apply(classfinal, 1, sum)
    predclass <- rep("O", n)
    for (i in 1:n) {
        predclass[i] <- as.character(levels(vardep)[(order(classfinal[i,
            ], decreasing = TRUE)[1])])
    }
### build model on probabilities
	rhs <- paste0("V", 1:ncol(prob)) 
	colnames(prob) <- rhs     
	dt <- cbind.data.frame(resp = data[, resp], prob) 
	colnames(dt) <- c(resp, rhs)	 
	stack.form <- as.formula(paste0(paste0(resp, " ~"), 
	paste0(rhs, collapse= "+"))) 	
        stack.fit <-  train(stack.form, data = dt, method = "gbm", 
                          trControl = fitControl, verbose = FALSE, 
                          tuneGrid = data.frame(n.trees = para$n.trees,
                          interaction.depth= para$interaction.depth, 
                          shrinkage= para$shrinkage, n.minobsinnode = 
                          para$n.minobsinnode))                                                         
	stack.prob <- predict(stack.fit, newdata = dt, type = "prob")	
        stack.thresh <- opt.thresh(stack.prob[,2], as.numeric(data[, resp])-1)       
        stack.class <-ifelse(stack.prob[,2] >= stack.thresh, 1, 0)	        
ans <- list(formula = formula, trees = trees, stack.fit=stack.fit,
              weights = mweights,votes = classfinal, prob = votes, 
              class = predclass, class.levels = class.levels, 
              w.prob=w.prob, stack.prob = stack.prob, stack.thresh=stack.thresh, 
              stack.class=stack.class, stack.form = stack.form)
    class(ans) <- "RUSBoostGBM"
    ans
}
#' @rdname RUSBoostModels
#' @export
predict.RUSBoostGBM <- function (object, newdata, 
newmfinal = length(object$trees), ...) 
{  
error <- NULL 
stack.error <- NULL
tab = NULL
stack.prob = stack.class= NULL
accuracy.votes = accuracy.stack = accuracy.weighted.probs = NULL 

    if (newmfinal > length(object$trees) | newmfinal < 1) 
        stop("newmfinal must be 1<newmfinal<mfinal")
    formula <- object$formula    
    n <- nrow(newdata)  
    class.levels <- object$class.levels 
    nclases <- length(class.levels)
    pesos <- rep(1/n, n)
    newdata <- data.frame(newdata, pesos)
    pond <- object$weights[1:newmfinal]
    pred <- data.frame(rep(0, n))
    prob <- c()  
    for (m in 1:newmfinal) {
        if (m == 1) {
            pred <- predict(object$trees[[m]], newdata, type = "raw")
            prob <- predict(object$trees[[m]], newdata, type = "prob")[,2]
        }
        else {
            pred <- data.frame(pred, predict(object$trees[[m]],
             newdata, type = "raw"))
            prob <- cbind(prob, predict(object$trees[[m]], 
            newdata, type = "prob")[,2])
        }
    }               
    classfinal <- array(0, c(n, nclases))
    for (i in 1:nclases) {
        classfinal[, i] <- matrix(as.numeric(pred == class.levels[i]), 
        nrow = n) %*% pond
    }       
    w.prob <-   prob%*%pond   
    w.prob <- cbind(1-w.prob, w.prob)      
    predclass <- rep("O", n)
    for (i in 1:n) {
        predclass[i] <- as.character(class.levels[(order(classfinal[i, ], 
        decreasing = TRUE)[1])])
    }    
### predict stack model 
	rhs <- paste0("V", 1:ncol(prob)) 
	colnames(prob) <- rhs  	
	stack.prob <- predict(object$stack.fit, prob, type = "prob")                      
	stack.class <- ifelse(stack.prob[,2] >= object$stack.thresh, 1, 0)  
		     
   votosporc <- classfinal/apply(classfinal, 1, sum)         
   resp <- as.character(object$formula[[2]])
   
   if(resp%in%colnames(newdata)) {
    vardep <- newdata[, resp]        
    tab <- table(predclass, vardep, dnn = c("Predicted Class", 
    "Observed Class"))
    error <- 1 - sum(predclass == vardep)/n
    accuracy.weighted.probs <- Performance(w.prob[, 2], 
    vardep, threshold=object$stack.thresh)
    
    stack.error <- 1 - sum(stack.class == vardep)/n
    accuracy.votes <- Performance(votosporc[, 2], vardep)
    accuracy.stack <- Performance(stack.prob[, 2], 
    vardep, threshold=object$stack.thresh)
    
    }           
    output <- list(formula = formula, votes = classfinal, 
    votes.prob = votosporc, class = predclass, confusion = tab, error = error, 
    w.prob=w.prob, stack.prob=stack.prob,stack.class=stack.class, 
    accuracy.votes=accuracy.votes, accuracy.stack = accuracy.stack, 
    accuracy.weighted.probs=accuracy.weighted.probs,
        stack.error = stack.error)
  return(output)
}





























