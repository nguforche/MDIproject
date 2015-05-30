#' Utility functions.
#' 
#' Some useful miscellaneous functions 
#'
#' @name Utils
#' @param D.dat A matix/data frame 
#' @param Nt Sample size 
#' @param K,k number of cross-validation and cross-validation number 
#' @param ix.cv cross-validation indices 
#' @param  train.propo,val.propo training set proportion 
#' @param test logical: should test set be created?  
#' @param code  function or expression 
#' @return  training, validation, test data sets or K-fold cross-validation data sets.  
#' @author  Che Ngufor Ngufor.Che@@mayo.edu
NULL 
#' @rdname  Utils
TrainValTestSplit <- function(D.dat,  test=TRUE){
dat.trn = dat.val = dat.tst  = NULL
if(!test){
K = 3
Nt = nrow(D.dat)
size = Nt%/%K
A = runif(Nt)
rk = rank(A)
bk = factor((rk-1)%/%size + 1)
if(Nt%%K > 0) {
levels(bk)[levels(bk) == K+1]  = 1 
}
dat.trn =  D.dat[bk%in%c(1,2), ]
dat.val <- D.dat[bk == 3, ]
} else {
K = 4
Nt = nrow(D.dat)
size = Nt%/%K
A = runif(Nt)
rk = rank(A)
bk = factor((rk-1)%/%size + 1)
if(Nt%%K > 0) {
levels(bk)[levels(bk) == K+1]  = 1 
}
dat.trn =  D.dat[bk%in%c(1,2), ]
dat.val <- D.dat[bk == 3, ]
dat.tst <- D.dat[bk == 4, ]
}
res <- list(dat.trn = dat.trn, dat.val = dat.val, dat.tst = dat.tst)
return(res)
}
#' @rdname  Utils
Split <- function(Nt, K){
size = Nt%/%K
A = runif(Nt)
rk = rank(A)
bk = factor((rk-1)%/%size + 1)
if(Nt%%K > 0) {
levels(bk)[levels(bk) == K+1]  = 1 
}
return(bk)
}
#' @rdname  Utils
SplitCrossVal <- function(D.dat, k, ix.cv, train.propo = 0.85,  test = TRUE){
dat.tst = NULL 
dat.trn <- D.dat[k!= ix.cv, ,drop=FALSE]
dat.tmp <- D.dat[k==ix.cv, ,drop=FALSE]
ix <- sample(nrow(dat.tmp), floor(nrow(dat.tmp)*train.propo))
dat.val <-  dat.tmp[-ix, ,drop=FALSE] 
if(test)
dat.tst  <- dat.tmp[ix, ,drop=FALSE] 
else 
dat.trn <- rbind.data.frame(dat.trn, dat.tmp[ix, ]) 
res <- list(dat.trn = dat.trn, dat.val = dat.val, dat.tst = dat.tst)
return(res)
}
#' @rdname  Utils
SplitCrossVal2 <- function(D.dat, k, ix.cv, val.propo = 0.15){
dat.tmp <- D.dat[k!= ix.cv, ,drop=FALSE]
dat.tst <- D.dat[k==ix.cv, ,drop=FALSE]
ix <- sample(nrow(dat.tmp), floor(nrow(dat.tmp)*val.propo))
dat.val <-  dat.tmp[ix, ,drop=FALSE] 
dat.trn <-  dat.tmp[-ix, ,drop=FALSE] 
res <- list(dat.trn = dat.trn, dat.val = dat.val, dat.tst = dat.tst)
return(res)
}
#' @rdname  Utils
collect.garbage = function(){
#if (exists("collect.garbage") ) rm(collect.garbage)
## The following function collects garbage until the memory is clean.
## Usage: 1. immediately call this function after you call a function or
##        2. rm()
	while (gc()[2,4] != gc()[2,4]){}
}
#' @rdname  Utils
CreateDummy = function(D.dat) {
nme <- colnames(D.dat) 
res <- do.call(cbind.data.frame, lapply(nme, function(x) {
  A <- as.character(D.dat[,x])
  levs <- sort(unique(A))
  Cols <- match(A, levs)
  m <- matrix(0, nrow = length(D.dat[,x]), ncol = length(levs),
              dimnames = list(NULL, paste(x, levs, sep = ".")))
  m[cbind(sequence(length(D.dat[,x])), Cols)] <- 1L
m
}))
return(res)
}
#' @rdname  Utils
my.tryCatch <- function(code) {
      err <- FALSE
    out <- tryCatch(
        {
            # Just to highlight: if you want to use more than one 
            # R expression in the "try" part then you'll have to 
            # use curly brackets.
            # 'tryCatch()' will return the last evaluated expression 
            # in case the "try" part was completed successfully
            code
            # The return value of `readLines()` is the actual value 
            # that will be returned in case there is no condition 
            # (e.g. warning or error). 
            # You don't need to state the return value via `return()` as code 
            # in the "try" part is not wrapped insided a function (unlike that
            # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
            cat("Error in the Expression: ",  paste(cond$call, collapse= ", "), ": original error message = ", cond$message, "\n")
            err <<- TRUE
#             cond <- paste(paste(cond$call)[1], cond$message, sep = " : ")
#            cat(cond, "\n")
            return(NULL)
        },
        warning=function(cond) {
            cat("Warning in the Expression: ",  paste(cond$call)[1], " original warning message = ", conditionMessage(cond), "\n")
            return(NULL)
        },
        finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you 
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>' 
#            message(paste("Processed : ", code))
        }
    )    
    if(err) stop("stopping")
    
    return(out)
}













