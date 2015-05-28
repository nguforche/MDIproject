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













