#' RunModels   
#' 
#' Run, cross-validation or bootstrap in parallel 
#' @name  RunModels 
#' @param classifier character list of classification models. See names(TrainAllModels()). 
#' @param cv,nBoots number of folds, bootsraps 
#' @param XY.dat data matrix  
#' @param resp.vars reponse variables : total expenditure, frequent ER visits, 
#'  frequent hopitalization 
#' @param rhs.vars.list named list of predictor variables for each incremental
#'  model 
#' @param do.STL,do.MTL logicals 
#' @param task.type parameter for MTL
#' @param seed random seed  
#' @param para,STL.para,MTL.para named list of model parameters 
#' @param opt.para,MTL.opt.para logical for parameter tuning. defaul to FALSE
#' @param \dots further arguments passed to or from other methods.
#' @return list of functions for training various algorithms 
#' @author  Che Ngufor <Ngufor.Che@@mayo.edu>
#' @import ELR caret randomGLM doMC 
#' @importFrom foreach foreach
#' @importFrom foreach '%:%'
#' @importFrom foreach '%do%'
#' @importFrom foreach '%dopar%'
NULL 
#' @rdname RunModels   
#' @export
RunHighRiskMedicaid.CV <- function(classifier, cv = 2, XY.dat, resp.vars, rhs.vars.list, 
                           para, opt.para=FALSE, do.STL = TRUE, do.MTL = FALSE, 
                           MTL.opt.para=FALSE, task.type = NULL, seed=12345678){
set.seed(seed) 
MTL.res = STL.res = STL.samp = STL.none = NULL

nme = names(rhs.vars.list)
ix.cv <- Split(Nt = dim(XY.dat)[1], K= cv)
CV.res <- lapply(1:cv, function(kk){ 
cat("Start CV # ", kk, "\n")   
dd.dat <- SplitCrossVal2(XY.dat, kk, ix.cv, val.propo = 0.15)
dat.trn  <- dd.dat$dat.trn 
dat.val <-  dd.dat$dat.val 
dat.tst <- dd.dat$dat.tst 

foreach(rhs.vars = rhs.vars.list) %dopar% { 
#### oversample training data for each model and outcome
#### loop over resp.vars only for STL 
if(do.STL){
STL.res <- lapply(resp.vars, function(yy){
STL.form <- as.formula(paste0(paste0(yy, "~"), paste0(rhs.vars, collapse= "+")))
para$prior <- (table(dat.trn[, lhs.form(STL.form)])/nrow(dat.trn))[2]

para$form <- STL.form 
para$opt.para <- opt.para 
### classification 
Y.trn <- dat.trn[, lhs.form(STL.form), drop = FALSE]
X.trn <-  dat.trn[, rhs.form(STL.form),drop=FALSE]
Y.val <- dat.val[, lhs.form(STL.form), drop = FALSE]
X.val <-  dat.val[, rhs.form(STL.form),drop=FALSE]
Y.tst <- dat.tst[, lhs.form(STL.form), drop = FALSE]
X.tst <-  dat.tst[, rhs.form(STL.form),drop=FALSE]
STL.none <- lapply(classifier, function(x) Train.Validate.Test(classifier=x, X.trn=X.trn, Y.trn=Y.trn,
              X.val=X.val,Y.val=Y.val,X.tst=X.tst,Y.tst=Y.tst,para=para,opt.para=opt.para))      
collect.garbage()               
names(STL.none) <- classifier
#### train oversample models
return(list(STL.none = STL.none))
}
)
names(STL.res)<- resp.vars 
}
if(do.MTL) {
d.dat <- lapply(resp.vars, function(x) 
return(list(dat.trn = dat.trn[, c(x, rhs.vars)], 
           dat.val = dat.val[, c(x, rhs.vars)], 
           dat.tst = dat.tst[, c(x, rhs.vars)])) )
dd.trn <- lapply(d.dat, function(y) y$dat.trn)
dd.val <- lapply(d.dat, function(y) y$dat.val)
dd.tst <- lapply(d.dat, function(y) y$dat.tst)
names(dd.trn) = names(dd.val) = names(dd.tst) = resp.vars
names(new.dd.trn) = resp.vars
para$MTL$prior <- lapply(resp.vars, function(x) (table(dd.trn[[x]][, x])/nrow(dd.trn[[x]]))[2])
names(para$MTL$prior) <- resp.vars
###############################################################################
#################### MTL 
MTL.form <- lapply(resp.vars, function(x) as.formula(paste0(paste0(x, "~"), paste0(rhs.vars, collapse= "+"))) )
names(MTL.form)  <- resp.vars
if(MTL.opt.para){  ## tune for optimal parameters 
dd <- lapply(resp.vars, function(yy) rbind(dd.trn[[yy]], dd.val[[yy]]))
names(dd) <- resp.vars
MTL.res <- approxMultiTaskELR.BigV2.tune(MTL.form, dd, resp.vars, task.type, para, seed)
#MultiTask.res <- MTL.res$MTL.mod
para$MTL$p <- MTL.res$para$MTL$p
para$MTL$gamma <- MTL.res$para$MTL$gamma
para$MTL$mu <- MTL.res$para$MTL$mu
}
MultiTask.res <- approxMultiTaskELR.BigV2(MTL.form, dd.trn, resp.vars, task.type, para, seed)
MultiTask.perf <- Performance.MultiTaskELR(MultiTask.res, dd.trn, dd.val, dd.tst, resp.vars, 
                   task.type,  prevalence = para$MTL$prior)
val.cls <- MultiTask.perf$val$class
tst.cls <- MultiTask.perf$tst$class
MTL.res <- list(val=val.cls, tst=tst.cls)
}     
para$do.STL = do.STL
para$do.MTL = do.MTL 
cat("Done Model : ", nme[sapply(rhs.vars.list, function(x) sum(!(x%in%rhs.vars)) == 0 )], "\n")                    
return(list(STL.res = STL.res, MTL.res = MTL.res, para = para))
}
}
)

for(kk in 1:cv)
  names(CV.res[[kk]]) <- names(rhs.vars.list)
return(CV.res)
}

#' @rdname RunModels   
#' @export
#'
RunHighRiskMedicaid.Boot <- function(classifier,  nBoots = 5, XY.dat, resp.vars, rhs.vars.list, 
                                           MTL.para = NULL, STL.para = NULL,  do.STL = TRUE, do.MTL = FALSE, 
                                           task.type, opt.para=FALSE, MTL.opt.para=FALSE){
                                                                                 
MTL.res = STL.res = NULL 
nme = names(rhs.vars.list)
nobs<-nrow(XY.dat)
rownames(XY.dat) <- NULL 
ix.boot <- 1:nobs
seed <- as.integer(round(2^31 * runif(nBoots, 0, 1)))

Boot.res <- foreach(kk = 1:nBoots) %dopar% {
set.seed(seed[kk])
cat("Start Bootstrap :", kk, "\n")

inbag <- sample(ix.boot, nobs, replace = TRUE)
outbag <- setdiff(ix.boot, inbag)
dat.trn.val <- XY.dat[inbag, ,drop = FALSE]
rownames(dat.trn.val) <- NULL

ix <- sample(nrow(dat.trn.val), floor(0.85*nrow(dat.trn.val)))
dat.trn  <- dat.trn.val[ix, ,drop = FALSE] 
dat.val <-  dat.trn.val[-ix, ,drop = FALSE] 
dat.tst <- XY.dat[outbag, ,drop = FALSE] 

lapply(1:length(rhs.vars.list), function(rhsvars) {
#### loop over resp.vars only for STL 

if(do.STL){
STL.res <- lapply(resp.vars, function(yy){

STL.form <- as.formula(paste0(paste0(yy, "~"), paste0(rhs.vars.list[[nme[rhsvars]]], collapse= "+")))

para$prior <- (table(dat.trn[, yy])/nrow(dat.trn))[2]
para$form <- STL.form 
para$opt.para <- opt.para 

### classification 
Y.trn <- dat.trn[, lhs.form(STL.form), drop = FALSE]
X.trn <-  dat.trn[, rhs.form(STL.form),drop=FALSE]
Y.val <- dat.val[, lhs.form(STL.form), drop = FALSE]
X.val <-  dat.val[, rhs.form(STL.form),drop=FALSE]
Y.tst <- dat.tst[, lhs.form(STL.form), drop = FALSE]
X.tst <-  dat.tst[, rhs.form(STL.form),drop=FALSE]
para <- lapply(STL.para, function(x) {
if(x$Model == nme[rhsvars] & x$Outcome == yy) 
return(x)
}
)
para[sapply(para, is.null)] <- NULL 
cls <- sapply(para, function(x) x$Classifier) 
names(para) <- cls
STL.res <- lapply(classifier, function(x) Train.Validate.Test.Boot(classifier=x, X.trn=X.trn, Y.trn=Y.trn,
              X.val=X.val,Y.val=Y.val,X.tst=X.tst,Y.tst=Y.tst,STL.para= para[[x]],opt.para=opt.para))      
collect.garbage()              
names(STL.res) <- classifier
return(STL.res)
}
)
names(STL.res)<- resp.vars 
}

if(do.MTL) {
d.dat <- lapply(resp.vars, function(x) 
return(list(dat.trn = dat.trn[, c(x, rhs.vars.list[[rhsvars]])], 
           dat.val = dat.val[, c(x, rhs.vars.list[[rhsvars]])], 
           dat.tst = dat.tst[, c(x, rhs.vars.list[[rhsvars]])])) )
dd.trn <- lapply(d.dat, function(y) y$dat.trn)
dd.val <- lapply(d.dat, function(y) y$dat.val)
dd.tst <- lapply(d.dat, function(y) y$dat.tst)
names(dd.trn) = names(dd.val) = names(dd.tst) = resp.vars
#para$MTL$prior <- lapply(resp.vars, function(x) (table(dd.trn[[x]][, x])/nrow(dd.trn[[x]]))[2])
#names(para$MTL$prior) <- resp.vars
###############################################################################
#################### MTL 
MTL.form <- lapply(resp.vars, function(x) as.formula(paste0(paste0(x, "~"), 
                paste0(rhs.vars.list[[nme[rhsvars]]], collapse= "+"))) )

names(MTL.form)  <- resp.vars
if(MTL.opt.para){  ## tune for optimal parameters 
dd <- lapply(resp.vars, function(yy) rbind(dd.trn[[yy]], dd.val[[yy]]))
names(dd) <- resp.vars
MTL.res <- approxMultiTaskELR.BigV2.tune(MTL.form, dd, resp.vars, task.type, MTL.para[[nme[rhsvars]]], seed)
MTL.para[[nme[rhsvars]]]$MTL$p <- MTL.res$para$MTL$p
MTL.para[[nme[rhsvars]]]$MTL$gamma <- MTL.res$para$MTL$gamma
MTL.para[[nme[rhsvars]]]$MTL$mu <- MTL.res$para$MTL$mu
}
MultiTask.res <- approxMultiTaskELR.BigV2(MTL.form, dd.trn, resp.vars, task.type, MTL.para[[nme[rhsvars]]], seed)
MultiTask.perf <- Performance.MultiTaskELR(MultiTask.res, dd.trn, dd.val, dd.tst, resp.vars, 
                   task.type,  prevalence = MTL.para[[nme[rhsvars]]]$MTL$prior)
val.cls <- MultiTask.perf$val$class
tst.cls <- MultiTask.perf$tst$class
MTL.res <- list(val=val.cls, tst=tst.cls)
}   
para$do.MTL = do.MTL                                                          
return(list(STL.res = STL.res, MTL.res = MTL.res, do.STL=do.STL, do.MTL=do.MTL))
}
)
}
for(kk in 1:nBoots)
  names(Boot.res[[kk]]) <- names(rhs.vars.list)
  
return(Boot.res)
}
















