#' getResults   
#' 
#' Extract prediction results  
#' @name  getResults 
#' @param res predictions
#' @param alpha significance level 
#' @return predictions  
#' @author  Che Ngufor <Ngufor.Che@@mayo.edu>
#' @import plyr 
NULL 
#' @rdname getResults    
#' @export
#'
getResults.CV <- function(res){
MTL.Results = STL.Results = NULL 

if(res[[1]][[1]]$para$do.MTL){
tab <- do.call(rbind.data.frame,lapply(res, function(xx) {
       nme = names(xx)      
      do.call(rbind.data.frame, lapply(1:length(xx), function(y) 
      cbind.data.frame(Model = nme[y], xx[[y]]$MTL.res$tst$opt.res
      )) )} )) 
tab$Model <- factor(tab$Model, levels =unique(tab$Model))
tab$Outcome <- factor(tab$Outcome, levels =unique(tab$Outcome))

Mean = ddply(tab, .variables = c("Model", "Outcome"), numcolwise(mean), na.rm = TRUE)
SD = ddply(tab, .variables = c("Model", "Outcome"), numcolwise(sd), na.rm = TRUE)
tab.pct <- do.call(rbind.data.frame,lapply(res, function(xx) {
       nme = names(xx)      
      do.call(rbind.data.frame, lapply(1:length(xx), function(y) 
      cbind.data.frame(Model = nme[y], xx[[y]]$MTL.res$tst$pct.res
      )) )} )) 
tab.pct$AUC = tab.pct$AUC.sd <- NULL            
tab$Model <- factor(tab$Model, levels =unique(tab$Model))
tab$Outcome <- factor(tab$Outcome, levels =unique(tab$Outcome))

tab.pct$Model <- factor(tab.pct$Model, levels =unique(tab.pct$Model))
tab.pct$Outcome <- factor(tab.pct$Outcome, levels =unique(tab.pct$Outcome))
Mean = ddply(tab, .variables = c("Model", "Outcome"), numcolwise(mean), na.rm = TRUE)
SD = ddply(tab, .variables = c("Model", "Outcome"), numcolwise(sd), na.rm = TRUE)

Mean.pct = ddply(tab.pct, .variables = c("Model", "Outcome"), numcolwise(mean), na.rm = TRUE)
SD.pct = ddply(tab.pct, .variables = c("Model", "Outcome"), numcolwise(sd), na.rm = TRUE)
MTL.Results = list(Mean = Mean, SD = SD, Mean.pct = Mean.pct, SD.pct = SD.pct)
}

if(res[[1]][[1]]$para$do.STL){
tab.none <- do.call(rbind.data.frame, lapply(res, function(x){ ## cv 
    nme = names(x)  ## models 
    do.call(rbind.data.frame, lapply(1:length(x), function(y){  ## each model 
    resp = names(x[[y]]$STL.res)   ## outcomes  
    do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res), function(z) {  ## each outcome 
        cls <- names(x[[y]]$STL.res[[z]]$STL.none)  ## no sampling classifiers 
       do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res[[z]]$STL.none), function(zz)
         cbind.data.frame(Model = nme[y],Outcome = resp[z], Classifier =cls[zz], 
                            x[[y]]$STL.res[[z]]$STL.none[[zz]]$perf$tst.perf))) }))}))}))  
                            
                                                                                                                        
tab.none$Model <- factor(tab.none$Model, levels =unique(tab.none$Model))
tab.none$Outcome <- factor(tab.none$Outcome, levels =unique(tab.none$Outcome))
tab.none$Classifier <- factor(tab.none$Classifier, levels =unique(tab.none$Classifier))   
Mean.none = ddply(tab.none, .variables = c("Model", "Outcome", "Classifier"), numcolwise(mean), na.rm = TRUE)
SD.none = ddply(tab.none, .variables = c("Model", "Outcome", "Classifier"), numcolwise(sd), na.rm = TRUE)
    
tab.none.pct <- do.call(rbind.data.frame, lapply(res, function(x){
    nme = names(x)
    do.call(rbind.data.frame, lapply(1:length(x), function(y){
    resp = names(x[[y]]$STL.res)    
    do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res), function(z) {
        cls <- names(x[[y]]$STL.res[[z]]$STL.none)
       do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res[[z]]$STL.none), function(zz)
         cbind.data.frame(Model = nme[y],Outcome = resp[z], Classifier =cls[zz], 
                            x[[y]]$STL.res[[z]]$STL.none[[zz]]$perf$tst.perf.pct))) }))}))}))
                            
                                                                   
Outcome = sapply(1:nrow(tab.none.pct), function(x) paste0(c(tab.none.pct$Outcome[x], ":", tab.none.pct$percentile[x]), collapse=""))
tab.none.pct$Outcome <- Outcome 
tab.none.pct$percentile = tab.none.pct$AUC = tab.none.pct$AUC.sd = NULL 
tab.none.pct$Model <- factor(tab.none.pct$Model, levels =unique(tab.none.pct$Model))
tab.none.pct$Outcome <- factor(tab.none.pct$Outcome, levels =unique(tab.none.pct$Outcome))
tab.none.pct$Classifier <- factor(tab.none.pct$Classifier, levels =unique(tab.none.pct$Classifier)) 
Mean.none.pct = ddply(tab.none.pct, .variables = c("Model", "Outcome", "Classifier"), numcolwise(mean), na.rm = TRUE)
SD.none.pct = ddply(tab.none.pct, .variables = c("Model", "Outcome", "Classifier"), numcolwise(sd), na.rm = TRUE)

STL.Results <- list(Mean.none = Mean.none, SD.none = SD.none, Mean.none.pct = Mean.none.pct, SD.none.pct = SD.none.pct)

}
return(list(STL.Results = STL.Results, MTL.Results = MTL.Results))
}
#' @rdname getResults    
#' @export
#'
getResults.Boot <- function(res, alpha=0.05){
MTL.Results = STL.Results = NULL 
if(res[[1]][[1]]$para$do.MTL){
tab <- do.call(rbind.data.frame,lapply(res, function(xx) {
       nme = names(xx)      
      do.call(rbind.data.frame, lapply(1:length(xx), function(y) 
      cbind.data.frame(Model = nme[y], xx[[y]]$MTL.res$tst$opt.res
      )) )} )) 
tab$Model <- factor(tab$Model, levels =unique(tab$Model))
tab$Outcome <- factor(tab$Outcome, levels =unique(tab$Outcome))

Mean = ddply(tab, .variables = c("Model", "Outcome"), numcolwise(mean), na.rm = TRUE)
SD = ddply(tab, .variables = c("Model", "Outcome"), numcolwise(sd), na.rm = TRUE)
tab.pct <- do.call(rbind.data.frame,lapply(res, function(xx) {
       nme = names(xx)      
      do.call(rbind.data.frame, lapply(1:length(xx), function(y) 
      cbind.data.frame(Model = nme[y], xx[[y]]$MTL.res$tst$pct.res
      )) )} )) 
tab.pct$AUC = tab.pct$AUC.sd <- NULL            
tab$Model <- factor(tab$Model, levels =unique(tab$Model))
tab$Outcome <- factor(tab$Outcome, levels =unique(tab$Outcome))

tab.pct$Model <- factor(tab.pct$Model, levels =unique(tab.pct$Model))
tab.pct$Outcome <- factor(tab.pct$Outcome, levels =unique(tab.pct$Outcome))
Mean = ddply(tab, .variables = c("Model", "Outcome"), numcolwise(mean), na.rm = TRUE)
SD = ddply(tab, .variables = c("Model", "Outcome"), numcolwise(sd), na.rm = TRUE)

Mean.pct = ddply(tab.pct, .variables = c("Model", "Outcome"), numcolwise(mean), na.rm = TRUE)
SD.pct = ddply(tab.pct, .variables = c("Model", "Outcome"), numcolwise(sd), na.rm = TRUE)
MTL.Results = list(Mean = Mean, SD = SD, Mean.pct = Mean.pct, SD.pct = SD.pct)
}

if(res[[1]][[1]]$para$do.STL){
tab.none <- do.call(rbind.data.frame, lapply(res, function(x){ ## cv 
    nme = names(x)  ## models 
    do.call(rbind.data.frame, lapply(1:length(x), function(y){  ## each model 
    resp = names(x[[y]]$STL.res)   ## outcomes  
    do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res), function(z) {  ## each outcome 
        cls <- names(x[[y]]$STL.res[[z]]$STL.none)  ## no sampling classifiers 
       do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res[[z]]$STL.none), function(zz)
         cbind.data.frame(Model = nme[y],Outcome = resp[z], Classifier =cls[zz], 
                            x[[y]]$STL.res[[z]]$STL.none[[zz]]$perf$tst.perf))) }))}))}))  
                            
                                                                                                                        
tab.none$Model <- factor(tab.none$Model, levels =unique(tab.none$Model))
tab.none$Outcome <- factor(tab.none$Outcome, levels =unique(tab.none$Outcome))
tab.none$Classifier <- factor(tab.none$Classifier, levels =unique(tab.none$Classifier))   
Mean.none = ddply(tab.none, .variables = c("Model", "Outcome", "Classifier"), numcolwise(mean), na.rm = TRUE)
SD.none = ddply(tab.none, .variables = c("Model", "Outcome", "Classifier"), numcolwise(sd), na.rm = TRUE)
CI.none <-   ddply(tab.none, .variables = c("Model", "Outcome", "Classifier"), numcolwise(quantile), 
                  probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
    
tab.none.pct <- do.call(rbind.data.frame, lapply(res, function(x){
    nme = names(x)
    do.call(rbind.data.frame, lapply(1:length(x), function(y){
    resp = names(x[[y]]$STL.res)    
    do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res), function(z) {
        cls <- names(x[[y]]$STL.res[[z]]$STL.none)
       do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res[[z]]$STL.none), function(zz)
         cbind.data.frame(Model = nme[y],Outcome = resp[z], Classifier =cls[zz], 
                            x[[y]]$STL.res[[z]]$STL.none[[zz]]$perf$tst.perf.pct))) }))}))}))
                            
                                                                   
Outcome = sapply(1:nrow(tab.none.pct), function(x) paste0(c(tab.none.pct$Outcome[x], ":", tab.none.pct$percentile[x]), collapse=""))
tab.none.pct$Outcome <- Outcome 
tab.none.pct$percentile = tab.none.pct$AUC = tab.none.pct$AUC.sd = NULL 
tab.none.pct$Model <- factor(tab.none.pct$Model, levels =unique(tab.none.pct$Model))
tab.none.pct$Outcome <- factor(tab.none.pct$Outcome, levels =unique(tab.none.pct$Outcome))
tab.none.pct$Classifier <- factor(tab.none.pct$Classifier, levels =unique(tab.none.pct$Classifier)) 
Mean.none.pct = ddply(tab.none.pct, .variables = c("Model", "Outcome", "Classifier"), numcolwise(mean), na.rm = TRUE)
SD.none.pct = ddply(tab.none.pct, .variables = c("Model", "Outcome", "Classifier"), numcolwise(sd), na.rm = TRUE)
CI.none.pct <- ddply(tab.none.pct, .variables = c("Model", "Outcome", "Classifier"), numcolwise(quantile), 
                  probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
STL.Results <- list(Mean.none = Mean.none, SD.none = SD.none, Mean.none.pct = Mean.none.pct, 
                     SD.none.pct = SD.none.pct, CI.none=CI.none,CI.none.pct=CI.none.pct)

}
return(list(STL.Results = STL.Results, MTL.Results = MTL.Results))
}
#' @rdname getResults    
#' @export
#'
getResults.BootV1 <- function(res, alpha=0.05){
MTL.Results = STL.Results = NULL 

if(res[[1]][[1]]$do.MTL){
tab <- do.call(rbind.data.frame,lapply(res, function(xx) {
       nme = names(xx)      
      do.call(rbind.data.frame, lapply(1:length(xx), function(y) 
      cbind.data.frame(Model = nme[y], xx[[y]]$MTL.res$tst$opt.res
      )) )} )) 
tab$Model <- factor(tab$Model, levels =unique(tab$Model))
tab$Outcome <- factor(tab$Outcome, levels =unique(tab$Outcome))

Mean = plyr::ddply(tab, .variables = c("Model", "Outcome"), numcolwise(mean), na.rm = TRUE)
SD = plyr::ddply(tab, .variables = c("Model", "Outcome"), numcolwise(sd), na.rm = TRUE)
CI <- plyr::ddply(tab, .variables = c("Model", "Outcome"), numcolwise(quantile), 
                  probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)

tab.pct <- do.call(rbind.data.frame,lapply(res, function(xx) {
       nme = names(xx)      
      do.call(rbind.data.frame, lapply(1:length(xx), function(y) 
      cbind.data.frame(Model = nme[y], xx[[y]]$MTL.res$tst$pct.res
      )) )} )) 
tab.pct$AUC = tab.pct$AUC.sd <- NULL            
tab.pct$Model <- factor(tab.pct$Model, levels =unique(tab.pct$Model))
tab.pct$Outcome <- factor(tab.pct$Outcome, levels =unique(tab.pct$Outcome))

Mean.pct = plyr::ddply(tab.pct, .variables = c("Model", "Outcome"), numcolwise(mean), na.rm = TRUE)
SD.pct = plyr::ddply(tab.pct, .variables = c("Model", "Outcome"), numcolwise(sd), na.rm = TRUE)
CI.pct <- plyr::ddply(tab.pct, .variables = c("Model", "Outcome"), numcolwise(quantile),  
                   probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
MTL.Results = list(Mean = Mean, SD = SD, CI = CI, Mean.pct = Mean.pct, SD.pct = SD.pct, CI.pct = CI.pct)
}

if(res[[1]][[1]]$do.STL){
tab <- do.call(rbind.data.frame, lapply(res, function(x){
    nme = names(x)
    do.call(rbind.data.frame, lapply(1:length(x), function(y){
    resp = names(x[[y]]$STL.res)    
    do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res), function(z) {
        cls <- names(x[[y]]$STL.res[[z]])
       do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res[[z]]), function(zz)
         cbind.data.frame(Model = nme[y],Outcome = resp[z], Classifier =cls[zz], 
                            x[[y]]$STL.res[[z]][[zz]]$tst.perf))) }))}))}))                                        
                            
tab$Model <- factor(tab$Model, levels =unique(tab$Model))
tab$Outcome <- factor(tab$Outcome, levels =unique(tab$Outcome))
tab$Classifier <- factor(tab$Classifier, levels =unique(tab$Classifier))   
Mean = plyr::ddply(tab, .variables = c("Model", "Outcome", "Classifier"), numcolwise(mean), na.rm = TRUE)
SD = plyr::ddply(tab, .variables = c("Model", "Outcome", "Classifier"), numcolwise(sd), na.rm = TRUE)
CI <- plyr::ddply(tab, .variables = c("Model", "Outcome", "Classifier"), numcolwise(quantile), 
                  probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
  
tab.pct <- do.call(rbind.data.frame, lapply(res, function(x){
    nme = names(x)
    do.call(rbind.data.frame, lapply(1:length(x), function(y){
    resp = names(x[[y]]$STL.res)    
    do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res), function(z) {
        cls <- names(x[[y]]$STL.res[[z]])
       do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res[[z]]), function(zz)
         cbind.data.frame(Model = nme[y],Outcome = resp[z], Classifier =cls[zz], 
                            x[[y]]$STL.res[[z]][[zz]]$tst.perf.pct))) }))}))}))                                       
Outcome = sapply(1:nrow(tab.pct), function(x) paste0(c(tab.pct$Outcome[x], ":", tab.pct$percentile[x]), collapse=""))
tab.pct$Outcome <- Outcome 
tab.pct$percentile = tab.pct$AUC = tab.pct$AUC.sd = NULL 

tab.pct$Model <- factor(tab.pct$Model, levels =unique(tab.pct$Model))
tab.pct$Outcome <- factor(tab.pct$Outcome, levels =unique(tab.pct$Outcome))
tab.pct$Classifier <- factor(tab.pct$Classifier, levels =unique(tab.pct$Classifier)) 
  
Mean.pct = plyr::ddply(tab.pct, .variables = c("Model", "Outcome", "Classifier"), numcolwise(mean), na.rm = TRUE)
SD.pct = plyr::ddply(tab.pct, .variables = c("Model", "Outcome", "Classifier"), numcolwise(sd), na.rm = TRUE)
CI.pct <- plyr::ddply(tab.pct, .variables = c("Model", "Outcome", "Classifier"), numcolwise(quantile), 
                  probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)


STL.Results <- list(Mean = Mean, SD = SD, CI = CI, Mean.pct = Mean.pct, SD.pct = SD.pct, CI.pct = CI.pct)
}
return(list(STL.Results = STL.Results, MTL.Results = MTL.Results))
}
#' @rdname getResults    
#' @export
#'
get.para <- function(res){
MTL.para = STL.para = NULL 
if(res[[1]][[1]]$para$do.MTL){
tab <- lapply(res, function(xx) {
       nme = names(xx)      
      do.call(rbind.data.frame, lapply(1:length(xx), function(y) 
      cbind.data.frame(Model = nme[y], AUC=xx[[y]]$MTL.res$tst$opt.res[, "AUC"]
      )) )} )      
Model <-  unique(tab[[1]]$Model)   
tab2 <- cbind.data.frame(Model = Model, do.call(cbind, lapply(tab, function(x) x$AUC) )) 
tab2$Model <- factor(tab2$Model, levels = unique(tab2$Model)) 
tab3 <- do.call(rbind, lapply(split(tab2, f = tab2$Model), function(x) apply(x[,-1], 2, mean)))       
ix <- apply(tab3, 1, which.max)       
MTL.para <- lapply(seq_along(ix), function(x) res[[ix[x]]][[x]]$para)
names(MTL.para) <- Model       
}

if(res[[1]][[1]]$para$do.STL){
  tab <-lapply(res, function(x){
    nme = names(x)
    do.call(rbind.data.frame, lapply(1:length(x), function(y){
      resp = names(x[[y]]$STL.res)    
      do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res), function(z) {
        cls <- names(x[[y]]$STL.res[[z]]$STL.none)
        do.call(rbind.data.frame, lapply(1:length(x[[y]]$STL.res[[z]]$STL.none), function(zz)
          cbind.data.frame(Model = nme[y],Outcome = resp[z], Classifier =cls[zz], 
                           AUC = x[[y]]$STL.res[[z]]$STL.none[[zz]]$perf$tst.perf[,"AUC"]))) }))}))})
  
  tab2 <- cbind.data.frame(Model = tab[[1]]$Model, Outcome = tab[[1]]$Outcome, Classifier = tab[[1]]$Classifier, 
                           do.call(cbind, lapply(tab, function(x) x$AUC)))                                                      
  ix <- apply(tab2[, -c(1:3)], 1, which.max)   
  Model = tab[[1]]$Model; Outcome = tab[[1]]$Outcome; Classifier = tab[[1]]$Classifier
  STL.para <- lapply(seq_along(ix), function(xx) {
  list(Model = Model[xx], Outcome = Outcome[xx], Classifier = Classifier[xx], 
  para = res[[ix[xx]]][[Model[xx]]]$STL.res[[Outcome[xx]]]$STL.none[[Classifier[xx]]]$para) })  
}
return(list(STL.para = STL.para, MTL.para = MTL.para))
}







