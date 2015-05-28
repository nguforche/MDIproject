#' preProcessData
#' 
#' Set up data  
#'
#' @param HighRiskMedicaid.data.file.name  data file name  
#' @return  training data, lsit of model domains, respons variables and list of fomulas.  
#' @author  Che Ngufor Ngufor.Che@@mayo.edu
#' @importFrom foreign  read.dta  
#' @export 
preProcessData <- function(HighRiskMedicaid.data.file.name=
"AnalyticFileForNilay_090814.dta"){
data <- read.dta( HighRiskMedicaid.data.file.name, convert.dates = TRUE, 
convert.factors = TRUE,   missing.type = FALSE,
         convert.underscore = FALSE, warn.missing.labels = TRUE)
dat <- subset(data, subset = sample == 1)        
dat$halex <- cut(dat$halex, breaks = c(0.0, 0.15, 0.25, 0.5, 0.75, 0.9, 1.0), 
labels = c("worst","v.poor","poor", "good", "v.good", "best"), 
          include.lowest = FALSE,  right = TRUE, dig.lab = 3,  ordered_result = FALSE)
dat$age <- cut(dat$age, breaks = c(19, 25, 35, 45, 55, 65), 
labels = c("19.to.24","25.to.34","35.to.44", "45.to.54", "55.to.65"), 
          include.lowest = FALSE,  right = FALSE,  ordered_result = FALSE)
vars <- c("male", "midwest", "south", "west", "married", "numkids", "numadults",   
              "pov100", "pov138", "anyheart",  "stroke",  "ulcer", "cancer", "diabetes",
              "kidney", "liver",  "asthmaetal", "mepspub", "mepsuns", "age", "halex", 
              "hospnt","ervisit","care10")                                             
dat[, vars] <-  do.call(cbind.data.frame, lapply(dat[, vars], function(x){
res <- as.factor(x)
if(nlevels(res) == 2) levels(res) <- c("No", "Yes") 
res 
}
))
levels(dat$numadults)[as.numeric(levels(dat$numadults)) > 3] <- "4plus"
levels(dat$numadults) <- c("One", "Two", "Three", "Four.or.more") 
levels(dat$numkids)[levels( dat$numkids) > 5] <- "6plus"
levels(dat$numkids) <- c("None", "One", "Two", "Three", "Four", "Five", "Six.or.more")
lhs.vars <-  c("totexpy1_dec", "twoer", "inpat")    
### data for variable importance      
Vimp.dat <- dat
### 
dat[, vars] <-  do.call(cbind.data.frame, lapply(dat[, vars], function(x){
if(nlevels(x) == 2) 
as.numeric(x)-1 
else x  
}
))
fac <- sapply(dat, function(x) class(x) == "factor")
fac.vars <- names(dat)[fac]
XY.fac <- CreateDummy(dat[, fac.vars, drop=FALSE]) 
XY.dat <- cbind.data.frame(dat[, names(dat)[!fac]], XY.fac)
rhs.vars <- c(vars[!vars%in%fac.vars], colnames(XY.fac))
lhs.vars <-  c("totexpy1_dec", "twoer", "inpat")  

XY.dat$age_2 <- NULL 
#### incremental predictive capabilities of variables          
baseline <- c(names(XY.dat)[grep("age.", names(XY.dat))],
              "male","midwest","south","west","married", 
              names(XY.dat)[grep("numkids",names(XY.dat))], 
              names(XY.dat)[grep("numadults", names(XY.dat))],
              "pov100","pov138")
conditions <- c("anyheart","stroke","ulcer","cancer","diabetes","kidney","liver",
                "asthmaetal")
access <- c("mepspub","mepsuns")
HSRQOL <- names(XY.dat)[grep("halex", names(XY.dat))]
utilization <- c("hospnt","ervisit","care10")
           
rhs.vars.list <- list(
              baseline = baseline,
              baseline.conditions = c(baseline,conditions),
              baseline.access = c(baseline,access),
              baseline.HSRQOL = c(baseline,HSRQOL),
              baseline.utilization = c(baseline,utilization),
              baseline.conditions.HSRQOL = c(baseline,conditions,HSRQOL),
              baseline.conditions.utilization = c(baseline,conditions,utilization),
              baseline.conditions.access = c(baseline,conditions,access),
              baseline.HSRQOL.utilization = c(baseline,HSRQOL,utilization),
              baseline.conditions.HSRQOL.utilization = 
              c(baseline,conditions,HSRQOL,utilization),
              baseline.conditions.HSRQOL.access = 
              c(baseline,conditions,HSRQOL,access), 
              baseline.conditions.HSRQOL.utilization.access = 
              c(baseline,conditions,HSRQOL,utilization, access) 
            )           
form.list <- lapply(lhs.vars, function(x) lapply(rhs.vars.list, function(y) 
              as.formula(paste(paste(x, "~"), paste(y, collapse= "+")))))                        
all.vars.form <- lapply(form.list, function(x) 
x$baseline.conditions.HSRQOL.utilization.access)
return(list( XY.dat=XY.dat, rhs.vars.list=rhs.vars.list, 
form.list= form.list, resp.vars = lhs.vars))     
}



