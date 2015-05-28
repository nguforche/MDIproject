# MDIproject

R package for the MDI research project on identifying high need medicaid enrollees using self reported health measures such as reported baseline demographics, desease conditions, health quality of life, access to health care, and past history of health care resource utilization. Seven machine learning models have been implemented:  "GLM", "ELR", "GBM", "bagFDA", "RUSBoostGLM", "RUSBoostELR", and "RUSBoostGBM". [ELR](https://github.com/nguforche/ELR) is the Extreme Logistic regression. GLM, GBM and bagFDA are implemeted via the [caret](http://cran.r-project.org/web/packages/caret/index.html) package while the RUSBoost* methods are modifications of the [RUSBoost](https://github.com/SteveOhh/RUSBoost) algorithm. Specificaly, the algorithm is a combination of the boosting algorith and random undersampling of the majority class. The weak learner "rpart" in RUSBoost is simply replaced with "GLM", "ELR", and "GBM" which are not necessary weak learners. Yes,"RUSBoostGBM" is boosting ontop of boosting! 

## Who can Use  MDIproject. 
This is an ongoing project. The implemeted algorithms are readily available for any classification task. 
 

## How to get Started? 
Install via devtools: 

```sh
> devtools::install_github("nguforche/MDIproject")
```
## Parameters


 
  

