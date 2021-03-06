% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Train.R
\name{Train}
\alias{Performance.MultiTaskELR}
\alias{Train}
\alias{Train.Validate.Test}
\alias{Train.Validate.Test.Boot}
\alias{TrainAllModels}
\alias{approxMultiTaskELR.BigV2}
\alias{approxMultiTaskELR.BigV2.tune}
\title{Train, Validate and Test various models}
\usage{
TrainAllModels()

Train.Validate.Test(classifier, X.trn, Y.trn, X.val, Y.val, X.tst, Y.tst, para,
  opt.para = FALSE, return.model = FALSE)

Train.Validate.Test.Boot(classifier, X.trn, Y.trn, X.val, Y.val, X.tst, Y.tst,
  STL.para, opt.para = FALSE, return.model = FALSE)

approxMultiTaskELR.BigV2.tune(form, ...)

approxMultiTaskELR.BigV2(form, ...)

Performance.MultiTaskELR(res, ...)
}
\arguments{
\item{classifier}{character list of classification models. See names(TrainAllModels()).}

\item{X.trn,X.val,X.tst}{matrix of predictors}

\item{Y.trn,Y.val,Y.tst}{matrix of true binary 0,1 class}

\item{para,STL.para}{named list of model parameters}

\item{opt.para}{logical for parameter tuning. defaul to FALSE}

\item{return.model}{logical to return trained model or not}

\item{form}{formula}

\item{res}{MTL model}

\item{\dots}{further arguments passed to or from other methods.}
}
\value{
list of functions for training various algorithms
}
\description{
Functions for training, validation and test a number of machine
learning classification models with or without parameter tuning
}
\examples{
TrainModels <- lapply(TrainAllModels(), function(x) x)
}
\author{
Che Ngufor <Ngufor.Che@mayo.edu>
}

