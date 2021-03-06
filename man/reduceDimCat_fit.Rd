% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reduceDimCat.R
\name{reduceDimCat_fit}
\alias{reduceDimCat_fit}
\title{reduceDimCat_fit}
\usage{
reduceDimCat_fit(data, column_name, min_percentage, max_numOflevel)
}
\arguments{
\item{dt}{data.table object}
}
\description{
reduceDimCat_fit
}
\examples{
library(data.table)
library(rAutoFE)
data(churn)
data <- as.data.table(churn)
data <- convert2fac(dt=data, columns=c("Churn.","State","Area.Code","Int.l.Plan","VMail.Plan"))
splits <- splitFrame(dt=data, ratio = c(0.5, 0.2), seed=1234)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]
fit <- reduceDimCat_fit(data=train, column_name=c("State", "Area.Code"), min_percentage=0.01, max_numOflevel=30)
}
