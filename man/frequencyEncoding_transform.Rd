% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequencyEncoding.R
\name{frequencyEncoding_transform}
\alias{frequencyEncoding_transform}
\title{frequencyEncoding_transform}
\usage{
frequencyEncoding_transform(dt, fit)
}
\arguments{
\item{dt}{data.table object}

\item{fit}{frequencyEncoding_fit object}

\item{x}{character vector of independet variables}
}
\description{
transform dataset using fit
}
\examples{
library(rAutoFE)
library(data.table)
data(churn, package = "rAutoFE")
data.table::setDT(churn)
splits <- rAutoFE::splitFrame(dt=churn, ratio = c(0.5, 0.3), seed = 1234)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]
x = c("State","Area.Code")
y = "Churn."
fit <- rAutoFE::frequencyEncoding_fit(dt = train, x = x)
saveRDS(fit,"fit.rds")
rm(fit)
fit <- readRDS("fit.rds")
train <- rAutoFE::frequencyEncoding_transform(dt = train, fit = fit)
valid <- rAutoFE::frequencyEncoding_transform(dt = valid, fit = fit)
test  <- rAutoFE::frequencyEncoding_transform(dt = test, fit = fit)
}
