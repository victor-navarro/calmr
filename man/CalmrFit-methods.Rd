% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_fit.R
\name{show,CalmrFit-method}
\alias{show,CalmrFit-method}
\alias{predict,CalmrFit-method}
\alias{NLL,CalmrFit-method}
\alias{AIC,CalmrFit-method}
\alias{BIC,CalmrFit-method}
\title{CalmrFit Methods}
\usage{
\S4method{show}{CalmrFit}(object)

\S4method{predict}{CalmrFit}(object, type = "response", ...)

\S4method{NLL}{CalmrFit}(object)

\S4method{AIC}{CalmrFit}(object, ..., k = 2)

\S4method{BIC}{CalmrFit}(object, ...)
}
\description{
CalmrFit Methods
}
\details{
The AIC is defined as \code{2*k - 2*-NLL}, where k is a penalty
term and NLL is the negative log likelihood of the model.

The BIC is defined as \code{k*log(n) - 2*-NLL}, where k is the number
of parameters in the model and n is the number of observations
}