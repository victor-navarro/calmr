% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_model.R
\docType{class}
\name{CalmrModel-class}
\alias{CalmrModel-class}
\title{S4 class for calmr Models}
\description{
S4 class for calmr Models
}
\section{Slots}{

\describe{
\item{model_name}{A model name string}
\item{outputs}{A character vector with model outputs}
\item{parameters}{A list with the model with model parameters}
\item{default_parameters}{A list with the default model parameters}
\item{.internal_states}{A character vector with internal states}
\item{.is_timed}{A logical indicating if the model is timed}
\item{.associations}{A character vector with associations output name}
\item{.dnames_map}{A list with data names mapping for outputs}
\item{.parse_map}{A list with parse functions for outputs}
\item{.formula_map}{A list with formula mapping for outputs}
\item{.plots_map}{A list with plot functions for outputs}
\item{.last_experience}{A data.frame with the last experience run}
\item{.last_raw_results}{A list with the last raw results}
\item{.last_parsed_results}{A list with the last parsed results}
}
}

