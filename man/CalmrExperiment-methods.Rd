% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_experiment.R
\name{CalmrExperiment-methods}
\alias{CalmrExperiment-methods}
\alias{show,CalmrExperiment-method}
\alias{design,CalmrExperiment-method}
\alias{design}
\alias{trials,CalmrExperiment-method}
\alias{trials}
\alias{parameters,CalmrExperiment-method}
\alias{parameters}
\alias{parameters<-,CalmrExperiment-method}
\alias{parameters<-}
\alias{experiences,CalmrExperiment-method}
\alias{experiences}
\alias{experiences<-,CalmrExperiment-method}
\alias{experiences<-}
\alias{results,CalmrExperiment-method}
\alias{results}
\alias{raw_results,CalmrExperiment-method}
\alias{raw_results}
\alias{parsed_results,CalmrExperiment-method}
\alias{parsed_results}
\alias{length,CalmrExperiment-method}
\alias{parse,CalmrExperiment-method}
\alias{parse}
\alias{aggregate,CalmrExperiment-method}
\alias{aggregate}
\alias{plot,CalmrExperiment-method}
\alias{plot}
\alias{graph,CalmrExperiment-method}
\alias{graph}
\alias{timings,CalmrExperiment-method}
\alias{timings}
\alias{timings<-,CalmrExperiment-method}
\alias{timings<-}
\alias{filter,CalmrExperiment-method}
\alias{filter}
\title{CalmrExperiment methods}
\usage{
\S4method{show}{CalmrExperiment}(object)

\S4method{design}{CalmrExperiment}(x)

\S4method{trials}{CalmrExperiment}(object)

\S4method{parameters}{CalmrExperiment}(x)

\S4method{parameters}{CalmrExperiment}(x) <- value

\S4method{experiences}{CalmrExperiment}(x)

\S4method{experiences}{CalmrExperiment}(x) <- value

\S4method{results}{CalmrExperiment}(object)

\S4method{raw_results}{CalmrExperiment}(object)

\S4method{parsed_results}{CalmrExperiment}(object)

\S4method{length}{CalmrExperiment}(x)

\S4method{parse}{CalmrExperiment}(object, outputs = NULL)

\S4method{aggregate}{CalmrExperiment}(x, outputs = NULL)

\S4method{plot}{CalmrExperiment}(x, type = NULL, ...)

\S4method{graph}{CalmrExperiment}(x, ...)

\S4method{timings}{CalmrExperiment}(x)

\S4method{timings}{CalmrExperiment}(x) <- value

\S4method{filter}{CalmrExperiment}(x, trial_types = NULL, phases = NULL, stimuli = NULL)
}
\arguments{
\item{object, x}{A \code{CalmrExperiment} object.}

\item{value}{A list of parameters (or list of parameter lists).}

\item{outputs}{A character vector specifying the model outputs to parse.}

\item{type}{A character vector specifying the type(s) of plots to create.
Defaults to NULL. See \link{supported_plots}.}

\item{...}{Extra arguments passed to \code{\link[=calmr_model_graph]{calmr_model_graph()}}.}

\item{trial_types}{A character vector with trial types to filter.}

\item{phases}{A character vector with phase names to filter.}

\item{stimuli}{A character vector with stimulus names to filter.}
}
\value{
\code{show()} returns NULL (invisibly).

\code{design()} returns the \code{CalmrDesign} contained in the object.

\code{trials()} returns NULL (invisibly).

\code{parameters()} returns the list of parameters
contained in the object.

\verb{parameters()<-} returns the object after updating parameters.

\code{experiences()} returns a list of \code{data.frame} objects
containing model training routines.

\verb{experiences()<-} returns the object after updating experiences.

\code{results()} returns a \code{data.table} objects with aggregated results.

\code{raw_results()} returns a list with raw model results.

\code{parsed_results()} returns a list of \code{data.table}
objects with parsed results.

\code{length()} returns an integer specifying the total length
of the experiment (groups by iterations).

\code{parse()} returns the object after parsing raw results.

\code{aggregate()} returns the object after aggregating parsed results.

\code{plot()} returns a list of 'ggplot' plot objects.

\code{graph()} returns a list of 'ggplot' plot objects.

\code{timings()} returns the list of timings
contained in the object.

\verb{timings()<-} returns the object after updating timings.

\code{filter()} returns the object after filtering
parsed aggregated results
}
\description{
S4 methods for \code{CalmrExperiment} class.
}
