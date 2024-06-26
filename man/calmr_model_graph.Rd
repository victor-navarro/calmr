% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_graphs.R
\name{calmr_model_graph}
\alias{calmr_model_graph}
\alias{patch_graphs}
\alias{get_graph_opts}
\title{Create a graph with calmr data}
\usage{
calmr_model_graph(
  x,
  loops = TRUE,
  limits = max(abs(x$value)) * c(-1, 1),
  colour_key = FALSE,
  t = max(x$trial),
  options = get_graph_opts()
)

patch_graphs(graphs, selection = names(graphs))

get_graph_opts(graph_size = "small")
}
\arguments{
\item{x}{A \code{data.frame}-like with data to use in the plot.
Contains a column named \code{value}.}

\item{loops}{Logical. Whether to draw arrows back and forth}

\item{limits}{Numerical. Limits for color scale.
Defaults to max(abs(x$value))*c(-1,1).}

\item{colour_key}{Logical. Whether to show the color key}

\item{t}{The trial from which weights are obtained
(defaults to the maximum trial in the data).}

\item{options}{A list with graph options, as returned by \code{\link[=get_graph_opts]{get_graph_opts()}}.}

\item{graphs}{A list of (named) graphs, as returned by \code{\link[=graph]{graph()}} or
\code{\link[=calmr_model_graph]{calmr_model_graph()}}}

\item{selection}{A character or numeric vector
determining the plots to patch.}

\item{graph_size}{A string (either "small" or "large").
to return default values for small or large graphs}

\item{trial}{Numerical. The trial to graph.}
}
\value{
A 'ggplot' object

\code{patch_graphs()} returns a 'patchwork' object

A list with graph options, to be passed to \code{ggnetwork::geom_nodes()}.
}
\description{
\code{patch_graphs()} patches graphs with 'patchwork'
}
\note{
You should probably be getting graphs via
the graph method for \linkS4class{CalmrExperiment}.
}
