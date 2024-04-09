#' Create a graph with calmr data
#'
#' @param x A `data.frame`-like with data to use in the plot.
#' Contains a column named `value`.
#' @param loops Logical. Whether to draw arrows back and forth
#' @param limits Numerical. Limits for color scale.
#' Defaults to max(abs(x$value))*c(-1,1).
#' @param colour_key Logical. Whether to show the color key
#' @param trial Numerical. The trial to graph.
#' @param options A list with graph options, as returned by [get_graph_opts()].
#' @param t The trial from which weights are obtained
#' (defaults to the maximum trial in the data).
#' @return A 'ggplot' object
#' @note You should probably be getting graphs via
#' the graph method for [CalmrExperiment-class].
#' @export
#' @rdname calmr_model_graph
#' @importFrom rlang .data
calmr_model_graph <- function(
    x, loops = TRUE,
    limits = max(abs(x$value)) * c(-1, 1),
    colour_key = FALSE,
    t = max(x$trial),
    options = get_graph_opts()) {
  trial <- value <- NULL # local binding
  # check if trial is valid
  if (t > max(x$trial)) {
    warning("Requested trial exceeds that found in data. Using last trial.")
    t <- max(x$trial)
  }
  # aggregate data
  x <- data.table::setDT(x)[trial == t,
    list("value" = mean(value)),
    by = c("s1", "s2")
  ]
  net <- ggnetwork::ggnetwork(network::as.network(x, loops = loops),
    layout = "circle",
    arrow.gap = options$arrow.gap
  )
  p <- ggplot2::ggplot(net, ggplot2::aes(
    x = .data$x, y = .data$y,
    xend = .data$xend, yend = .data$yend,
    colour = .data$value,
    label = .data$vertex.names
  )) +
    ggnetwork::geom_edges(
      curvature = options$arrow.curvature,
      linewidth = options$edge.size,
      arrow = grid::arrow(
        length = grid::unit(options$arrow.pt, "pt"),
        type = "closed"
      )
    ) +
    ggnetwork::geom_nodes(
      size = options$node.size, pch = 21, colour = "black",
      fill = "white", stroke = options$node.stroke
    ) +
    ggnetwork::geom_nodetext(
      size = options$node.text.size,
      colour = "black"
    ) +
    ggplot2::scale_colour_gradient2(
      high = "#fde725",
      low = "#440154", mid = "gray", midpoint = 0, limits = limits
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_cartesian(xlim = c(-0.2, 1.2), ylim = c(-0.2, 1.2)) +
    ggplot2::labs(colour = "Strength")
  if (!colour_key) {
    p <- p + ggplot2::guides(colour = "none")
  }
  p
}

#' @description `patch_graphs()` patches graphs with 'patchwork'
#' @param graphs A list of (named) graphs, as returned by [graph()] or
#' [calmr_model_graph()]
#' @param selection A character or numeric vector
#' determining the plots to patch.
#' @aliases patch_graphs
#' @rdname calmr_model_graph
#' @export
#' @return `patch_graphs()` returns a 'patchwork' object

patch_graphs <- function(graphs, selection = names(graphs)) {
  # unlist graphs
  gnames <- unlist(unname(lapply(graphs, names)))
  graphs <- stats::setNames(
    unlist(graphs,
      recursive = FALSE,
      use.names = FALSE
    ), gnames
  )
  graphs <- graphs[selection]
  patch <- patchwork::wrap_plots(graphs)
  patch
}

#' Get options for calmr graph
#' @param graph_size A string (either "small" or "large").
#' to return default values for small or large graphs
#' @return A list with graph options, to be passed to `ggnetwork::geom_nodes()`.
#' @aliases get_graph_opts
#' @rdname calmr_model_graph
#' @export
get_graph_opts <- function(graph_size = "small") {
  if (graph_size == "large") {
    arrow.gap <- 0.16
    arrow.curvature <- 0.2
    arrow.pt <- 20
    edge.size <- 3
    node.size <- 40
    node.stroke <- 3
    node.text.size <- 15
  }
  if (graph_size == "small") {
    arrow.gap <- 0.10
    arrow.curvature <- 0.2
    arrow.pt <- 10
    edge.size <- 1.5
    node.size <- 20
    node.stroke <- 1.5
    node.text.size <- 7.5
  }
  return(list(
    arrow.gap = arrow.gap,
    arrow.curvature = arrow.curvature,
    arrow.pt = arrow.pt,
    edge.size = edge.size,
    node.size = node.size,
    node.stroke = node.stroke,
    node.text.size = node.text.size
  ))
}
