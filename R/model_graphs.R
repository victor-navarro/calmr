#' Create a graph with calmr data
#'
#' @param x A tbl with data to use in the plot
#' @param loops Logical. Whether to draw arrows back and forth
#' @param limits Numerical. Limits for color scale. Defaults to NULL,
#' in which case, limits are set to be max(abs(x$value))*c(-1,1)
#' @param colour_key Logical. Whether to show the color key
#' @param trial Numerical. The trial to graph.
#' @param options A list with graph options, as returned by `get_graph_opts`
#' @return A ggplot object
#' @note You should probably be getting graphs via
#' the `graph` method for CalmrExperiments.
#' @export
#' @rdname graph
#' @importFrom rlang .data

calmr_model_graph <- function(
    x, loops = TRUE,
    limits = max(abs(x$value)) * c(-1, 1), colour_key = FALSE,
    t = max(x$trial), options = get_graph_opts(), ...) {
  x <- stats::aggregate(value ~ s1 + s2, mean, data = x)
  x <- dplyr::rename(x, "from" = "s1", "to" = "s2")

  if (is.null(limits)) {
    limits <- max(abs(x$value)) * c(-1, 1)
  }
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
      size = options$edge.size,
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

make_graphs <- function(parsed_experiment,
                        limits = NULL,
                        t = NULL,
                        options = get_graph_opts()) {
  plotlist <- list()
  if ("eivs" %in% names(parsed_experiment@parsed_results)) {
    vs <- parsed_experiment@parsed_results$eivs %>%
      filter(assoc_type == "Net")
  } else {
    vs <- parsed_experiment@parsed_results$vs
  }
  if (is.null(t)) {
    t <- max(vs$trial)
  }
  if (is.null(limits)) {
    limits <- max(abs(range(vs$value))) * c(-1, 1)
  }

  for (g in unique(vs$group)) {
    gdat <- vs[vs$group == g, ]
    gtmax <- max(gdat$trial)
    plot_t <- t
    if (t > gtmax) {
      warning(sprintf(
        "Requested trial (%d) exceeds the maximum of
        trials (%d) for group %s, plotting the last trial.",
        t, gtmax, g
      ))
      plot_t <- gtmax
    }
    plotlist[[sprintf("Group %s: (Trial %d)", g, t)]] <- graph_weights(gdat,
      t = plot_t,
      limits = limits,
      options = options
    ) +
      ggplot2::labs(title = sprintf("%s (Trial %d)", g, plot_t))
  }
  plotlist
}

patch_graphs <- function(graphs, selection = names(graphs)) {
  # expects a list of graphs via make_graphs/graph_weights
  graphs <- graphs[selection]
  cow <- cowplot::plot_grid(plotlist = graphs)
  cow
}

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
