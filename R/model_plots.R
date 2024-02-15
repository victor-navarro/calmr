#' Create a plot with calmr data
#'
#' @param dat A tbl with data to use in the plot
#' @param type A character specifying the type of plot
#' @return A ggplot object
#' @note You should probably be getting plots via
#' the `plot` method for CalmrExperiments.
#' @export
#' @importFrom rlang .data

calmr_model_plot <- function(dat, type) {
  # recalculate trial
  dat$trial <- ceiling(dat$trial / dat$block_size)
  line_point <- list(
    ggplot2::stat_summary(geom = "line", fun = "mean"),
    ggplot2::stat_summary(
      geom = "point",
      fun = "mean", position = ggbeeswarm::position_quasirandom()
    )
  )

  # Assemble aesthetics
  if (type %in% c("vs", "rs", "acts", "relacts")) {
    .aes <- ggplot2::aes(x = .data$trial, y = .data$value, colour = .data$s2)
  }
  if (type %in% c("as")) {
    .aes <- ggplot2::aes(x = .data$trial, y = .data$value, colour = .data$s1)
  }
  if (type %in% c("os")) {
    .aes <- ggplot2::aes(x = .data$trial, y = .data$value, colour = .data$comp)
  }
  if ("type" %in% names(dat)) {
    .aes <- ggplot2::aes(
      x = .data$trial, y = .data$value,
      colour = .data$s2, linetype = type
    )
  }

  # Assemble geoms
  geoms <- line_point

  # Assemble labels
  labels <- list(ggplot2::labs(
    y = .get_prettyname(type),
    x = "Trial/Miniblock"
  ))
  if (type %in% c("vs", "rs", "eivs", "acts", "relacts")) {
    labels <- c(labels, list(ggplot2::labs(colour = "Target")))
  }
  if (type %in% c("as")) {
    labels <- c(labels, list(ggplot2::labs(colour = "Stimulus")))
  }
  if (type %in% c("os")) {
    labels <- c(labels, list(ggplot2::labs(colour = "Comparison")))
  }
  if ("type" %in% names(dat)) {
    labels <- c(labels, list(ggplot2::labs(linetype = "Type")))
  }

  # Assemble scales
  scales <- .calmr_scales(c("colour_d", "fill_d"))
  if (type %in% c("vs")) {
    scales <- c(scales, ggplot2::scale_x_continuous(breaks = NULL))
  }

  # Define grid
  grid <- list()
  if (type %in% c("vs", "rs", "eivs")) {
    grid <- ggplot2::facet_grid(.data$s1 ~ .data$phase, scales = "free_x")
  }
  if (type %in% c("acts", "relacts")) {
    grid <- ggplot2::facet_grid(
      .data$s2 ~ .data$phase + .data$trial_type,
      scales = "free_x"
    )
  }
  if (type %in% "os") {
    grid <- ggplot2::facet_grid(
      .data$s1 ~ .data$s2 + .data$phase,
      scales = "free_x", switch = "y"
    )
  }
  if (type %in% c("as")) {
    grid <- ggplot2::facet_grid(
      . ~ .data$phase +
        .data$trial_type,
      scales = "free_x"
    )
  }
  ggplot2::ggplot(data = dat, mapping = .aes) +
    ggplot2::theme_bw() +
    geoms +
    labels +
    scales +
    grid
}

# internal function to define and make scales available
.calmr_scales <- function(which) {
  scales <- list(
    colour_d = ggplot2::scale_colour_viridis_d(begin = .1, end = .9),
    fill_d = ggplot2::scale_fill_viridis_d(begin = .1, end = .9)
  )
  scales[which]
}


.get_prettyname <- function(output) {
  prettynames <- c(
    "vs" = "Association Strength",
    "rs" = "Response Strength",
    "as" = "Saliency",
    "os" = "Switch Value",
    "eivs" = "Association Strength",
    "acts" = "Activation Strength",
    "relacts" = "Relative Activation"
  )
  prettynames[output]
}


make_plots <- function(parsed_experiment) {
  if (!is.null(parsed_experiment)) {
    if (!("CalmrExperiment" %in% class(parsed_experiment))) {
      stop("parsed_experiment must be a CalmrExperiment")
    }
    parsed_results <- parsed_experiment@parsed_results
    plot_types <- names(parsed_results)
    if ("ivs" %in% plot_types) {
      plot_types <- plot_types[plot_types != "ivs"]
    }
    plot_funs <- .get_plot_functions(plot_types)
    plotlist <- list()
    for (g in unique(parsed_results[[1]]$group)) {
      for (p in 1:length(plot_funs)) {
        if (plot_types[p] == "evs") {
          dat <- parsed_results$evs
          dat$value <- dat$value - parsed_results$ivs$value
        } else {
          dat <- parsed_results[[p]]
        }
        dat <- dat %>% dplyr::filter(.data$group == g)
        plotlist[[sprintf("%s: %s", g, plot_funs[[p]]$name)]] <-
          plot_funs[[p]]$fun(dat)
      }
    }
    plotlist
  }
}

.get_plot_functions <- function(name) {
  defs <- list(
    "as" = list(fun = plot_as, name = "Alphas"),
    "acts" = list(fun = plot_acts, name = "Activations"),
    "relacts" = list(fun = plot_acts, name = "Relative Activations"),
    "rs" = list(fun = plot_rs, name = "Responses"),
    "vs" = list(fun = plot_vs, name = "Associations"),
    "es" = list(fun = plot_es, name = "Expectations"),
    "eivs" = list(fun = plot_eivs, name = "Associations"),
    "os" = list(fun = plot_os, name = "Operator Switches")
  )
  defs[name]
}

plot_common_scale <- function(plots) {
  # get min and max y-scale
  ranges <- unlist(lapply(plots, function(p) {
    ggplot2::layer_scales(p)$y$range$range
  }))
  miny <- min(ranges)
  maxy <- max(ranges)
  for (p in 1:length(plots)) {
    plots[[p]] <- plots[[p]] + ggplot2::coord_cartesian(ylim = c(miny, maxy))
  }
  plots
}

get_plot_opts <- function(common_scale = TRUE) {
  return(list(common_scale = common_scale))
}

graph_weights <- function(
    weights, loops = TRUE,
    limits = NULL, colour_key = FALSE,
    t = max(weights$trial), graph_opts = get_graph_opts()) {
  weights <- weights %>%
    dplyr::filter(.data$trial == t) %>%
    dplyr::group_by(.data$s1, .data$s2) %>%
    dplyr::summarise(value = mean(.data$value)) %>%
    dplyr::mutate(s1 = as.character(.data$s1), s2 = as.character(.data$s2)) %>%
    dplyr::rename(from = .data$s1, to = .data$s2, weight = .data$value) %>%
    as.data.frame()
  if (is.null(limits)) {
    limits <- max(abs(range(weights$weight))) * c(-1, 1)
  }
  net <- ggnetwork::ggnetwork(network::as.network(weights, loops = loops),
    layout = "circle",
    arrow.gap = graph_opts$arrow.gap
  )
  p <- ggplot2::ggplot(net, ggplot2::aes(
    x = .data$x, y = .data$y,
    xend = .data$xend, yend = .data$yend,
    colour = .data$weight,
    label = .data$vertex.names
  )) +
    ggnetwork::geom_edges(
      curvature = graph_opts$arrow.curvature,
      size = graph_opts$edge.size,
      arrow = grid::arrow(
        length = grid::unit(graph_opts$arrow.pt, "pt"),
        type = "closed"
      )
    ) +
    ggnetwork::geom_nodes(
      size = graph_opts$node.size, pch = 21, colour = "black",
      fill = "white", stroke = graph_opts$node.stroke
    ) +
    ggnetwork::geom_nodetext(
      size = graph_opts$node.text.size,
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
                        graph_opts = get_graph_opts()) {
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
      graph_opts = graph_opts
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

patch_plots <- function(
    plots, selection = NULL,
    type = NULL, plot_options = get_plot_opts()) {
  type_mapping <- data.frame(
    type = c(
      "vs", "rs_simple", "rs_complex",
      "acts_learning", "acts_bar", "as"
    ),
    str = c(
      ": Vs", ": Rs \\(simple\\)", ": Rs \\(complex\\)",
      ": Acts \\(learning\\)", ": Acts \\(bar\\)", ": As"
    )
  )

  # argument checks
  if (is.null(selection) & is.null(type)) {
    stop("You must pass either a selection or a type.")
  }
  if (!is.null(selection)) {
    if (!all(selection %in% names(plots))) {
      stop("Selection must match names in plots")
    }
  }
  if (!is.null(type)) {
    if (!all(type %in% type_mapping$type)) {
      stop(c(
        "Argument type must be one of ",
        paste(type_mapping$type, collapse = ", ")
      ))
    }
    if (length(type) > 1) {
      stop(c("Argument type must be length 1."))
    }
    pnames <- names(plots)
    selection <- pnames[grepl(
      type_mapping$str[type_mapping$type == type], pnames
    )]
  }
  cow <- NULL
  selected <- length(selection)
  if (selected) {
    plots <- plots[selection]
    # if we want common scales
    if (plot_options$common_scale & selected > 1) {
      plots <- plot_common_scale(plots)
    }
    cow <- cowplot::plot_grid(plotlist = plots)
  }
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
