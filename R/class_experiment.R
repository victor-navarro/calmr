#' S4 class for calmr experiments.
#' @section Slots:
#' \describe{
#' \item{\code{arguments}:}{A tbl containing arguments to run models.}
#' \item{\code{design}:}{A CalmrDesign object.}
#' \item{\code{design}:}{A CalmrExperimentResult object.}
#' }
#' @rdname CalmrExperiment
#' @exportClass CalmrExperiment
#' @seealso CalmrExperiment-methods

methods::setClass(
  "CalmrExperiment",
  representation(
    arguments = "tbl",
    design = "CalmrDesign",
    results = "CalmrExperimentResult"
  )
)

#' @title CalmrExperiment methods
#' @rdname CalmrExperiment-methods
#' @export
setMethod("show", "CalmrExperiment", function(object) {
  print(object@arguments)
})

methods::setGeneric("design", function(x) methods::standardGeneric("design"))
#' @export
#' @aliases design
#' @rdname CalmrExperiment-methods
methods::setMethod("design", "CalmrExperiment", function(x) {
  x@design
})


methods::setGeneric(
  "arguments",
  function(x) methods::standardGeneric("arguments")
)
#' @export
#' @aliases arguments
#' @rdname CalmrExperiment-methods
methods::setMethod("arguments", "CalmrExperiment", function(x) {
  x@arguments
})

#' @export
#' @rdname CalmrExperiment-methods
#' @examples
#' # Concatenate designs
#' df <- get_design("blocking")
#' pars1 <- get_parameters(df, model = "RW1972")
#' pars2 <- get_parameters(df, model = "MAC1975")
#' ex1 <- make_experiment(df, model = "RW1972")
#' ex2 <- make_experiment(df, model = "MAC1975")
#' c(ex1, ex2)
#'
#' # Concatenate results
#' lapply(c(ex1, ex2), run_experiment)
methods::setMethod("c", "CalmrExperiment", function(x, ..., recursive = FALSE) {
  allexps <- list(x, ...)
  methods::new("CalmrExperiment",
    arguments = dplyr::bind_rows(lapply(allexps, function(e) e@arguments)),
    design = allexps[[1]]@design,
    results = do.call(c, lapply(allexps, function(e) e@results))
  )
})


methods::setGeneric("parameters", function(x) standardGeneric("parameters"))
methods::setGeneric(
  "parameters<-",
  function(x, value) standardGeneric("parameters<-")
)
#' @rdname CalmrExperiment-methods
#' @aliases parameters
#' @export
methods::setMethod(
  "parameters", "CalmrExperiment",
  function(x) x@arguments$parameters
)
#' @rdname CalmrExperiment-methods
#' @export
methods::setMethod("parameters<-", "CalmrExperiment", function(x, value) {
  if (length(names(value))) {
    # If there are names in the parameters,
    # this is meant to be a single list
    value <- list(value)
  } else {
    if (length(value) != length(x)) {
      stop(sprintf("Length of parameters list
      must be equal to the length of the experiment (%s)", length(x)))
    }
  }
  x@arguments$parameters <- value
  x
})


methods::setGeneric("experience", function(x) standardGeneric("experience"))
methods::setGeneric(
  "experience<-",
  function(x, value) standardGeneric("experience<-")
)
#' @rdname CalmrExperiment-methods
#' @aliases experience
#' @export
methods::setMethod(
  "experience", "CalmrExperiment",
  function(x) x@arguments$experience
)
#' @rdname CalmrExperiment-methods
#' @export
methods::setMethod("experience<-", "CalmrExperiment", function(x, value) {
  if (length(names(value))) {
    # If there are names in the experience,
    # this is meant to be a single list
    value <- list(value)
  } else {
    if (length(value) != length(x)) {
      stop(sprintf("Length of experience list
      must be equal to the length of the experiment (%s)", length(x)))
    }
  }
  x@arguments$experience <- value
  x
})


methods::setGeneric(
  "results",
  function(object) methods::standardGeneric("results")
)
#' @rdname CalmrExperiment-methods
#' @aliases results
#' @export
methods::setMethod("results", "CalmrExperiment", function(object) {
  # Returns aggregated results
  object@results@aggregated_results
})

methods::setGeneric(
  "raw_results",
  function(object) methods::standardGeneric("raw_results")
)
#' @rdname CalmrExperiment-methods
#' @aliases raw_results
#' @export
methods::setMethod("raw_results", "CalmrExperiment", function(object) {
  # Returns raw results
  object@results@raw_results
})

methods::setGeneric(
  "parsed_results",
  function(object) methods::standardGeneric("parsed_results")
)
#' @rdname CalmrExperiment-methods
#' @aliases parsed_results
#' @export
methods::setMethod("parsed_results", "CalmrExperiment", function(object) {
  # Returns raw results
  object@results@parsed_results
})

#' @rdname CalmrExperiment-methods
#' @export
methods::setMethod("length", "CalmrExperiment", function(x) {
  if (!is.null(x@arguments)) {
    nrow(x@arguments)
  } else {
    NULL
  }
})

setGeneric(
  "parse",
  function(object) methods::standardGeneric("parse")
)
#' @rdname CalmrExperiment-methods
#' @export
methods::setMethod(
  "parse", "CalmrExperiment",
  function(object) {
    if (!is.null(object@results@raw_results)) {
      # we gotta parse
      n <- length(object)
      pb <- progressr::progressor(n)
      .parallel_standby(pb) # print parallel backend message
      object@results@parsed_results <- future.apply::future_sapply(
        seq_len(n), function(r) {
          pb("Parsing results")
          .parse_model(
            object@results@raw_results[[r]],
            # insane stuff due to tbls being stupid
            unlist(as.list(object@arguments[r, ]), recursive = FALSE)
          )
        },
        simplify = FALSE
      )
    } else {
      stop("Found no raw_results to parse.")
    }
    object
  }
)

#' @rdname CalmrExperiment-methods
#' @export
methods::setMethod(
  "aggregate", "CalmrExperiment",
  function(x, ...) {
    if (is.null(x@results@parsed_results)) {
      x <- parse(x)
    }
    res <- .aggregate_experiment(x, ...)
    tbl <- do.call(
      dplyr::bind_rows,
      sapply(names(res), function(m) {
        tibble::tibble(model = m, tibble::as_tibble(lapply(res[[m]], list)))
      }, simplify = FALSE)
    )
    x@results@aggregated_results <- sapply(names(tbl)[-1], function(o) {
      dat <- tbl[, c("model", o)]
      tidyr::unnest(dat, tidyselect::all_of(o))
    }, simplify = FALSE)

    x
  }
)

#' Plot CalmrExperiment
#'
#' Creates plots (or plot) with aggregated results in CalmrExperiment
#'
#' @param x An object of class \code{\link{CalmrExperiment}}.
#' @param type character vector specifying the types of plots to create.
#' See ??supported_plots. Defaults to NULL.
#' @return A ggplot object
#' @note With type = NULL, all supported plots are returned.
#' @export
#' @rdname plot
setGeneric("plot", function(x, y, ...) methods::standardGeneric("plot"))
setMethod(
  "plot", "CalmrExperiment",
  function(x, type = NULL, ...) {
    if (is.null(x@results@aggregated_results)) {
      stop("Experiment does not contain aggregated results.
      Please parse and aggregate results beforehand.")
    }
    # get aggregated results
    res <- results(x)
    plots <- list()
    models <- unique(x@arguments$model)
    # Go through each row
    for (m in models) {
      model_plots <- supported_plots(m)
      if (!is.null(type)) {
        sapply(type, .calmr_assert, supported = model_plots)
        model_plots <- type
      }
      for (p in model_plots) {
        odat <- res[[p]]
        pdat <- odat[odat$model == m, ]
        groups <- unique(pdat$group)
        for (g in groups) {
          plot_name <- sprintf("%s - %s (%s)", g, .get_prettyname(p), m)
          plots[[plot_name]] <- calmr_model_plot(pdat[pdat$group == g, ],
            type = p
          )
        }
      }
    }
    plots
  }
)

setGeneric("graph", function(x, ...) standardGeneric("graph"))
#' @export
#' @rdname graph
setMethod("graph", "CalmrExperiment", function(x, ...) {
  if (is.null(x@results@aggregated_results)) {
    stop("Experiment does not contain aggregated results.
      Please parse and aggregate results beforehand.")
  }
  # get aggregated results
  res <- results(x)
  graphs <- list()
  models <- unique(x@arguments$model)
  for (m in models) {
    assoc_output <- .model_associations(m)
    odat <- res[[assoc_output]]
    weights <- odat[odat$model == m, ]
    if (assoc_output == c("eivs")) {
      evs <- weights[weights$type == "evs", ]
      ivs <- weights[weights$type == "ivs", ]
      weights <- evs
      weights$value <- weights$value - ivs$value
    }
    groups <- unique(weights$group)
    mgraphs <- list()
    for (g in groups) {
      graph_name <- sprintf("%s - Associations (%s)", g, m)
      mgraphs[[graph_name]] <- calmr_model_graph(
        weights[weights$group == g, ], ...
      ) + ggplot2::labs(title = graph_name)
    }
    graphs[[m]] <- mgraphs
  }
  graphs
})


# Set generic here for rsa function (see rsa.R)
methods::setGeneric("rsa", function(x, layers, ...) standardGeneric("rsa"))

#' Perform representational similarity analysis on CalmrExperiment
#'
#' @param x A tbl of m by o (models by outputs) with aggregated results.
#' @param comparisons A model-named list containing the model
#' outputs to compare.
#' @param test Whether to test the RSA via permutation test. Default = FALSE.
#' @param ... Additional parameters passed to `stats::dist`
#' and `stats::cor`
#' @returns A CalmrRSA object
#' @note The object returned by this function
#' can be later tested via its own `test` method.
#' @rdname rsa
#' @export
#' @examples
#' # Comparing the associations in three models
#' exp <- data.frame(
#'   Group = c("A", "B"),
#'   P1 = c("2(A)>(US)/1B>(US)", "1(A)>(US)/2B>(US)"),
#'   R1 = TRUE
#' )
#' exp <- parse_design(exp)
#' models <- c("HD2022", "RW1972", "PKH1982")
#' parameters <- sapply(models, get_parameters, design = exp)
#' options <- get_exp_opts()
#' exp_res <- compare_models(exp,
#'   models = models,
#'   parameters = parameters, options = options
#' )
#' comparisons <- list(
#'   "HD2022" = c("vs"),
#'   "RW1972" = c("vs"),
#'   "PKH1982" = c("eivs")
#' )
#' res <- rsa(exp_res, comparisons = comparisons)
#' test(res, n_samples = 100)
methods::setMethod(
  "rsa", "CalmrExperiment",
  function(x, comparisons, test = FALSE, ...) {
    .rsa(results(x), comparisons = comparisons, .test = test, ...)
  }
)
