#' S4 class for Calmr representational similarity analysis
#'
#' @section Slots:
#' \describe{
#' \item{\code{corr_mat}:}{Array. Correlation matrix}
#' \item{\code{distances}:}{List. Pairwise distance matrices}
#' \item{\code{arguments}:}{List. Arguments used to create the object.}
#' \item{\code{test_data}:}{List.
#' Test data, populated after testing the object.}
#' }
#' @exportClass CalmrRSA
setClass(
  "CalmrRSA",
  representation(
    corr_mat = "array",
    distances = "list",
    args = "list",
    test_data = "list"
  )
)

#' CalmrRSA Methods
#' @param object A CalmrRSA object
#' @export
#' @rdname CalmrRSA-methods
setMethod("show", "CalmrRSA", function(object) {
  cat("CalmrRSA object\n")
  cat("---------------\n")
  cat("Correlation matrix:\n")
  print(object@corr_mat)
  if (!is.null(object@test_data)) {
    cat("\nSignificance matrix:\n\n")
    print(object@test_data$sig_mat)
    cat(sprintf(
      "\n%d permutation samples, two-tailed test with alpha = %1.2f.\n",
      object@test_data$n_samples, 1 - object@test_data$p
    ))
  }
})


methods::setGeneric(
  "test",
  function(object, n_samples = 1e3, p = .95) standardGeneric("test")
)
#' Test CalmrRSA object via permutation test
#'
#' @param object A CalmrRSA object
#' @param n_samples The number of samples for the permutation test
#' (default = 1e3)
#' @param p The critical threshold level for the permutation test
#' (default = 0.95)
#' @return A CalmrRSA object with the test results
#' @rdname rsa
#' @export
methods::setMethod("test", "CalmrRSA", function(
    object, n_samples, p) {
  .rsa_test(object, n_samples = n_samples, p = p)
})

#' @param object A CalmrRSA object
#' @param ... Extra parameters passed to the plot call
#' @rdname rsa
#' @export
setMethod(
  "plot", "CalmrRSA",
  function(x, ...) {
    p <- NULL
    corrmat <- x@corr_mat
    corrmat[lower.tri(corrmat)] <- NA
    dat <- data.frame(as.table(corrmat))
    dat$label <- round(dat$Freq, 2)
    p <- dat %>%
      stats::na.omit() %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$Var1, y = .data$Var2,
        fill = .data$Freq, label = .data$label
      )) +
      ggplot2::geom_tile(na.rm = TRUE) +
      ggplot2::geom_text(na.rm = TRUE) +
      .calmr_scales("fill_c", limits = c(-1, 1)) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank()
      ) +
      ggplot2::labs(fill = "Correlation") +
      ggplot2::scale_x_discrete(position = "top")
    if (length(x@test_data)) {
      sigmat <- x@test_data$sig_mat
      sigmat[lower.tri(sigmat)] <- NA
      dat <- p$data
      dat$sig <- data.frame(as.table(sigmat))$Freq
      p <- p + ggplot2::geom_label(
        data = stats::na.omit(dat[dat$sig, ]), fill = "white"
      )
    }
    p
  }
)
