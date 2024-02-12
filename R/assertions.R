# Go-to stop for assertions and default arguments

.calmr_assert <- function(what, given, ...) { # nolint: cyclocomp_linter.
  narg <- list(...)
  switch(what,
    "supported_model" = {
      if (is.null(given)) {
        warning("No time model passed. Using RW1972.",
          call. = FALSE
        )
        return("RW1972")
      } else {
        if (!given %in% supported_models()) {
          stop("Model is not supported. Must be one returned by
           supported_models()",
            call. = FALSE
          )
        }
        return(given)
      }
    },
    "parameters" = {
      default_pars <- get_parameters(...)
      if (is.null(given)) {
        warning("No parameters were passed. Getting default values",
          call. = FALSE
        )
        return(default_pars)
      }
      # TODO: Implement parameter check
    },
    "experiment_options" = {
      if (is.null(given)) {
        warning("No experiment options passed. Using default options.",
          call. = FALSE
        )
        return(get_exp_opts())
      } else {
        if (length(setdiff(names(given), names(given))) > 0) {
          stop("Did not supply proper options. Please see ?get_exp_opts",
            call. = FALSE
          )
        }
      }
    },
    "supported_optimizer" = {
      if (is.null(given)) {
        warning("No optimizer passed. Using 'optim'.",
          call. = FALSE
        )
        return("optim")
      } else {
        if (!given %in% supported_optimizers()) {
          stop("Optimizer is not supported. Must be one
          returned by supported_optimizers()",
            call. = FALSE
          )
        }
      }
    },
    "supported_family" = {
      if (is.null(given)) {
        warning("No family passed. Using 'identity'",
          call. = FALSE
        )
        return("identity")
      } else {
        if (!given %in% supported_families()) {
          stop("Family is not supported. Must be one
          returned by supported_families()",
            call. = FALSE
          )
        }
      }
    },
    "limits_OK" = {
      if (any(is.na(given$ll)) | any(is.na(given$ul))) {
        stop("Did not supply limits for all parameters estimated.
        Count your parameters. Please see ?fit_model",
          call. = FALSE
        )
      }
    },
    "filepath_OK" = {
      if (!file.exists(dirname(given))) {
        stop(sprintf("Path to file (%s) does not exist.", given),
          call. = FALSE
        )
      }
    },
    "no_functional_stimuli" = {
      if (
        length(given$unique_nominal_stimuli) >
          length(given$unique_functional_stimuli)
      ) {
        stop("The model does not support functional/nominal
          stimuli specifications.",
          call. = FALSE
        )
      }
    },
    "supported_plot" = {
      if (!(given %in% narg$supported)) {
        stop(sprintf("Plot not supported. The model does
        not contain '%s' in model results.", given),
          call. = FALSE
        )
      }
    },
    "good_experiment" = {
      if (any(unlist(lapply(
        given$mapping,
        function(x) length(x$unique_nominal_stimuli)
      )) == 1)) {
        stop("Experiment is too simple to run for one or more groups.
        Please check your design.",
          call. = FALSE
        )
      }
    },
    "comparator_order" = {
      if (length(unique(given)) > 1) {
        stop("Multiple orders for comparison process are
        not currently supported. Please make sure the
        orders column only contains one value.")
      }
    }
  )
}
