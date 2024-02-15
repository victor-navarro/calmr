#' Check if object is Calmr experiment
#' @param object The object to be checked
#' @returns A logical
#' @export
is_experiment <- function(object) {
  inherits(object, "CalmrExperiment")
}


#' Go-to stops for calmr assertions
.calmr_assert <- function(what, given, ...) { # nolint: cyclocomp_linter.
  nargs <- list(...)
  switch(what,
    "supported_model" = {
      if (is.null(given)) {
        warning("No model passed. Using RW1972.",
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
      } else {
        # TODO: Implement parameter check
        return(given)
      }
    },
    "parsed_design" = {
      if (is.null(given)) {
        stop("Must provide a design data.frame or a parsed design.")
      } else {
        return(parse_design(given))
      }
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
        } else {
          return(given)
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
      if (!(given %in% nargs$supported)) {
        stop(sprintf("Plot not supported. The model does
        not contain '%s' in model results.", given),
          call. = FALSE
        )
      }
    },
    "good_experiment" = {
      if (any(unlist(lapply(
        given@arguments$mapping,
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
    },
    "length" = {
      if (!all(lapply(nargs, length) == given)) {
        stop(sprintf(
          "Function requires %s arguments to be equal to %d",
          paste0(names(nargs), collapse = ","), given
        ))
      }
    }
  )
}
