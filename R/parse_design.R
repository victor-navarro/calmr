#' Parse design data.frame
#' @param df A data.frame of dimensions Groups, 2*Phases+1
#' @param model An optional model to augment the design. See ??augment design
#' @param ... Other arguments passed to augment
#' @return A CalmDesign object
#' @note
#' \itemize{
#' \item{
#' Each entry in even-numbered columns of df is
#' a string formatted as per phase_parser.
#' }
#' }
#' @examples
#' df <- data.frame(
#'   Group = c("Group 1", "Group 2"),
#'   P1 = c("10AB(US)", "10A(US)"), R1 = c(TRUE, TRUE)
#' )
#' parse_design(df)
#' @seealso \code{\link{phase_parser}}
#' @export

parse_design <- function(df, model = NULL, ...) {
  if (!is.null(model)) {
    .calm_assert("length", 1, model = model)
  }
  # if already parsed, skip
  if ("CalmDesign" %in% class(df)) {
    design_obj <- df
    # augment if design hasn't been augmented
    if (!is.null(model) && !df@augmented) {
      design_obj <- augment(design_obj, model = model, ...)
    }
  } else {
    ri <- seq(2, ncol(df), 2)
    design <- apply(df, 1, function(r) {
      sapply(ri, function(p) {
        list(
          group = r[[1]],
          phase = names(df)[p],
          parse_string = r[[p]],
          randomize = r[[p + 1]],
          phase_info = phase_parser(r[[p]])
        )
      }, simplify = FALSE)
    }, simplify = FALSE)
    # unnest one level (now group:phase is flat now)
    design <- unlist(design, recursive = FALSE, use.names = FALSE)
    # That's the easy part
    # The hard part is to create the mapping for the experiment
    map <- .get_mapping(design)

    # create design object
    design_obj <- methods::new("CalmDesign",
      design = design, mapping = map, raw_design = df
    )
    # augment design if required
    if (!is.null(model)) {
      design_obj <- augment(design_obj, model = model, ...)
    }
  }
  design_obj
}

.get_mapping <- function(design) {
  # get phase info
  pinfo <- lapply(design, "[[", "phase_info")
  # trial names
  tinfo <- lapply(pinfo, "[[", "trial_info")
  ginfo <- lapply(pinfo, "[[", "general_info")

  # gather at the period level
  mastert <- unlist(lapply(ginfo, "[[", "trial_names"))
  # VN: in case of null tinfo, information gets duplicated (but then removed)
  funcs <- unlist(lapply(tinfo, function(r) {
    lapply(r, "[[", "functionals")
  }), recursive = FALSE)
  nomis <- unlist(lapply(tinfo, function(r) {
    lapply(r, "[[", "nominals")
  }), recursive = FALSE)
  # reduce
  uniqs <- !duplicated(mastert)
  tnames <- mastert[uniqs]
  funcs <- stats::setNames(funcs[uniqs], tnames)
  nomis <- stats::setNames(nomis[uniqs], tnames)

  # get transition names (nested within trial)
  transitions <- lapply(funcs, function(t) {
    periods <- names(t)
    nperiods <- length(periods)
    if (nperiods > 1) {
      sapply(
        seq_len(nperiods - 1),
        function(n) {
          sprintf("%s>%s", periods[n], periods[n + 1])
        }
      )
    }
  })
  transitions <- transitions[!unlist(lapply(transitions, is.null))]

  # make at the trial level
  tfuncs <- lapply(funcs, function(t) {
    unique(unlist(t, use.names = FALSE))
  })
  tnomis <- lapply(nomis, function(t) {
    unique(unlist(t, use.names = FALSE))
  })

  # sort unique names
  uni_fun <- sort(unique(unlist(funcs)))
  uni_nom <- sort(unique(unlist(nomis)))

  # make stimulus mapping
  n2f <- unlist(lapply(ginfo, "[[", "nomi2func"))
  n2f <- n2f[!duplicated(names(n2f))]
  f2n <- unlist(lapply(ginfo, "[[", "func2nomi"))
  f2n <- f2n[!duplicated(names(f2n))]

  # make one-hot vectors
  # period based
  period_onehots <- lapply(funcs, function(t) {
    lapply(t, one_hot, stimnames = uni_fun)
  })
  # trial based
  trial_onehots <- lapply(funcs, function(t) {
    one_hot(unique(unlist(t)), stimnames = uni_fun)
  })

  list(
    trial_names = tnames,
    unique_functional_stimuli = uni_fun,
    unique_nominal_stimuli = uni_nom,
    period_functionals = funcs,
    trial_functionals = tfuncs,
    period_nominals = nomis,
    trial_nominals = tnomis,
    period_ohs = period_onehots,
    trial_ohs = trial_onehots,
    nomi2func = n2f,
    func2nomi = f2n,
    transitions = transitions
  )
}

# Makes a onehot representation of the stimulus vector, given all stimuli
one_hot <- function(s, stimnames) {
  return(stats::setNames(as.numeric(stimnames %in% s), stimnames))
}
