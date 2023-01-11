#' Make a tibble to fit a calmr model
#'
#' @param design A design tibble, as returned by `parse_design`
#' @param pars A data.frame containing parameters as returned by `get_model_params`
#' @param opts A list with options as returned by `get_exp_opts`
#'
#' @return A tibble with the arguments required to run the model. Each row represents a group in the experimental design.
#'
#' @seealso \code{\link{parse_design}}, \code{\link{get_model_params}}, \code{\link{get_exp_opts}}
#' @examples
#' df <- data.frame(Group = c('Group 1', 'Group 2'), P1 = c('10AB(US)', '10A(US)'), R1 = c(TRUE, TRUE))
#' des <- parse_design(df)
#' ps <- get_model_params(des, 0.2)
#' make_model_args(design = des, pars = ps, model = "HD2022", opts = get_exp_opts(iterations = 1))
#'
#' @export

make_model_args <- function(design, pars = NULL, model = NULL, opts = get_exp_opts()){
  #parse design if necessary
  design = parse_design(design)

  if (is.null(model)){
    .calmr_default("model_name")
  }else{
    .calmr_check("supported_model", given = model)
  }

  #Some early info
  snames = pars$stimulus

  #the only challenge here is to create a master list of trials (trials)
  #and sample the training for each group (tps)
  #create master lists of trials and trial_names
  tinfo = design %>% tidyr::unnest_wider(.data$trial_info) %>%
    dplyr::select(.data$trial_pre_functional,
                  .data$trial_post_functional,
                  .data$trial_pre_nominal,
                  .data$trial_post_nominal,
                  .data$trial_names)

  if (class(tinfo$trial_names) == "list"){
    master_trial_names = do.call('c', tinfo$trial_names)
  }else{
    master_trial_names = tinfo$trial_names
  }
  trial_pre_functional_list = do.call('c', tinfo$trial_pre_functional)
  trial_post_functional_list = do.call('c', tinfo$trial_post_functional)
  trial_pre_nominal_list = do.call('c', tinfo$trial_pre_nominal)
  trial_post_nominal_list = do.call('c', tinfo$trial_post_nominal)

  #reduce
  trial_pre_functional_list = trial_pre_functional_list[!duplicated(master_trial_names)]
  trial_post_functional_list = trial_post_functional_list[!duplicated(master_trial_names)]
  trial_pre_nominal_list = trial_pre_nominal_list[!duplicated(master_trial_names)]
  trial_post_nominal_list = trial_post_nominal_list[!duplicated(master_trial_names)]

  master_trial_names = master_trial_names[!duplicated(master_trial_names)]

  #make stimulus mapping
  map = design %>%
    #this bit saves us having to crunch data about stimuli that are not present in a group but are present in another
    tidyr::unnest_wider(.data$trial_info) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::mutate(unique_nominal_stimuli = list(unique(unlist(.data$unique_nominal_stimuli))),
                  unique_functional_stimuli = list(unique(unlist(.data$unique_functional_stimuli))),
                  nomi2func = list({
                    n2f = do.call('c', .data$nomi2func)
                    n2f = n2f[!duplicated(names(n2f))]}),
                  func2nomi = list({
                    f2n = do.call('c', .data$func2nomi)
                    f2n = f2n[!duplicated(names(f2n))]})) %>%
    dplyr::select(.data$group, .data$unique_functional_stimuli,
                  .data$unique_nominal_stimuli, .data$nomi2func, .data$func2nomi) %>%
    dplyr::distinct() %>%
    #add the extra information above
    dplyr::rowwise() %>%
    dplyr::mutate(trial_pre_func = list(trial_pre_functional_list),
                  trial_post_func = list(trial_post_functional_list),
                  trial_pre_nomi = list(trial_pre_nominal_list),
                  trial_post_nomi = list(trial_post_nominal_list),
                  trial_names = list(master_trial_names))

  #filter
  map = do.call("rbind", apply(map, 1, function(x){
    dat = x
    tibble::tibble(group = dat[[1]], mapping = list(dat[-1]))
  } ))

  #add stimulus parameters
  map_par = .add_parameters(map, pars, model)

  #we can sample now
  #Dom, if you are reading this, I apologize for this bit
  #It basically creates a tibble of iterations*groups*phases, with pointers for trials in the masterlist
  exptb = design %>%
    tidyr::unnest_wider(.data$trial_info) %>%
    tidyr::expand_grid(iteration = 1:opts$iterations) %>%
    dplyr::rowwise() %>%
    #the pointers are returned by the function .sample_trial
    dplyr::mutate(samp = list(.sample_trials(.data$trial_names, .data$is_test, .data$trial_repeats,
                                             .data$randomize, opts$miniblocks, master_trial_names))) %>%
    dplyr::select(-.data$is_test) %>% #unfortunate naming
    tidyr::unnest_wider(.data$samp) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(phaselab = list(rep(.data$phase, length(.data$tps))),
                  blocks = list(rep(.data$block_size, length(.data$tps)))) %>%
    #one last manipulation to concatenate phases into single rows
    dplyr::group_by(.data$iteration, .data$group) %>%
    dplyr::summarize(tp = list(unlist(.data$tps)),
                     is_test = list(unlist(.data$is_test)),
                     phase = list(unlist(.data$phaselab)),
                     block_size = list(unlist(.data$blocks)))

  #bundle into experiences
  experience = apply(exptb[c(-1)], 1, function(x) do.call("cbind.data.frame", x))

  #return the tibble
  tibble::tibble(model,
                 exptb[, c("iteration", "group")],
                 experience) %>%
    dplyr::left_join(map_par, by = "group")
}

.sample_trials <- function(names, test, repeats, randomize, miniblocks, masterlist){
  block_size = 1
  #do miniblocks if necessary
  if (length(repeats) > 1 & miniblocks){
    gcd = Reduce(.gcd, repeats)
    per_block = repeats/gcd
    block_size = sum(per_block)
    tps = c() #note the redefining
    tstps = c()
    for (b in 1:gcd){
      ts = unlist(sapply(seq_along(names), function(n) rep(which(masterlist %in% names[n]), per_block[n])))
      tsts = unlist(sapply(seq_along(names), function(n) rep(test[n], per_block[n])))
      #randomize if necessary
      if (randomize){
        ri = sample(length(ts))
        ts = ts[ri]
        tsts = tsts[ri]
      }
      tps = c(tps, ts)
      tstps = c(tstps, tsts)
    }
  }else{
    tps = unlist(sapply(seq_along(names), function(n) rep(which(masterlist %in% names[n]), repeats[n])))
    tstps = unlist(sapply(seq_along(names), function(n) rep(test[n], repeats[n])))
    #randomize if necessary
    if (randomize){
      ri = sample(length(tps))
      tps = tps[ri]
      tstps = tstps[ri]
    }
  }
  return(list(tps = tps, is_test = tstps, block_size = block_size))
}

#function to return the gcd
.gcd <-  function(x,y) {
  r <- x%%y;
  return(ifelse(r, .gcd(y, r), y))
}

.add_parameters <- function(map, pars, model){
  parnames = .get_model_parnames(model)
  pars = sapply(parnames, function(p) lapply(map$mapping, function(x) setNames(pars[[p]], pars$stimulus)[unlist(x$unique_nominal_stimuli)]), simplify = F)
  tibble::tibble(map, tibble::as_tibble(pars))
}



