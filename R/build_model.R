#' Build and return a model
#'
#' @param model_name A string with a specific model. Currently supports one of `c("HDI2020", "HD2022")`.


build_model <- function(model_name = NULL, env = parent.frame()){

  #the function proceeds in two steps: first, it figures the necessary arguments for the model
  #second, it figures out the body of the model (i.e., the learning and expression functions)

  #set arguments
  if (model_name %in% hd_mods){
    args = as.pairlist(alist(alphas = ,
                             experience = ,
                             mapping =))
  }
  #set body
  model_functions = get_model_functions(model_name)

  body = quote({
    #boilerplate bits that every model needs
    V = gen_ss_weights(mapping$unique_functional_stimuli)
    rs = vs = array(NA, dim = c(length(experience$tps), dim(V)),
               dimnames = list(NULL, rownames(V), rownames(V)))
    as = array(NA, dim = c(length(experience$tp), nrow(V)),
               dimnames = list(NULL, rownames(V)))
    combvs = chainvs = vector('list', length(experience$tp))
    fsnames = rownames(V) #get functional stimuli names
    nsnames = names(alphas) #get nominal stimuli names

    #call function that assigns model-specific variables
    sals_avg = with(data.frame(mapping$nomi_func_map, alpha = alphas), tapply(alphas, func, mean))
    test_stims = fsnames

    #training function
    for (t in 1:length(tps)){
      #get pre functional and nominal stimuli
      fprestims = trial_pre_func[[tps[t]]]
      nprestims = trial_pre_nomi[[tps[t]]]
      #get post nominal stimuli
      fpoststims = trial_post_func[[tps[t]]]
      npoststims = trial_post_nomi[[tps[t]]]

      #compute combV for all stimuli
      combV = .combV(V = V, pre_func = fprestims, post_func = test_stims, db_trial = t)

      #compute chainV for all stimuli
      #without similarity
      #chainV = .chainV(V = V, pre_func = teststim_func, post_func = test_stims, db_trial = t)
      #with similarity
      chainV = .chainVSim(as_nomi = sals,
                          as_avg = sals_avg,
                          V = V,
                          pre_nomi = nprestims,
                          pre_func = fprestims,
                          post_func = test_stims,
                          db_trial = t)

      #Now we calculate rs for all the stimuli involved in the design (snames)

      #First, we need to identify absent stimuli and calculate their "retrieved" saliency
      rsals = .getSals(V = V,
                       sals_nomi = sals,
                       pre_nomi = nprestims,
                       pre_func = fprestims,
                       fsnames = fsnames,
                       nfmap = nomi_func_map,
                       db_trial = t)

      #Distribute R
      r = .distR(rsals, combV, chainV, t)

      #learn if we need to
      if (!is_test[t]){
        #make one-hot vector of pre functional stimuli (for learning)
        oh_fstims = .makeOH(c(fprestims, fpoststims), fsnames)
        #get saliencies for learning
        lsals = stats::setNames(rep(0, length(fsnames)), fsnames)
        #this bit is really annoying, as the mapping and the trial stimuli can sometimes be in different order
        lsals[sapply(c(nprestims, npoststims), function(x) nomi_func_map$func[nomi_func_map$nomi == x])] = sals[c(nprestims, npoststims)]

        #Learn
        e = oh_fstims %*% V #expectation
        err = oh_fstims*lsals-e #error
        d = oh_fstims*lsals%*%err #delta
        diag(d) = 0
        V = V+d
      }

      #save data
      vs[t, , ] = V
      as[t, ] = rsals
      rs[t, , ] = r
      combvs[[t]] = combV
      chainvs[[t]] = chainV
    }

    #results function


  })

  eval(call("function", args, body), env)
}
