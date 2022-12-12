#' Train the HD2022 model
#'
#' @param alphas A named vector with stimulus saliences.
#' @param V (optional) A named matrix of dimensions S,S; where S is the number of stimuli.
#' @param experience A data.frame specifying trials as rows, as returned by `make_model_args`
#' @param mapping A named list specifying trial and stimulus mapping, as returned by `make_model_args`
#' @return A list with
#' \itemize{
#' \item{vs, rs - Arrays of dimensions P,S,S; where P is the number of trials used to train the model and S is the number of stimuli involved in the experiment. Respectively, vs and rs contain the stimulus weights and the stimulus-specific responses.}
#' \item{as - Arrays of dimensions P,1,S; where P is the number of trials used to train the model and S is the number of stimuli involved in the experiment. Respectively, vs and rs contain the stimulus weights and the stimulus-specific responses.}
#' \item{combs, chains - Lists of length P with combined and chained activation values.}
#' \item{experience, mapping - As passed in arguments; carryover arguments further processing.}
#' }
#' @note The array V contains the associations for all stimuli involved in the experiment. Entry i,j specifies the associative strength between stimulus i to stimulus j. Entry j,i specifies the opposite direction.
#' @export

HD2022 <- function(alphas,
                   V = NULL,
                   experience,
                   mapping){

  mod = new("CalmrModel",
            model = "HD2022")

  #data initialization
  ntrials = length(experience$tp)
  if(is.null(V)){V = gen_ss_weights(mapping$unique_functional_stimuli)}
  vs = array(NA, dim = c(ntrials, dim(V)),
             dimnames = list(NULL, rownames(V), rownames(V)))
  rs = vs
  as = array(NA, dim = c(ntrials, nrow(V)),
             dimnames = list(NULL, rownames(V)))
  combvs = chainvs = vector('list', ntrials)
  fsnames = rownames(V) #get functional stimuli names
  nsnames = names(alphas) #get nominal stimuli names
  alphas_avg = tapply(alphas, mapping$nomi2func, mean) #average saliencies
  test_stims = fsnames

  for (t in 1:ntrials){
    #get pre functional and nominal stimuli
    fprestims = mapping$trial_pre_func[[experience$tp[t]]]
    nprestims = mapping$trial_pre_nomi[[experience$tp[t]]]
    #get post nominal stimuli
    fpoststims = mapping$trial_post_func[[experience$tp[t]]]
    npoststims = mapping$trial_post_nomi[[experience$tp[t]]]


    #compute combV for all stimuli
    combV = .combV(V = V, pre_func = fprestims, post_func = test_stims, db_trial = t)

    #compute chainV for all stimuli with a similarity rule
    chainV = .chainVSim(as_nomi = alphas,
                        as_avg = alphas_avg,
                        V = V,
                        pre_nomi = nprestims,
                        pre_func = fprestims,
                        post_func = test_stims,
                        db_trial = t)

    #identify absent stimuli and calculate their "retrieved" salience
    ralphas = .getalphass(V = V,
                         alphas_nomi = alphas,
                         pre_nomi = nprestims,
                         pre_func = fprestims,
                         fsnames = fsnames,
                         nomi2func = mapping$nomi2func,
                         db_trial = t)

    #Distribute R
    r = .distR(ralphas, combV, chainV, t)

    #learn if we need to
    if (!experience$is_test[t]){
      #make one-hot vector of pre functional stimuli (for learning)
      oh_fstims = .makeOH(c(fprestims, fpoststims), fsnames)
      oh_fprestims = .makeOH(fprestims, fsnames)

      #get saliencies for learning (nominal only)
      lalphas = stats::setNames(rep(0, length(fsnames)), fsnames)
      lalphas[mapping$nomi2func[c(nprestims, npoststims)]] = alphas[c(nprestims, npoststims)]

      #Learn
      e = oh_fstims %*% V #expectation
      #e = oh_fprestims %*% V #expectation //under investigation
      err = oh_fstims*lalphas-e #error
      d = oh_fstims*lalphas%*%err #delta
      diag(d) = 0
      V = V+d
    }

    #save data
    vs[t, , ] = V
    as[t, ] = ralphas
    rs[t, , ] = r
    combvs[[t]] = combV
    chainvs[[t]] = chainV
  }
  mod@parameters = list(alphas = alphas)
  mod@model_results = list(vs = vs,
                        rs = rs,
                        as = as,
                        acts = list(combvs = combvs, chainvs = chainvs))
  mod@experience = experience
  mod@mapping = mapping
  mod
}
