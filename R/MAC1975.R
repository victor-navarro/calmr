#' Train the RW1972 model
#'
#' @param alphas A named vector with stimulus saliences.
#' @param lambdas A named vector with stimulus asymptotes.
#' @param V (optional) A named matrix of dimensions S,S; where S is the number of stimuli.
#' @param experience A data.frame specifying trials as rows, as returned by `make_model_args`
#' @param mapping A named list specifying trial and stimulus mapping, as returned by `make_model_args`
#' @return A list with
#' \itemize{
#' \item{vs, Array of dimensions P,S,S; where P is the number of trials used to train the model and S is the number of stimuli involved in the experiment. Respectively, vs and rs contain the stimulus weights and the stimulus-specific responses.}
#' \item{alphas, lambdas - Named vectors as above.}
#' \item{combs, chains - Lists of length P with combined and chained activation values.}
#' \item{experience, mapping - As passed in arguments; carryover arguments further processing.}
#' }
#' @note
#' @export

MAC1975 <- function(alphas,
                   lambdas,
                   V = NULL,
                   experience,
                   mapping){

  mod = new("CalmrModel",
            model = "RW1972")

  #data initialization
  ntrials = length(experience$tp)
  if(is.null(V)){V = gen_ss_weights(mapping$unique_functional_stimuli)}

  vs = es = array(NA, dim = c(ntrials, dim(V)),
             dimnames = list(NULL, rownames(V), rownames(V)))

  fsnames = test_stims = rownames(V) #get functional stimuli names
  nsnames = names(alphas) #get nominal stimuli names

  for (t in 1:ntrials){
    #get pre functional and nominal stimuli
    fprestims = mapping$trial_pre_func[[experience$tp[t]]]
    nprestims = mapping$trial_pre_nomi[[experience$tp[t]]]
    #get post nominal stimuli
    fpoststims = mapping$trial_post_func[[experience$tp[t]]]
    npoststims = mapping$trial_post_nomi[[experience$tp[t]]]

    #make one-hot vector of functional stimuli (for learning)
    oh_fstims = .makeOH(c(fprestims, fpoststims), fsnames)
    oh_fprestims = .makeOH(fprestims, fsnames)
    oh_fpoststims = .makeOH(fpoststims, fsnames)

    #generate expectation matrix
    emat = apply(V, 2, function(x) x*oh_fstims)


    #generate expectation vector
    e = oh_fstims %*% V #expectation
    #e = oh_fprestims %*% V #expectation //under investigation



    # #Distribute R
    # r = .distR(ralphas, combV, chainV, t)

    #learn if we need to
    if (!experience$is_test[t]){
      #get alphas and lambdas for learning (nominal only)
      lalphas = llambdas = stats::setNames(rep(0, length(fsnames)), fsnames)
      lalphas[mapping$nomi2func[c(nprestims, npoststims)]] = alphas[c(nprestims, npoststims)]
      llambdas[mapping$nomi2func[c(nprestims, npoststims)]] = lambdas[c(nprestims, npoststims)]

      #Learn
      err = oh_fstims*llambdas-e #error
      d = oh_fprestims*lalphas%*%err #delta

      diag(d) = 0
      V = V+d
    }

    #save data
    vs[t, , ] = V
    es[t, , ] = emat
  }
  mod@parameters = list(alphas = alphas,
                        lambdas = lambdas)
  mod@model_results = list(vs = vs,
                            es = es)
  mod@experience = experience
  mod@mapping = mapping
  mod
}
