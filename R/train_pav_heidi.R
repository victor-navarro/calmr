#' Trains a Pavlovian HeiDI
#' @param sals A named vector with stimulus saliencies.
#' @param w A named array of dimensions S,S; where S is the number of stimuli.
#' @param tps A vector of trial pointers for training, as a function of trials.
#' @param trial_pre_func A list of length T, with character vectors specifying the functional stimuli involved in the expectation part of each trial. T is the number of unique trials in the experiment.
#' @param trial_post_func As above, but for the correction part of the trial.
#' @param trial_pre_nomi A list of length T, with character vectors specifying the nominal stimuli involved in the expectation part of each trial.
#' @param trial_post_nomi As above, but for the correctino part of the trial.
#' @param nomi_func_map A data.frame with the mappings between nominal and functional stimuli
#' @param trial_names (optional) A character vector of length T with the names of the trials
#' @param phase (optional) A character vector of length T with the names of the phases
#' @param block_size (optional) A integer vector of length T specifying the block size per trial
#' @param is_test (optional) A logical vector specifying whether the trial should result in learning (update w). If an element is TRUE, no update occurs.
#' @return A list with
#' \itemize{
#' \item{ws, rs - Arrays of dimensions P,S,S; where P is the number of trials used to train the model and S is the number of stimuli involved in the experiment. Respectively, ws and rs contain the stimulus weights and the stimulus-specific responses.}
#' \item{combvs, chainvs - Lists of length P with the r-values, combination vs, and chain vs.}
#' \item{tps, trial_pre_func, trial_post_func, trial_pre_nomi, trial_post_nomi, nomi_func_map, trial_names, phase, block_size, is_test -  Carryover for further processing. See Arguments.}
#' }
#' @note The array w contains the associations for all stimuli involved in the experiment. Entry i,j specifies the associative strength between stimulus i to stimulus j. Entry j,i specifies the opposite direction.
#' @export
train_pav_heidi <- function(sals, w, tps,
                            trial_pre_func,
                            trial_post_func,
                            trial_pre_nomi,
                            trial_post_nomi,
                            nomi_func_map,
                            trial_names = NULL,
                            phase = NULL,
                            block_size = NULL,
                            is_test = rep(FALSE, length(tps))){
  ws = array(NA, dim = c(length(tps), dim(w)),
             dimnames = list(NULL, rownames(w), rownames(w)))
  rs = ws
  as = array(NA, dim = c(length(tps), nrow(w)),
             dimnames = list(NULL, rownames(w)))
  combvs = chainvs = vector('list', length(tps))
  fsnames = rownames(w) #get functional stimuli names
  nsnames = names(sals) #get nominal stimuli names
  sals_avg = with(data.frame(nomi_func_map, sals = sals), tapply(sals, func, mean))
  test_stims = fsnames

  for (t in 1:length(tps)){
    #get pre functional and nominal stimuli
    fprestims = trial_pre_func[[tps[t]]]
    nprestims = trial_pre_nomi[[tps[t]]]
    #get post nominal stimuli
    fpoststims = trial_post_func[[tps[t]]]
    npoststims = trial_post_nomi[[tps[t]]]

    #compute combV for all stimuli
    combV = .combV(w = w, pre_func = fprestims, post_func = test_stims, db_trial = t)

    #compute chainV for all stimuli
    #without similarity
    #chainV = .chainV(w = w, pre_func = teststim_func, post_func = test_stims, db_trial = t)
    #with similarity
    chainV = .chainVSim(as_nomi = sals,
                        as_avg = sals_avg,
                        w = w,
                        pre_nomi = nprestims,
                        pre_func = fprestims,
                        post_func = test_stims,
                        db_trial = t)

    #Now we calculate rs for all the stimuli involved in the design (snames)

    #First, we need to identify absent stimuli and calculate their "retrieved" saliency
    rsals = .getSals(w = w,
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
      v = oh_fstims %*% w #expectation
      e = oh_fstims*lsals-v #error
      d = oh_fstims*lsals%*%e #delta
      diag(d) = 0
      w = w+d
    }

    #save data
    ws[t, , ] = w
    as[t, ] = rsals
    rs[t, , ] = r
    combvs[[t]] = combV
    chainvs[[t]] = chainV
  }
  dat = list(ws = ws,
             rs = rs,
             combvs = combvs,
             chainvs = chainvs,
             as = as,
             tps = tps,
             trial_pre_func = trial_pre_func,
             trial_post_func = trial_post_func,
             trial_pre_nomi = trial_pre_nomi,
             trial_post_nomi = trial_post_nomi,
             trial_names = trial_names,
             is_test = is_test,
             nomi_func_map = nomi_func_map,
             phase = phase,
             block_size = block_size)
  return(dat)
}

#### Internal functions ####
#Calculation of combined V
.combV <- function(w, pre_func, post_func, db_trial = NA){
  #w is a weight matrix,
  #pre_func is a character vector of the stimuli being presented
  #post_func is a character vector of the stimuli being predicted
  #
  #returns a matrix of dimensions pre_func x post_func, with the combV values

  #intial implementation used invidual terms that were later summed
  #late implementation just returns

  #if (db_trial == 1) browser()
  #if (length(pre_func) > 1)
  mat = array(0, dim = c(1, length(post_func)), dimnames = list(paste0(pre_func, collapse = ''), post_func))
  for (po in post_func){
    mat[1, po] = sum(w[pre_func, po])+(sum(w[pre_func, po])*(sum(w[po, pre_func])))
  }
  return(mat)
}

#Calculation of chain V
.chainV <- function(w, pre_func, post_func, db_trial = NA){
  #w is a weight matrix,
  #pre_func is a character vector of the stimuli being presented
  #post_func is a character vector of the stimuli being predicted
  #
  #The trick here is to obtain the chainV from every pre_func stimulus to every post_func stimulus,
  #while hitting every absent stimulus along the way
  #
  #Returns a matrix of dimensions pre_func post_func with chainV values

  #Preallocate zeros
  mat = array(0, dim = c(length(pre_func), length(post_func)), dimnames = list(pre_func, post_func))

  #find the absent stimuli
  allstims = rownames(w)
  absent = setdiff(allstims, pre_func)

  if (length(absent)){
    #a for loop for readability
    for (po in post_func){
      for (pr in pre_func){
        total_sum = 0
        for (a in absent){
          total_sum = total_sum + w[pr, a]*.combV(w, a, po, db_trial)[, po]
        }
        mat[pr, po] = total_sum
      }
    }
  }
  return(mat)
}

#Calculation of chain V with Similarity
.chainVSim <- function(w, as_nomi, as_avg, pre_nomi, pre_func, post_func, db_trial = NA){
  #Same as above, but with similarity of retrieved and nominal alphas modulating the the chain
  #as: a vector of nominal saliencies

  #Preallocate zeros
  mat = array(0, dim = c(length(pre_func), length(post_func)), dimnames = list(pre_func, post_func))

  #get absent stimuli
  allstims = rownames(w)
  absent = setdiff(allstims, pre_func)

  #a for loop for readability
  if (length(absent)){
    #get retrieved alphas
    retrieved_as = .absentAlpha(w = w, pre_func = pre_func, db_trial = NA)
    #get the average of their nominal alphas (TEMPORARY)
    nomi_avg_as = as_avg[absent]
    for (po in post_func){
      for (pr in pre_func){
        total_sum = 0
        for (a in absent){
          total_sum = total_sum +
            .alphaSim(retrieved_as[a], nomi_avg_as[a])*w[pr, a]*.combV(w, a, po, db_trial)[, po]
        }
        mat[pr, po] = total_sum
      }
    }
  }
  #if (db_trial == 10) browser()
  return(mat)
}

#Distribution of R among stimuli
.distR <- function(sals, combv, chainv, db_trial = NA){
  #Distributes the associative strength among all stimuli (sals)
  #returns a matrix of dimensions length(sals) x ncols(combv)
  #if (nrow(chainv) > 1) browser()
  #if (db_trial > 10) browser()
  mat = (sals/sum(sals))%*%(combv+colSums(chainv))
  rownames(mat) = names(sals)
  return(mat)
}

.getSals <- function(sals_nomi, w, pre_nomi, pre_func, fsnames, nfmap, db_trial = NA){
  #gets the saliencies for a given trial
  #it performs two actions:
  #1. populates a vector of saliencies for functional stimuli
  #[this based on the saliency (sals) of the nominal stimuli on the trial (pre_nomi)]
  #2. calculates the saliency for absent stimuli, via the .absentAlpha function
  as = stats::setNames(rep(0, length(fsnames)), fsnames)
  #Annoying bit again, see main function
  as[sapply(pre_nomi, function(x) nfmap$func[nfmap$nomi == x])] = sals_nomi[pre_nomi]
  #now do absent stimuli
  absent = names(as[as==0])
  if (length(absent)){
    as[absent] = .absentAlpha(w = w, pre_func = pre_func, db_trial = t)
  }
  as
}

#Function to calculate the alpha of absent stimuli
.absentAlpha <- function(w, pre_func, db_trial = NA){
  #w is a weight matrix,
  #pre_func is a character vector of the stimuli being presented
  #
  #Returns a vector of alphas equal to the number of absent of stimuli
  allstims = rownames(w)
  absent = setdiff(allstims, pre_func)
  as = stats::setNames(rep(0, length(absent)), absent)
  for (ab in absent){
    total_sum = 0
    for (pr in pre_func){
      total_sum = total_sum + w[pr, ab] #the direct association
      #now do the indirect associations via other absent stimuli
      int = setdiff(setdiff(absent, ab), pr) #the other absent stimuli
      if (length(int)){
        total_sum = total_sum + sum(sapply(int, function(i) w[pr, i]*w[i, ab], USE.NAMES = F))
      }
    }
    as[ab] = abs(total_sum) #Note the absolute function; important to study it
  }
  as
}

#Returns the similarity between two (salience) values
.alphaSim <- function(i, j){
  (i/(i + abs(i-j))) * (j/(j+ abs(i-j)))
}


#Makes a onehot representation of the stimulus vector, given all stimuli
.makeOH <- function(s, stimnames){
  return(as.numeric(stimnames %in% s))
}

#### Unused ####
#Softmax function
.soft <- function(acts, temp = 10){
  acts = acts-max(acts)
  return(exp(acts*temp)/sum(exp(acts*temp)))
}




