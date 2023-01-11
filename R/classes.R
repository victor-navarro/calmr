require(methods)

setClass("CalmrModel",
         slots = c(model = "character",
                   parameters = "list",
                   model_results = "list",
                   experience = "data.frame",
                   mapping = "list",
                   is_parsed = "logical"),
         prototype = list(is_parsed = F))

setClass("CalmrExperiment",
         slots = c(results = "data.frame",
                   parsed_results = "list",
                   is_parsed = "logical"),
         prototype = list(is_parsed = F))

setClass("CalmrComparison",
         slots = c(results = "data.frame",
                   models = "character",
                   layers = "character",
                   model_layer_names = "character"))

setClass("CalmrRSA",
         slots = c(corr_mat = "array",
                   distance_mats = "list",
                   trials = "character",
                   dist_method = "character",
                   corr_method = "character"))
setClass("CalmrRSATest",
         slots = c(RSA = "CalmrRSA",
                   sig_mat = "array",
                   lower_crit = "array",
                   upper_crit = "array",
                   n_samples = "numeric",
                   p = "numeric"))

setClass("CalmrFit",
         slots = c(nloglik = "numeric",
                   best_pars = "numeric",
                   model_pars = "numeric",
                   link_pars = "numeric",
                   data = "numeric",
                   model_function = "function",
                   link_function = "function",
                   ll_function = "function",
                   model_args = "tbl",
                   optimizer_options = "list",
                   extra_pars = "list"))
