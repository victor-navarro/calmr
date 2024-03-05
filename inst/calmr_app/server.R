source("support.R")
# welcome design
base_df <- data.frame(
  Group = c("Blocking", "Control"),
  P1 = c("10N>(US)", ""),
  R1 = FALSE,
  P2 = c("10NL>(US)/10#L", "10NL>(US)/10#L"),
  R2 = FALSE
)

# whether to print debugging messages
debug <- FALSE
# some options
base_plot_options <- list(common_scale = TRUE)
base_sim_options <- list(iterations = 1, miniblocks = TRUE)

shiny::shinyServer(function(input, output) { # nolint: cyclocomp_linter.
  #### Reactive values ####
  design_df <- shiny::reactiveVal(base_df)
  design <- shiny::reactiveVal()
  current_parameters <- shiny::reactiveVal()
  par_tables <- shiny::reactiveVal()
  plots <- shiny::reactiveVal()
  graphs <- shiny::reactiveVal()
  selected_plots <- shiny::reactiveVal()
  sim_options <- shiny::reactiveVal(base_sim_options)
  plot_options <- shiny::reactiveVal(base_plot_options)
  parsed <- shiny::reactiveVal(FALSE)
  needs_globalpars <- shiny::reactiveVal(FALSE)
  needs_trialpars <- shiny::reactiveVal(FALSE)
  needs_transpars <- shiny::reactiveVal(FALSE)
  ran <- shiny::reactiveVal(FALSE)
  raw_results <- shiny::reactiveVal()
  experiment <- shiny::reactiveVal()

  #### Input Logic ####
  shiny::observeEvent(input$groupadd, {
    if (debug) print("adding group")
    df <- rhandsontable::hot_to_r(input$design_tbl)
    df[nrow(df) + 1, ] <- df[nrow(df), ]
    df[nrow(df), 1] <- paste("Group", nrow(df))
    design_df(df)
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$grouprm, {
    if (debug) print("removing group")
    df <- rhandsontable::hot_to_r(input$design_tbl)
    if (nrow(df) > 1) {
      df <- df[1:(nrow(df) - 1), ]
      design_df(df)
      parsed(FALSE)
      ran(FALSE)
    }
  })

  shiny::observeEvent(input$phaseadd, {
    if (debug) print("adding phase")
    df <- rhandsontable::hot_to_r(input$design_tbl)
    cols <- ncol(df) - 1
    df[, paste0("P", cols / 2 + 1)] <- ""
    df[, paste0("R", cols / 2 + 1)] <- TRUE
    design_df(df)
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$phaserm, {
    if (debug) print("removing phase")
    df <- rhandsontable::hot_to_r(input$design_tbl)
    if (ncol(df) > 3) {
      df <- df[, 1:(ncol(df) - 2)]
      design_df(df)
      parsed(FALSE)
      ran(FALSE)
    }
  })

  shiny::observeEvent(input$model_selection, {
    if (debug) print("reset due to model selection")
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$parse_design, {
    # get old stimuli (for parameter retention)
    if (debug) print("parsing")
    # parse design_df
    design_df(rhandsontable::hot_to_r(input$design_tbl))
    design(calmr::parse_design(design_df(), model = input$model_selection))
    # get parameters
    # but, keep parameters if there are compatible parameters already
    if (debug) print("getting parameters")
    new_params <- calmr::get_parameters(
      design(),
      model = input$model_selection
    )
    current_parameters(new_params)
    # make parameter tables
    par_tables(make_par_tables(
      model = input$model_selection,
      current_parameters()
    ))
    # flip needs_globalpars if necessary
    needs_globalpars(check_globalpars(
      input$model_selection,
      current_parameters()
    ))
    # flip needs_trialpars if necessary
    needs_trialpars(check_trialpars(
      input$model_selection,
      current_parameters()
    ))
    needs_transpars(check_transpars(
      input$model_selection,
      current_parameters()
    ))
    if (debug) print("done with parameters")
    # flip parsed
    parsed(TRUE)
  })

  shiny::observeEvent(input$run_experiment, {
    if (debug) print("running experiment")
    tryCatch(
      {
        # use design_df and current_parameters to create
        # the experiment
        # expected experiment size
        n <- length(unique(design()@design$group)) * input$iterations
        # create a callback function for sampling progress
        args_call <- function() {
          shiny::incProgress(
            1 / n
          )
        }
        if (debug) {
          print("running with parameters ...")
          print(current_parameters())
        }
        shiny::withProgress(message = "Sampling trials...", value = 0, {
          experiment <- calmr::make_experiment(
            design(),
            model = input$model_selection,
            parameters = current_parameters(),
            options = sim_options(),
            .callback_fn = args_call
          )
        })
        # create a callback function for running progress
        run_call <- function() {
          shiny::incProgress(
            1 / n
          )
        }
        # run the experiment
        shiny::withProgress(message = "Simulating...", value = 0, {
          experiment <- calmr::run_experiment(experiment,
            aggregate = FALSE,
            .callback_fn = run_call
          )
        })
        # create a callback function for aggregation progress
        n_outputs <- length(c(
          sapply(
            unique(arguments(experiment)$model),
            calmr::model_outputs
          )
        ))
        agg_call <- function() {
          shiny::incProgress(
            1 / n_outputs
          )
        }
        if (debug) print("experiment ran")
        shiny::withProgress(message = "Aggregating results...", value = 0, {
          experiment <- calmr::aggregate(experiment, .callback_fn = agg_call)
        })
        if (debug) print("experiment aggregated")
        experiment(experiment)
        shiny::withProgress(message = "Making plots...", value = 0, {
          plots(calmr::plot(experiment))
          graphs(calmr::graph(experiment))
          shiny::setProgress(1)
        })
        if (debug) print("plots made")
        ran(TRUE)
      },
      error = function(x) {
        print(x)
        shinyalert::shinyalert(
          title = "Error!",
          text = "Something went wrong. Please check your design/parameters.",
          size = "s", closeOnEsc = TRUE,
          closeOnClickOutside = TRUE, html = FALSE,
          type = "error", showConfirmButton = TRUE, showCancelButton = FALSE,
          confirmButtonText = "OK", confirmButtonCol = "#AEDEF4"
        )
      }
    )
  })

  shiny::observeEvent(input$common_scale, {
    if (debug) print("changing common scale option")
    popts <- plot_options()
    popts$common_scale <- input$common_scale
    plot_options(popts)
  })

  shiny::observeEvent(input$iterations, {
    if (debug) print("changing iterations option")
    sopts <- sim_options()
    sopts$iterations <- input$iterations
    sim_options(sopts)
  })

  shiny::observeEvent(input$miniblocks, {
    if (debug) print("changing miniblocks option")
    sopts <- sim_options()
    sopts$miniblocks <- input$miniblocks
    sim_options(sopts)
  })

  #### Observers ####
  # populating the slider for graph_trial selection upon design change
  shiny::observeEvent(design(), {
    if (debug) print("populating phase/trials options due to design change")

    if (!is.null(design())) {
      ph_choices <- unique(design()@design$phase)
      t_choices <- design()@mapping$trial_names
    }

    shiny::updateSelectInput(
      inputId = "phase_selection",
      choices = ph_choices,
      selected = ph_choices
    )

    shiny::updateSelectInput(
      inputId = "trial_type_selection",
      choices = t_choices,
      selected = t_choices
    )
  })

  # populating the slider for graph_trial selection upon design change
  shiny::observeEvent(input$phase_selection, {
    if (debug) print("populating trials option due to design change")
    if (!is.null(design())) {
      d <- design()@design
      t_choices <- unique(unlist(lapply(
        d[d$phase %in% input$phase_selection, ]$trial_info,
        function(t) unique(t$trial_names)
      )))
      shiny::updateSelectInput(
        inputId = "trial_type_selection",
        choices = t_choices, selected = t_choices
      )
    }
  })

  # populating the slider for graph_trial selection
  shiny::observeEvent(experiment(), {
    if (!is.null(experiment())) {
      if (debug) print("populating graph slider after experiment run")
      last_trial <- max(
        results(experiment())[[1]]$trial
      )
      shiny::updateSliderInput(
        inputId = "graph_trial",
        value = last_trial,
        max = last_trial
      )
    }
  })

  # remaking the graphs on graph_trial change
  shiny::observeEvent(input$graph_trial, {
    if (debug) print("remaking graphs due to slider change")
    if (!is.null(experiment())) {
      graphs(calmr:::graph(experiment(), t = input$graph_trial))
    }
  })

  # change plot choices upon change in available plots
  shiny::observeEvent(plots(), {
    if (debug) print("populating plots choices due to plot changes")
    plot_names <- names(plots())
    # remember selection
    selection <- plot_names[1]
    if (!is.null(selected_plots())) {
      if (all(selected_plots() %in% plot_names)) {
        selection <- selected_plots()
      }
    }
    shiny::updateSelectInput(
      inputId = "plot_selection",
      choices = plot_names, selected = selection
    )
  })

  # Changes to the stimulus parameter table
  shiny::observeEvent(input$stim_par_tbl$changes$changes, {
    if (debug) print("changing stimulus parameters due to changes in table")
    df <- rhandsontable::hot_to_r(input$stim_par_tbl)
    pars <- current_parameters()
    newpars <- df_to_parlist(df, type = "stimulus")
    pars[names(newpars)] <- newpars
    current_parameters(pars)

    # Remake the parameter tables
    par_tables(make_par_tables(
      model = input$model_selection,
      current_parameters()
    ))
    ran(FALSE)
  })

  # Changes to the global parameter table
  shiny::observeEvent(input$glob_par_tbl$changes$changes, {
    if (debug) print("changing global parameters due to changes in table")
    df <- rhandsontable::hot_to_r(input$glob_par_tbl)
    pars <- current_parameters()
    newpars <- df_to_parlist(df, type = "global")
    pars[names(newpars)] <- newpars
    current_parameters(pars)
    # Remake the parameter tables
    par_tables(make_par_tables(
      model = input$model_selection,
      current_parameters()
    ))
    ran(FALSE)
  })

  # Changes to the trial parameter table
  shiny::observeEvent(input$trial_par_tbl$changes$changes, {
    if (debug) print("changing trial parameters due to changes in table")
    df <- rhandsontable::hot_to_r(input$trial_par_tbl)
    pars <- current_parameters()
    newpars <- df_to_parlist(df, type = "trial")
    pars[names(newpars)] <- newpars
    current_parameters(pars)
    # Remake the parameter tables
    par_tables(make_par_tables(
      model = input$model_selection,
      current_parameters()
    ))
    ran(FALSE)
  })

  # Changes to the transition parameter table
  shiny::observeEvent(input$trans_par_tbl$changes$changes, {
    if (debug) print("changing transition parameters due to changes in table")
    df <- rhandsontable::hot_to_r(input$trans_par_tbl)
    pars <- current_parameters()
    newpars <- df_to_parlist(df, type = "transition")
    pars[names(newpars)] <- newpars
    current_parameters(pars)
    # Remake the parameter tables
    par_tables(make_par_tables(
      model = input$model_selection,
      current_parameters()
    ))
    ran(FALSE)
  })

  # Changes to the design_df table
  shiny::observeEvent(input$design_tbl$changes$changes, {
    if (debug) print("changing design due to changes in table")
    design_df(rhandsontable::hot_to_r(input$design_tbl))
    parsed(FALSE)
    ran(FALSE)
  })

  # Changes the plots to be patched
  shiny::observeEvent(input$plot_selection, {
    if (debug) print("changing plot selection")
    selected_plots(input$plot_selection)
  })

  #### Outputs ####
  # Design table
  output$design_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering design table")
    if (!is.null(design_df())) {
      rhandsontable::rhandsontable(design_df(), rowHeaders = F) %>%
        rhandsontable::hot_col(col = seq(3, ncol(design_df()), 2), renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
              td.style.textAlign = 'center';
           }")
    }
  })

  # Stimulus parameters table
  output$stim_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering stimulus parameters table")
    if (!is.null(par_tables()$stimulus)) {
      rhandsontable::rhandsontable(par_tables()$stimulus,
        rowHeaders = FALSE
      ) %>%
        rhandsontable::hot_col("Stimulus", readOnly = TRUE)
    }
  })

  # Global parameters table
  output$glob_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering global parameters table")
    if (!is.null(par_tables()$global)) {
      rhandsontable::rhandsontable(par_tables()$global,
        rowHeaders = FALSE
      ) %>%
        rhandsontable::hot_col("Parameter", readOnly = TRUE)
    }
  })

  # To show global parameters table
  output$needs_globalpars <- shiny::reactive({
    return(needs_globalpars())
  })

  # Trial parameters table
  output$trial_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering trial parameters table")
    if (!is.null(par_tables()$trial)) {
      rhandsontable::rhandsontable(par_tables()$trial,
        rowHeaders = FALSE
      ) %>%
        rhandsontable::hot_col(c("Parameter", "Trial"), readOnly = TRUE)
    }
  })

  # To show trial parameters table
  output$needs_trialpars <- shiny::reactive({
    return(needs_trialpars())
  })

  # Transition parameters table
  output$trans_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering trans parameters table")
    if (!is.null(par_tables()$transition)) {
      rhandsontable::rhandsontable(par_tables()$transition,
        rowHeaders = FALSE
      ) %>%
        rhandsontable::hot_col(c("Parameter", "Trial", "Transition"),
          readOnly = TRUE
        )
    }
  })

  # To show transition parameters table
  output$needs_transpars <- shiny::reactive({
    return(needs_transpars())
  })


  # To export the state of the design_df and hide/show run button
  output$parsed <- shiny::reactive({
    return(parsed())
  })

  output$ran <- shiny::reactive({
    return(ran())
  })

  output$plot <- shiny::renderPlot({
    if (debug) print("rendering plot")
    if (!is.null(plots())) {
      calmr:::patch_plots(
        plots = plots(),
        selection = selected_plots(),
        plot_options = plot_options()
      )
    }
  })

  output$graph <- shiny::renderPlot({
    if (debug) print("rendering graph")
    if (!is.null(graphs())) {
      calmr:::patch_graphs(graphs())
    }
  })

  output$exportresults <- shiny::downloadHandler(
    filename = "calmr_results.xlsx",
    content = function(filename) {
      data <- list(
        design = design_df(),
        model = input$model_selection,
        stimulus_parameters = rhandsontable::hot_to_r(input$stim_par_tbl)
      )
      if (needs_globalpars()) {
        data <- c(
          data,
          list(global_parameters = rhandsontable::hot_to_r(input$glob_par_tbl))
        )
      }
      if (needs_trialpars()) {
        data <- c(
          data,
          list(trial_parameters = rhandsontable::hot_to_r(input$trial_par_tbl))
        )
      }
      if (needs_transpars()) {
        data <- c(
          data,
          list(trans_parameters = rhandsontable::hot_to_r(input$trans_par_tbl))
        )
      }
      data <- c(
        data,
        results(experiment())
      )
      openxlsx::write.xlsx(data, file = filename, overwrite = TRUE)
    }
  )

  shiny::outputOptions(output, "parsed", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "needs_globalpars", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "needs_trialpars", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "needs_transpars", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "ran", suspendWhenHidden = FALSE)
})
