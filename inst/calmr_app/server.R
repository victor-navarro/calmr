base_df <- data.frame(
  Group = c("Blocking", "Control"),
  P1 = c("10N>(US)", ""),
  R1 = FALSE,
  P2 = c("10NL>(US)/#10L", "10NL>(US)/#10L"),
  R2 = FALSE
)

base_plot_options <- list(common_scale = TRUE)
base_sim_options <- list(iterations = 1, miniblocks = TRUE)

# Define server logic required to draw a histogram
shiny::shinyServer(function(input, output) { # nolint: cyclocomp_linter.
  #### Reactive values ####

  design_df <- shiny::reactiveVal(base_df)
  parsed_design <- shiny::reactiveVal()
  current_parameters <- shiny::reactiveVal()
  plots <- shiny::reactiveVal()
  graphs <- shiny::reactiveVal()
  selected_plots <- shiny::reactiveVal()
  sim_options <- shiny::reactiveVal(base_sim_options)
  plot_options <- shiny::reactiveVal(base_plot_options)
  parsed <- shiny::reactiveVal(FALSE)
  ran <- shiny::reactiveVal(FALSE)
  raw_results <- shiny::reactiveVal()
  parsed_experiment <- shiny::reactiveVal()
  plot_filters <- shiny::reactiveVal()
  model_parameters <- shiny::reactiveVal()

  #### Input Logic ####
  shiny::observeEvent(input$groupadd, {
    df <- rhandsontable::hot_to_r(input$design_tbl)
    df[nrow(df) + 1, ] <- df[nrow(df), ]
    df[nrow(df), 1] <- paste("Group", nrow(df))
    design_df(df)
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$grouprm, {
    df <- rhandsontable::hot_to_r(input$design_tbl)
    if (nrow(df) > 1) {
      df <- df[1:(nrow(df) - 1), ]
      design_df(df)
      parsed(FALSE)
      ran(FALSE)
    }
  })

  shiny::observeEvent(input$phaseadd, {
    df <- rhandsontable::hot_to_r(input$design_tbl)
    cols <- ncol(df) - 1
    df[, paste0("P", cols / 2 + 1)] <- ""
    df[, paste0("R", cols / 2 + 1)] <- T
    design_df(df)
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$phaserm, {
    df <- rhandsontable::hot_to_r(input$design_tbl)
    if (ncol(df) > 3) {
      df <- df[, 1:(ncol(df) - 2)]
      design_df(df)
      parsed(FALSE)
      ran(FALSE)
    }
  })

  shiny::observeEvent(input$model_selection, {
    model_parameters(calmr:::.get_model_parnames(input$model_selection))
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$parse_design, {
    design_df(rhandsontable::hot_to_r(input$design_tbl))
    parsed_design(calmr::parse_design(design_df()))
    # get parameters
    # but, keep parameters if there are compatible parameters already
    new_params <- calmr::get_parameters(
      design = parsed_design(),
      model = input$model_selection
    )
    browser()
    old_params <- current_parameters()
    if (setequal(
      new_params$stimulus,
      old_params$stimulus
    ) & setequal(
      names(new_params),
      names(old_params)
    )) {
      current_parameters(old_params)
    } else {
      current_parameters(new_params)
    }
    parsed(TRUE)
  })

  shiny::observeEvent(input$runmodel, {
    tryCatch(
      {
        # use design_df and current_parameters to create a
        # tibble containing all necessary arguments for calmr
        calmr_args <- calmr::make_model_args(
          design = parsed_design(),
          pars = current_parameters(),
          model = input$model_selection,
          opts = sim_options()
        )
        iterations <- sim_options()$iterations
        # run calmr, run!
        res <- tibble::tibble()
        shiny::withProgress(message = "Simulating...", value = 0, {
          res <- calmr::run_model(calmr_args, parse = FALSE)
          shiny::incProgress(1)
        })
        raw_results(res)
        # parse results
        shiny::withProgress(message = "Parsing results...", value = 0, {
          parsed_experiment(calmr:::parse_experiment_results(raw_results()))
          shiny::setProgress(1)
        })
        shiny::withProgress(message = "Making plots...", value = 0, {
          plots(calmr:::make_plots(
            calmr:::filter_calmr_results(parsed_experiment(), plot_filters())
          ))
          graphs(calmr:::make_graphs(parsed_experiment()))
          shiny::setProgress(1)
        })
        ran(TRUE)
      },
      error = function(x) {
        print(x)
        shinyalert::shinyalert(
          title = "Error!",
          text = "Something went wrong. Please check your parameters.",
          size = "s", closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE,
          type = "error", showConfirmButton = TRUE, showCancelButton = FALSE,
          confirmButtonText = "OK", confirmButtonCol = "#AEDEF4"
        )
      }
    )
  })

  shiny::observeEvent(input$common_scale, {
    popts <- plot_options()
    popts$common_scale <- input$common_scale
    plot_options(popts)
  })

  shiny::observeEvent(input$iterations, {
    sopts <- sim_options()
    sopts$iterations <- input$iterations
    sim_options(sopts)
  })

  shiny::observeEvent(input$miniblocks, {
    sopts <- sim_options()
    sopts$miniblocks <- input$miniblocks
    sim_options(sopts)
  })

  # populating the phase selectInput and the options
  shiny::observeEvent(parsed_design(), {
    if (!is.null(parsed_design())) {
      ph_choices <- unique(parsed_design()$phase)
      t_choices <- parsed_design() %>%
        unnest_wider("trial_info") %>%
        select("trial_names") %>%
        unlist() %>%
        unique()
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

  # populating the trial_type selectInput
  shiny::observeEvent(input$phase_selection, {
    if (!is.null(parsed_design())) {
      t_choices <- parsed_design() %>%
        filter(phase %in% input$phase_selection) %>%
        unnest_wider("trial_info") %>%
        select("trial_names") %>%
        unlist() %>%
        unique()
      shiny::updateSelectInput(
        inputId = "trial_type_selection",
        choices = t_choices, selected = t_choices
      )
    }
  })

  # populating the slider for graph_trial selection
  shiny::observeEvent(parsed_experiment(), {
    last_trial <- max(parsed_experiment()@parsed_results[[1]]$trial)
    shiny::updateSliderInput(
      inputId = "graph_trial",
      value = last_trial,
      max = last_trial
    )
  })

  # remaking the graphs on graph_trial change
  shiny::observeEvent(input$graph_trial, {
    if (!is.null(parsed_experiment())) {
      graphs(calmr:::make_graphs(parsed_experiment(), t = input$graph_trial))
    }
  })

  #### Other reactives
  shiny::observeEvent(plots(), {
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

  shiny::observeEvent(input$parameter_tbl$changes$changes, {
    df <- rhandsontable::hot_to_r(input$parameter_tbl)
    names(df) <- stringr::str_to_lower(names(df))
    current_parameters(df)
    ran(FALSE)
  })

  shiny::observeEvent(input$design_tbl$changes$changes, {
    design_df(rhandsontable::hot_to_r(input$design_tbl))
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$plot_selection, {
    selected_plots(input$plot_selection)
  })

  shiny::observeEvent(input$phase_selection, {
    filters <- plot_filters()
    filters$phase <- input$phase_selection
    plot_filters(filters)
    shiny::withProgress(message = "Making plots...", value = 0, {
      plots(calmr:::make_plots(calmr:::filter_calmr_results(
        parsed_experiment(),
        plot_filters()
      )))
      shiny::setProgress(1)
    })
  })

  shiny::observeEvent(input$trial_type_selection, {
    filters <- plot_filters()
    filters$trial_type <- input$trial_type_selection
    plot_filters(filters)
    shiny::withProgress(message = "Making plots...", value = 0, {
      plots(calmr:::make_plots(calmr:::filter_calmr_results(
        parsed_experiment(),
        plot_filters()
      )))
      shiny::setProgress(1)
    })
  })

  #### Outputs ####

  output$design_tbl <- rhandsontable::renderRHandsontable({
    if (!is.null(design_df())) {
      rhandsontable::rhandsontable(design_df(), rowHeaders = F) %>%
        rhandsontable::hot_col(col = seq(3, ncol(design_df()), 2), renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
              td.style.textAlign = 'center';
           }")
    }
  })

  output$parameter_tbl <- rhandsontable::renderRHandsontable({
    if (!is.null(current_parameters())) {
      par_df <- current_parameters()
      names(par_df) <- stringr::str_to_title(names(par_df))
      rhandsontable::rhandsontable(par_df, rowHeaders = F) %>%
        rhandsontable::hot_col("Stimulus", readOnly = T)
    }
  })

  # To activate parameter sliders
  output$par_alpha <- shiny::reactive({
    return("alphas" %in% model_parameters())
  })

  # To export the state of the design and hide/show run button
  output$parsed <- shiny::reactive({
    return(parsed())
  })

  output$ran <- shiny::reactive({
    return(ran())
  })

  output$plot <- shiny::renderPlot({
    if (!is.null(plots())) {
      calmr:::patch_plots(
        plots = plots(),
        selection = selected_plots(),
        plot_options = plot_options()
      )
    }
  })

  output$graph <- shiny::renderPlot({
    if (!is.null(graphs())) {
      calmr:::patch_graphs(graphs())
    }
  })

  output$exportresults <- shiny::downloadHandler(
    filename = "my_simulation_results.xlsx",
    content = function(filename) {
      data <- c(
        list(
          design = design_df(),
          parameters = data.frame(model = input$model_selection, current_parameters())
        ),
        parsed_experiment()@parsed_results
      )
      openxlsx::write.xlsx(data, file = filename, overwrite = TRUE)
    }
  )

  shiny::outputOptions(output, "parsed", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "ran", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "par_alpha", suspendWhenHidden = FALSE)
})
