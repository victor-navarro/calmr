# base_df = data.frame(Group = c("X+", "A+", "A"),
#                      P1 = c("10AX/10BX", "10AX/10BX", "10AX/10BX"),
#                      R1 = c(TRUE),
#                      P2 = c("1X>(US)", "1A>(US)", "1A"),
#                      R2 = c(TRUE),
#                      P3 = c("10B>(US)", "10B>(US)", "10B>(US)"),
#                      R3 = c(TRUE))

base_df = data.frame(Group = c("G1", "G2"),
                     P1 = c("10A>(US)/10#A", "10B>(US)"),
                     R1 = c(TRUE))

base_plot_options <- list(common_scale = TRUE)
base_sim_options <- list(iterations = 1, miniblocks = TRUE)

# Define server logic required to draw a histogram
shiny::shinyServer(function(input, output) {

  #### Reactive values ####

  design_df = shiny::reactiveVal(base_df)
  parsed_design = shiny::reactiveVal()
  param_df = shiny::reactiveVal()
  plots = shiny::reactiveVal()
  graphs = shiny::reactiveVal()
  selected_plots = shiny::reactiveVal()
  sim_options = shiny::reactiveVal(base_sim_options)
  plot_options = shiny::reactiveVal(base_plot_options)
  parsed = shiny::reactiveVal(FALSE)
  ran = shiny::reactiveVal(FALSE)
  raw_results = shiny::reactiveVal()
  parsed_results = shiny::reactiveVal()
  plot_filters = shiny::reactiveVal()

  #### Input Logic ####
  shiny::observeEvent(input$loaddesign, {
    dsg = readRDS(input$loaddesign$datapath)
    design_df(dsg$design_df)
    parsed_design(dsg$parsed_design)
    param_df(dsg$param_df)
    plots(dsg$plots)
    graphs(dsg$graphs)
    selected_plots(dsg$selected_plots)
    sim_options(dsg$sim_options)
    plot_filters(dsg$plot_filters)

    #set some of the options/selections
    shiny::updateSelectInput(inputId = "plot_selection", selected = selected_plots(), choices = names(plots))
    shiny::updateSliderInput(inputId = 'iterations', value = sim_options()$iterations)
    shiny::updateCheckboxInput(inputId = "miniblocks", value = sim_options()$miniblocks)
    plot_options(dsg$plot_options)
    shiny::updateCheckboxInput(inputId = "common_scale", value = plot_options()$common_scale)
    shiny::updateSelectInput(inputId = "phase_selection", selected = dsg$plot_filters$phase, choices = parsed_design()$phase)
    shiny::updateSelectInput(inputId = "trial_type_selection", selected = dsg$plot_filters$trial_type, choices = parsed_design() %>%
                               filter(phase %in% dsg$plot_filters$phase) %>%
                               unnest_wider(trial_info) %>%
                               select(trial_names) %>%
                               unlist() %>%
                               unique())

    #set some of the internal states
    parsed(dsg$parsed)
    ran(dsg$ran)
    raw_results(dsg$raw_results)
    parsed_results(dsg$parsed_results)

  })

  shiny::observeEvent(input$groupadd, {
    df = rhandsontable::hot_to_r(input$design_tbl)
    df[nrow(df)+1, ] = df[nrow(df), ]
    df[nrow(df), 1] = paste('Group', nrow(df))
    design_df(df)
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$grouprm, {
    df = rhandsontable::hot_to_r(input$design_tbl)
    if (nrow(df) > 1){
      df = df[1:(nrow(df)-1), ]
      design_df(df)
      parsed(FALSE)
      ran(FALSE)
    }
  })

  shiny::observeEvent(input$phaseadd, {
    df = rhandsontable::hot_to_r(input$design_tbl)
    cols = ncol(df)-1
    df[, paste0('P', cols/2+1)] = ""
    df[, paste0('R', cols/2+1)] = T
    design_df(df)
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$phaserm, {
    df = rhandsontable::hot_to_r(input$design_tbl)
    if (ncol(df) > 3){
      df = df[, 1:(ncol(df)-2)]
      design_df(df)
      parsed(FALSE)
      ran(FALSE)
    }
  })

  shiny::observeEvent(input$model_selction, {
    parsed(FALSE)
    ran(FALSE)
  })

  shiny::observeEvent(input$parse_design, {
    design_df(rhandsontable::hot_to_r(input$design_tbl))
    parsed_design(parse_design(design_df()))
    #get parameters
    #but, keep parameters if there are compatible parameters already
    new_params = get_params(parsed_design(), input$defaultpar, input$model_selection)
    old_params = param_df()
    if (setequal(new_params$Stimulus, old_params$Stimulus)){
      param_df(old_params)
    }else{
      param_df(new_params)
    }
    parsed(TRUE)
  })

  shiny::observeEvent(input$runmodel, {
    tryCatch({
      #use design_df and param_df to create a tibble containing all necessary arguments for heidi
      heidi_args = heidi::make_model_args(design = parsed_design(),
                                          pars = param_df(),
                                          model = input$model_selection,
                                          opts = sim_options())
      iterations = sim_options()$iterations
      #run heidi, run!
      res = tibble::tibble()
      shiny::withProgress(message = "Simulating...", value = 0, {
        for (i in 1:nrow(heidi_args)){
          res = rbind(res, heidi::run_model(heidi_args[i, ], model = input$model_selection, parse = FALSE))
          shiny::incProgress(1/iterations)
        }
      })
      raw_results(res)
      #parse results
      shiny::withProgress(message = "Parsing results...", value = 0, {
        parsed_results(heidi::parse_experiment_results(raw_results()))
        shiny::setProgress(1)
      })
      shiny::withProgress(message = "Making plots...", value = 0, {
        plots(heidi::make_plots(heidi::filter_heidi_results(parsed_results(), plot_filters())))
        graphs(heidi::make_graphs(parsed_results()))
        shiny::setProgress(1)
      })
      ran(TRUE)

    }, error = function(x){
      print(x)
      shinyalert::shinyalert(
        title = "Error!", text = "Something went wrong. Please check your parameters.", size = "s", closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = FALSE,
        type = "error", showConfirmButton = TRUE, showCancelButton = FALSE, confirmButtonText = "OK", confirmButtonCol = "#AEDEF4")
    })
  })

  shiny::observeEvent(input$common_scale,{
    popts = plot_options()
    popts$common_scale = input$common_scale
    plot_options(popts)
  })

  shiny::observeEvent(input$iterations, {
    sopts = sim_options()
    sopts$iterations = input$iterations
    sim_options(sopts)
  })

  shiny::observeEvent(input$miniblocks, {
    sopts = sim_options()
    sopts$miniblocks = input$miniblocks
    sim_options(sopts)
  })

  shiny::observeEvent(input$defaultpar, {
    if (!is.null(parsed_design()) & parsed()){
      param_df(heidi::get_params(parsed_design(), input$defaultpar))
    }
  })

  #populating the phase selectInput and the options
  shiny::observeEvent(parsed_design(), {
    if (!is.null(parsed_design())){
      ph_choices = unique(parsed_design()$phase)
      t_choices = parsed_design() %>%
        unnest_wider(trial_info) %>%
        select(trial_names) %>%
        unlist() %>%
        unique()
    }
    shiny::updateSelectInput(inputId = "phase_selection",
                             choices = ph_choices,
                             selected = ph_choices)

    shiny::updateSelectInput(inputId = "trial_type_selection",
                             choices = t_choices,
                             selected = t_choices)
  })

  #populating the trial_type selectInput
  shiny::observeEvent(input$phase_selection, {
    if (!is.null(parsed_design())){
      t_choices = parsed_design() %>%
        filter(phase %in% input$phase_selection) %>%
        unnest_wider(trial_info) %>%
        select(trial_names) %>%
        unlist() %>%
        unique()
      shiny::updateSelectInput(inputId = "trial_type_selection", choices = t_choices, selected = t_choices)
    }
  })

  #populating the slider for graph_trial selection
  shiny::observeEvent(parsed_results(), {
    last_trial = max(parsed_results()$vs$trial)
    shiny::updateSliderInput(inputId = "graph_trial",
                             value = last_trial,
                             max = last_trial)
  })

  #remaking the graphs on graph_trial change
  shiny::observeEvent(input$graph_trial, {
    if (!is.null(parsed_results())){
      graphs(heidi::make_graphs(parsed_results(), t = input$graph_trial))
    }
  })

  #### Other reactives
  shiny::observeEvent(plots(), {
    plot_names = names(plots())
    #remember selection, it gets annoying to reselect plots when playing with alpha
    selection = plot_names[1]
    if (!is.null(selected_plots())){
      if (all(selected_plots() %in% plot_names)){
        selection = selected_plots()
      }
    }
    shiny::updateSelectInput(inputId = 'plot_selection', choices = plot_names, selected = selection)
  })

  shiny::observeEvent(input$parameter_tbl$changes$changes, {
    param_df(rhandsontable::hot_to_r(input$parameter_tbl))
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
    filters = plot_filters()
    filters$phase = input$phase_selection
    plot_filters(filters)
    shiny::withProgress(message = "Making plots...", value = 0, {
      plots(heidi::make_plots(heidi::filter_heidi_results(parsed_results(), plot_filters())))
      shiny::setProgress(1)
    })
  })

  shiny::observeEvent(input$trial_type_selection, {
    filters = plot_filters()
    filters$trial_type = input$trial_type_selection
    plot_filters(filters)
    shiny::withProgress(message = "Making plots...", value = 0, {
      plots(heidi::make_plots(heidi::filter_heidi_results(parsed_results(), plot_filters())))
      shiny::setProgress(1)
    })
  })

  #### Outputs ####

  output$design_tbl <- rhandsontable::renderRHandsontable({
    if (!is.null(design_df())){
      rhandsontable::rhandsontable(design_df(), rowHeaders = F) %>%
        rhandsontable::hot_col(col = seq(3, ncol(design_df()), 2), renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
              td.style.textAlign = 'center';
           }")
    }
  })

  output$parameter_tbl <- rhandsontable::renderRHandsontable({
    if (!is.null(param_df())){
      rhandsontable::rhandsontable(param_df(), rowHeaders = F) %>%
        rhandsontable::hot_col("Stimulus", readOnly = T)
    }
  })

  #To export the state of the design and hide/show run button
  output$parsed <- shiny::reactive({
    return(parsed())
  })

  output$ran <- shiny::reactive({
    return(ran())
  })

  output$plot <- shiny::renderPlot({
    if (!is.null(plots())){
      heidi::patch_plots(plots = plots(), selection = selected_plots(), plot_options = plot_options())
    }
  })

  output$graph <- shiny::renderPlot({
    if (!is.null(graphs())){
      heidi::patch_graphs(graphs())
    }
  })

  output$savedesign <- shiny::downloadHandler(
    filename = "my_design.rds",
    content = function(fpath){
      saveRDS(list(design_df = design_df(),
                   parsed_design = parsed_design(),
                   param_df = param_df(),
                   plots = plots(),
                   graphs = graphs(),
                   selected_plots = selected_plots(),
                   sim_options = sim_options(),
                   plot_options = plot_options(),
                   plot_filters = plot_filters(),
                   parsed = parsed(),
                   ran = ran(),
                   raw_results = raw_results(),
                   parsed_results = parsed_results()), fpath)
    }
  )

  output$exportresults <- shiny::downloadHandler(
    filename = "my_simulation_results.xlsx",
    content = function(fpath){
      openxlsx::write.xlsx(parsed_results(), file = fpath, overwrite = TRUE)
    })

  shiny::outputOptions(output, "parsed", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "ran", suspendWhenHidden = FALSE)

})
