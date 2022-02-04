#base_df <- data.frame(Group = "Group 1", P1 = "", R1 = TRUE)
base_df <- data.frame(Group = c("True", "Pseudo"),
                      P1 = c("10AB(US)/10AC", "5AB(US)/5AB/5AC(US)/5AC"),
                      R1 = c(TRUE, TRUE),
                      P2 = c("1A", "1A"),
                      R2 = c(TRUE, TRUE))

base_plot_options <- list(common_scale = TRUE)
base_sim_options <- list(iterations = 1)

# Define server logic required to draw a histogram
shiny::shinyServer(function(input, output) {

  #### Reactive values ####

  design_df = shiny::reactiveVal(base_df)
  parsed_design = shiny::reactiveVal()
  param_df = shiny::reactiveVal()
  plots = shiny::reactiveVal()
  selected_plots = shiny::reactiveVal()
  sim_options = shiny::reactiveVal(base_sim_options)
  plot_options = shiny::reactiveVal(base_plot_options)
  parsed = shiny::reactiveVal(FALSE)
  ran = shiny::reactiveVal(FALSE)
  raw_results = shiny::reactiveVal()
  parsed_results = shiny::reactiveVal()

  #### Input Logic ####
  shiny::observeEvent(input$loaddesign, {
    dsg = readRDS(input$loaddesign$datapath)
    design_df(dsg$design_df)
    parsed_design(dsg$parsed_design)
    param_df(dsg$param_df)
    plots(dsg$plots)
    selected_plots(dsg$selected_plots)
    sim_options(dsg$sim_options)

    #set some of the options/selections
    shiny::updateSelectInput(inputId = "plot_selection", selected = selected_plots(), choices = names(plots))
    shiny::updateSliderInput(inputId = 'iterations', value = sim_options()$iterations)
    plot_options(dsg$plot_options)
    shiny::updateCheckboxInput(inputId = "common_scale", value = plot_options()$common_scale)
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
  })

  shiny::observeEvent(input$grouprm, {
    df = rhandsontable::hot_to_r(input$design_tbl)
    if (nrow(df) > 1){
      df = df[1:(nrow(df)-1), ]
      design_df(df)
    }
  })

  shiny::observeEvent(input$phaseadd, {
    df = rhandsontable::hot_to_r(input$design_tbl)
    cols = ncol(df)-1
    df[, paste0('P', cols/2+1)] = ""
    df[, paste0('R', cols/2+1)] = T
    design_df(df)
  })

  shiny::observeEvent(input$phaserm, {
    df = rhandsontable::hot_to_r(input$design_tbl)
    if (ncol(df) > 3){
      df = df[, 1:(ncol(df)-2)]
      design_df(df)
    }
  })

  shiny::observeEvent(input$parse_design, {
    design_df(rhandsontable::hot_to_r(input$design_tbl))
    parsed_design(parse_design(design_df()))
    param_df(get_params(parsed_design(), input$defaultpar))
    parsed(TRUE)
  })

  shiny::observeEvent(input$runmodel, {
    tryCatch({
      #use design_df and param_df to create a tibble containing all necessary arguments for heidi
      heidi_df = heidi::make_heidi_args(parsed_design(), param_df(), sim_options())
      #run heidi, run!
      raw_results(heidi_df %>% dplyr::rowwise() %>% dplyr::mutate(mod_data = list(heidi::train_pav_heidi(stim_alphas, stim_cons, heidi::gen_ss_weights(stim_names), tps, trials, trialnames))))
      #parse results
      parsed_results(heidi::parse_heidi_results(raw_results()))
      #make plots
      plots(heidi::make_plots(parsed_results()))
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

  shiny::observeEvent(input$defaultpar, {
    if (!is.null(parsed_design()) & parsed()){
      param_df(heidi::get_params(parsed_design(), input$defaultpar))
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


  #### Outputs ####

  output$design_tbl <- rhandsontable::renderRHandsontable({
    if (!is.null(design_df())){
      rhandsontable::rhandsontable(design_df(), rowHeaders = F)
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
      heidi::patch_plots(plots(), selected_plots(), plot_options())
    }
  })

  output$savedesign <- shiny::downloadHandler(
    filename = "my_design.rds",
    content = function(fpath){
      saveRDS(list(design_df = design_df(),
                   parsed_design = parsed_design(),
                   param_df = param_df(),
                   plots = plots(),
                   selected_plots = selected_plots(),
                   sim_options = sim_options(),
                   plot_options = plot_options(),
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
