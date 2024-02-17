library(shiny)
library(shinydashboard)
library(shinyalert)
library(tidyr)
library(dplyr)
library(ggbeeswarm)
library(htmltools)
library(magrittr)
library(rhandsontable)
library(stringr)
library(tibble)
library(calmr)
source("support.R")

supported_models <- calmr::supported_models()

ui <- shinydashboard::dashboardPage(
  title = "Calmr Simulator",
  skin = "red",
  dashboardHeader(
    title = "Calmr Simulator"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Options", tabName = "options"),
      menuItem("Help", tabName = "help"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "home",
        fluidRow(
          shinydashboard::box(
            width = 12,
            title = "Design",
            htmltools::div(
              style = "float:left",
              shiny::actionButton(
                inputId = "grouprm",
                label = "Group-", class = "btn-s"
              ),
              shiny::actionButton(
                inputId = "groupadd",
                label = "Group+", class = "btn-s"
              ),
              shiny::actionButton(
                inputId = "phaserm",
                label = "Phase-", class = "btn-s"
              ),
              shiny::actionButton(
                inputId = "phaseadd",
                label = "Phase+", class = "btn-s"
              ),
              shiny::actionButton(
                inputId = "parse_design",
                label = "Parse Design", class = "btn-s"
              ),
              htmltools::div(
                style = "display:inline-block;",
                shiny::conditionalPanel(
                  "output.parsed",
                  shiny::actionButton(
                    inputId = "run_experiment",
                    label = "Run Experiment", class = "btn-s"
                  )
                )
              ),
              htmltools::div(
                style = "display:inline-block;",
                shiny::conditionalPanel(
                  "output.ran",
                  shiny::downloadButton("exportresults", "Save Data",
                    icon = shiny::icon("file-download"), class = "btn-s"
                  )
                )
              )
            ),
            htmltools::br(), htmltools::br(),
            rhandsontable::rHandsontableOutput("design_tbl", width = "100%"),
            htmltools::br()
          ),
          shinydashboard::box(
            collapsible = TRUE,
            width = 12,
            title = "Parameters",
            shiny::selectInput(
              inputId = "model_selection",
              label = "Model", choices = supported_models,
              selected = "RW1972", multiple = FALSE
            ),
            shiny::conditionalPanel(
              "output.parsed",
              h5("Stimulus-specific parameters")
            ),
            shiny::conditionalPanel(
              "output.parsed",
              rhandsontable::rHandsontableOutput(
                "stim_par_tbl",
                width = "100%"
              )
            ),
            shiny::conditionalPanel(
              "output.parsed && output.needs_globals",
              h5("Global parameters")
            ),
            shiny::conditionalPanel(
              "output.parsed && output.needs_globals",
              rhandsontable::rHandsontableOutput(
                "glob_par_tbl",
                width = "100%"
              )
            )
          ),
          shinydashboard::box(
            collapsible = TRUE,
            width = 12,
            title = "Results",
            shiny::conditionalPanel(
              "output.ran",
              shiny::selectInput(
                inputId = "plot_selection",
                label = NULL, choices = NA, multiple = T
              ),
              shiny::plotOutput("plot")
            )
          ),
          shinydashboard::box(
            collapsible = TRUE,
            width = 12,
            title = "Association Graphs",
            shiny::conditionalPanel(
              "output.ran",
              shiny::sliderInput(
                inputId = "graph_trial", label = "Trial",
                min = 1, max = 1, value = 1, step = 1,
                ticks = FALSE, width = "30%"
              ),
              shiny::plotOutput("graph")
            )
          )
        )
      ),
      # Options tab content
      tabItem(
        tabName = "options",
        fluidPage(
          shinydashboard::box(
            collapsible = TRUE,
            width = NULL,
            title = "Simulation Options",
            shiny::sliderInput(
              inputId = "iterations",
              label = "Iterations", min = 1,
              max = 200, value = 1, ticks = FALSE
            ),
            shiny::checkboxInput(
              inputId = "miniblocks",
              label = "Create trial blocks",
              value = TRUE
            )
          ),
          shinydashboard::box(
            collapsible = TRUE,
            width = NULL,
            title = "Plotting Options",
            shiny::checkboxInput(
              inputId = "common_scale",
              label = "Plot in common scale", value = TRUE
            )
          )
        )
      ),
      tabItem(
        tabName = "help",
        fluidPage(
          p("Follow the rabbit"),
          a("Link",
            href = "https://victornavarro.org/calmr/articles/calmr_app.html"
          )
        )
      ),
      tabItem(
        tabName = "about",
        fluidPage(
          HTML('<center><img src="logo.png" width="20%"></center>'),
          br(), br(),
          HTML('Canonical Associative Learning Models in R calmr
          is developed by <a href="https://victornavarro.org" target="_blank">
          Victor Navarro</a>.'),
          br(), br(),
          HTML('To get access to the source code behind the package
          (and this app), head over to the
          <a href="https://github.com/victor-navarro/calmr"
          target="_blank">GitHub repository</a>.'),
          br(), br(),
          HTML('To consult the package documentation and other articles of
          interest, head over to the
          <a href="https://victornavarro.org/calmr/"
          target="_blank">package site</a>.'),
          br(), br(),
          p("Thanks for using the simulator."),
          p("Victor")
        )
      )
    )
  )
)
