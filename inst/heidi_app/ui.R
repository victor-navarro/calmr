library(shiny)
library(shinydashboard)
library(shinyalert)
library(tidyr)
library(dplyr)
library(ggbeeswarm)
library(htmltools)
library(magrittr)
library(patchwork)
library(rhandsontable)
library(stringr)
library(tibble)
library(heidi)

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "HeiDI Simulator"),
  shinydashboard::dashboardSidebar(disable = T),
  shinydashboard::dashboardBody(
    # Boxes need to be put in a row (or column)
    shiny::fluidPage(
      shiny::splitLayout(cellWidths = c("65%", "35%"),
                         shinydashboard::box(width = 12,
                                             title = "Design Table",
                                             htmltools::div(style="float:left",
                                                            shiny::actionButton(inputId = "grouprm", label = "Group-", class = 'btn-s'),
                                                            shiny::actionButton(inputId = "groupadd", label = "Group+", class = 'btn-s'),
                                                            shiny::actionButton(inputId = "phaserm", label = "Phase-", class = 'btn-s'),
                                                            shiny::actionButton(inputId = "phaseadd", label = "Phase+", class = 'btn-s'),
                                                            shiny::actionButton(inputId = "parse_design", label = "Parse Design", class = 'btn-s'),
                                                            htmltools::div(style="display:inline-block;",
                                                                           shiny::conditionalPanel("output.parsed",
                                                                                                   shiny::actionButton(inputId = 'runmodel', label = 'Run Model', class = 'btn-s')))

                                             ),
                                             htmltools::br(), htmltools::br(),
                                             rhandsontable::rHandsontableOutput("design_tbl", width = "100%")
                         ),
                         shinydashboard::box(width = 12,
                                             title = "Import/Export",
                                             htmltools::div(
                                               shiny::fileInput("loaddesign", "Load Sim", multiple = FALSE, accept = c(".rds"), buttonLabel = "...", width = "79%"),
                                               htmltools::div(style = "margin-top: -15px"),
                                               shiny::downloadButton("savedesign", "Save Sim", icon = shiny::icon("save"), class = "btn-s"),
                                               shiny::downloadButton("exportresults", "Save Data", icon = shiny::icon("file-download"), class = "btn-s")
                                             )
                         )
      ),
      shiny::column(width = 3,
                    shinydashboard::box(collapsible = TRUE,
                                        width = NULL,
                                        title = "Parameters",
                                        shiny::sliderInput(inputId = 'defaultpar', label = 'Default Alpha', min = 0, max = 1, value = .1, ticks = FALSE),
                                        htmltools::br(),
                                        shiny::conditionalPanel("output.parsed", rhandsontable::rHandsontableOutput("parameter_tbl", width = "100%"))
                    ),
                    shinydashboard::box(width = NULL,
                                        title = "Preferences",
                                        shiny::sliderInput(inputId = 'iterations', label = 'Sim Iterations', min = 1, max = 100, value = 1, ticks = FALSE),
                                        shiny::checkboxInput(inputId = "common_scale", label = 'Plot in Common Scale', value = T)
                    )
      ),
      shiny::column(width = 9,
                    shinydashboard::box(width = NULL,
                                        title = "Results",
                                        shiny::conditionalPanel("output.ran",
                                                                shiny::selectInput(inputId = 'plot_selection', label = NULL, choices = NA, multiple = T),
                                                                shiny::plotOutput("plot")
                                        )
                    )
      )
    )
  )
)
