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
library(shinyscreenshot)

header <- shinydashboard::dashboardHeader(tags$li(class = "dropdown",
                                                  tags$style(".main-header .logo {height: 60px;}")),
                                          tags$li(a("Help", href = 'https://victornavarro.org/calmr/articles/calmr_app.html',
                                                    target = "_blank",
                                                    title = "Help"),
                                            style = "padding-top:5px; padding-right:10px;",
                                            class = "dropdown"))

anchor <- tags$a(href="https://victornavarro.org/calmr", target="_blank", style = 'color: white;',
                 tags$img(src="logo.png", height='54', width='46'),
                 'Calmr Simulator')
header$children[[2]]$children <- tags$div(anchor, class = 'name')


supported_models <- supported_models()

ui <- shinydashboard::dashboardPage(title = "Calmr Simulator",
                                    skin = "red",
                                    header,
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
                                                           shinydashboard::box(collapsible = TRUE,
                                                                               width = NULL,
                                                                               title = "Parameters",
                                                                               shiny::conditionalPanel("output.parsed", rhandsontable::rHandsontableOutput("parameter_tbl", width = "100%")),
                                                           ),
                                                           # shinydashboard::box(width = 12,
                                                           #                     title = "Import/Export",
                                                           #                     htmltools::div(
                                                           #                       shiny::fileInput("loaddesign", "Load Sim", multiple = FALSE, accept = c(".rds"), buttonLabel = "...", width = "79%"),
                                                           #                       htmltools::div(style = "margin-top: -15px"),
                                                           #                       shiny::downloadButton("savedesign", "Save Sim", icon = shiny::icon("save"), class = "btn-s"),
                                                           #                       shiny::downloadButton("exportresults", "Save Data", icon = shiny::icon("file-download"), class = "btn-s")
                                                           #                     )
                                                           # )
                                        ),
                                        shiny::column(width = 3,
                                                      shinydashboard::box(collapsible = TRUE,
                                                                          width = NULL,
                                                                          title = "Sim Preferences",
                                                                          shiny::selectInput(inputId = "model_selection", label = "Model", choices = supported_models, selected = "RW1972", multiple = F),
                                                                          shiny::sliderInput(inputId = 'iterations', label = 'Iterations', min = 1, max = 200, value = 1, ticks = FALSE),
                                                                          shiny::checkboxInput(inputId = "miniblocks", label = 'Randomize trials in miniblocks', value = T)
                                                      ),
                                                      shinydashboard::box(collapsible = TRUE,
                                                                          width = NULL,
                                                                          title = "Plot Preferences",
                                                                          shiny::checkboxInput(inputId = "common_scale", label = 'Plot in common scale', value = T),
                                                                          shiny::selectInput(inputId = "phase_selection", label = 'Phase selection', choices = NULL, multiple = TRUE),
                                                                          shiny::selectInput(inputId = "trial_type_selection", label = 'Trial type selection', choices = NULL, multiple = TRUE)
                                                      )
                                        ),
                                        shiny::column(width = 9,
                                                      shinydashboard::box(collapsible = TRUE,
                                                                          width = NULL,
                                                                          title = "Results",
                                                                          shiny::conditionalPanel("output.ran",
                                                                                                  shiny::selectInput(inputId = 'plot_selection', label = NULL, choices = NA, multiple = T),
                                                                                                  shiny::plotOutput("plot")
                                                                          )
                                                      ),
                                                      shinydashboard::box(collapsible = TRUE,
                                                                          width = NULL,
                                                                          title = "Association Graphs",
                                                                          shiny::conditionalPanel("output.ran",
                                                                                                  shiny::sliderInput(inputId = 'graph_trial', label = 'Trial',
                                                                                                                     min = 1, max = 1, value = 1, step = 1,
                                                                                                                     ticks = FALSE, width = "30%"),
                                                                                                  shiny::plotOutput("graph")
                                                                          )
                                                      )

                                        )
                                      )
                                    )
)
