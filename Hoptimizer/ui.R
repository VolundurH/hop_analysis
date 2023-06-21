library(shiny)
library(shinyWidgets)


ui <- tagList(
    navbarPage(
      theme = shinythemes::shinytheme("cosmo"),
      title = md("Hoptimizer"),
      tabPanel("Countries",
               sidebarPanel(
                 selectInput("inputCountry", label="Select a country:",
                             choices = c(unique(hop_aromas$country)), multiple = TRUE),
                 h5("This is a summary of the data:"),
                 gt_output('summaryCountry')
               ),
               mainPanel(
                 fluidPage(
                   fluidRow(column(width = 5,
                                   gt_output('hop_table_countries')),
                            column(width = 6, offset = 1,
                                   plotOutput("hop_plot_countries"))))
               )
      ),
      tabPanel("Profiles",
               chooseSliderSkin(skin = 'Flat', color = 'forestgreen'),
               sidebarPanel(
                 selectInput("inputPurpose", label="Select a purpose:",
                             choices = c("Any", unique(hop_aromas$hop_purpose)), selected = 'Any'),
                 
                 h5("There are 182 hops with aroma information. See below for filtering."),
                 tags$div(
                   class = "panel panel-default",
                   tags$div(
                     class = "panel-heading",
                     tags$h4(class = "panel-title", "Aromas",
                             tags$a(href = "#collapseGroup1", "[show/hide]",
                                    class = "accordion-toggle",
                                    `data-toggle` = "collapse"))
                   ),
                   tags$div(
                     id = "collapseGroup1",
                     class = "panel-collapse collapse in",
                     tags$div(class = "panel-body",
                              # Input: Aroma profile sliders -----
                              h5("Select a range for the following profiles to filter them:"),
                              sliderInput("sliderCitrus", width = "50%", label="Citrus:",
                                          min=0, max=5, value=c(0,5), step = 1),
                              sliderInput("sliderTropicalfruit", width = "50%", label="Tropical Fruit:",
                                          value=c(0,5), min=0, max=5, step = 1),
                              sliderInput("sliderStonefruit", width = "50%", label="Stone Fruit:",
                                          value=c(0,5), min=0, max=5, step = 1),
                              sliderInput("sliderBerry", width = "50%", label="Berry:",
                                          value=c(0,5), min=0, max=5, step = 1),
                              sliderInput("sliderFloral", width = "50%", label="Floral:",
                                          value=c(0,5), min=0, max=5, step = 1),
                              sliderInput("sliderGrassy", width = "50%", label="Grassy:",
                                          value=c(0,5), min=0, max=5, step = 1),
                              sliderInput("sliderHerbal", width = "50%", label="Herbal:",
                                          value=c(0,5), min=0, max=5, step = 1),
                              sliderInput("sliderSpice", width = "50%", label="Spice:",
                                          value=c(0,5), min=0, max=5, step = 1),
                              sliderInput("sliderPine", width = "50%", label="Pine:",
                                          value=c(0,5), min=0, max=5, step = 1)
                              )
                   )
                 ),
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Table", 
                            fluidPage(gt_output('hop_table_check'),
                                      gt_output('hop_table_profiles'))
                   ),
                   tabPanel("Overview plots",
                            fluidPage(
                              h3("Aroma value distribution of selected hops"),
                              gt_output('hop_table_check2'),
                              plotOutput("profile_overview"),
                              h3("Breakdown of oil percentages"),
                              h4("Hop oil contents"),
                              h5("Myrcene: Resin and citrus"),
                              h5("Humulene: Wood and spices"),
                              h5("Caryophyllene: Wood, pepper, herbal"),
                              h5("Farnesene: Floral, green apple"),
                              plotOutput("hop_oil_overview_plot")
                              )
                   ),
                   tabPanel("Per hop aroma plots",
                            fluidPage(gt_output('hop_table_check3'),
                                      gt_output('hop_table_check_max'),
                                      plotOutput("radial_plots"))
                            ),
                   tabPanel("Per hop brew values plots",
                            fluidPage(sidebarPanel(
                              h5("This list is not affected by previous filtering."),
                                selectInput("inputHop_panel3", label="Select a hop:",
                                  choices = c("-Select hop-",
                                    hop_aromas |> pull(hop_name) ),
                                selected = '-Select hop-'),
                              selectInput("inputBrewOption", label="Select a brew property:",
                                          choices = c("-Select property-",
                                                      hop_brew_values |> pull(brew_value) |> unique()),
                                          selected = '-Select hop-')
                                # end of sidebar panel 3
                              ),
                            mainPanel(
                                  plotOutput("brew_property_ranks")
                                  # end of main panel 3
                                ))
                   ),
                 )
               )
              ),
    )
)

