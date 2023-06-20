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
               chooseSliderSkin(color = 'forestgreen'),
               sidebarPanel(
                 selectInput("inputPurpose", label="Select a purpose:",
                             choices = c("Any", unique(hop_aromas$hop_purpose)), selected = 'Any'),
                 # Input: Aroma profile sliders -----
                 h5("There are 182 hops with aroma information. Select a range for the following profiles to filter them:"),
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
                 sliderInput("sliderSpice", width = "50%", label="Select a range for the Spice:",
                             value=c(0,5), min=0, max=5, step = 1),
                 sliderInput("sliderPine", width = "50%", label="Pine:",
                             value=c(0,5), min=0, max=5, step = 1)
               ),
               mainPanel(
                 gt_output('hop_table_profiles')
               )
              ),
      tabPanel("Brew Values",
        sidebarPanel(
          selectInput("inputHop_panel3", label="Select a hop:",
            choices = c("-Select hop-", 
              hop_aromas |> select(hop_name, Citrus) |> drop_na() |> pull(hop_name) ), 
          selected = '-Select hop-')
          
          
          # end of sidebar panel 3
        ),
        mainPanel(
          plotOutput("radial_plots"),
          h4("Hop oil contents"),
          h5("Myrcene: Resin and citrus"),
          h5("Humulene: Wood and spices"),
          h5("Caryophyllene: Wood, pepper, herbal"),
          h5("Farnesene: Floral, green apple"),
          plotOutput("total_oil_overview")
          # end of main panel 3
        )
        
        # end of tabPanel 3
      )
    )
)

