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
                 # fileInput("file", "File input:"),
                 # textInput("txt", "Text input:", "general"),
                 # sliderInput("slider", "Slider input:", 1, 100, 30),
                 # tags$h5("Default actionButton:"),
                 # actionButton("action", "Search"),
                 # 
                 # tags$h5("actionButton with CSS class:"),
                 # actionButton("action2", "Action button", class = "btn-primary")
                 h5("This is a summary of the data:"),
                 gt_output('summaryCountry')
               ),
               mainPanel(
                 gt_output('hop_table_countries')
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
          
          
          # end of sidebar panel 3
        ),
        mainPanel(
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












# # Define UI for app that draws a histogram ----
# ui <- navbarPage(
#   theme = bslib::bs_theme(bootswatch = "united"),
#   
#   # App title ----
#   titlePanel("Hoptimizer"),
#   
#   # First tab
#   tabPanel("Countries",
#            # Sidebar layout with input and output definitions ----
#              sidebarLayout(
#              
#              # Sidebar panel for inputs ----
#              sidebarPanel(
#                
#                # Input: Choose a country -----
#                selectInput("inputCountry",
#                            label="Select a country:",
#                            choices = c(unique(hop_aromas$country)),
#                            multiple = TRUE),
#                
#                # Show table with all
#                tableOutput('summaryCountry'),
#              ),
#              
#              # Main panel for displaying outputs ----
#              mainPanel(
#                gt_output('hop_table_profiles')
#              )
#            )
#   ),
# 
#   
#   # Second tab
#   tabPanel("Profiles",
#            chooseSliderSkin(color = "forestgreen"),
#            # Sidebar layout with input and output definitions ----
#            sidebarLayout(
#              
#              # Sidebar panel for inputs ----
#              sidebarPanel(
#                
#                # Input: Choose purpose ----
#                selectInput("inputPurpose",
#                            label="Select a purpose:",
#                            choices = c("Any", unique(hop_aromas$hop_purpose)),
#                            selected = 'Any'),
#                
#                # Input: Aroma profile sliders -----
#                h5("Select a range for the following profiles:"),
#                
#                sliderInput("sliderCitrus", width = "50%",
#                            label="Citrus:",
#                            min=0, max=5, value=c(0,5), step = 1),
#                sliderInput("sliderTropicalfruit", width = "50%",
#                            label="Tropical Fruit:",
#                            value=c(0,5), min=0, max=5, step = 1),
#                sliderInput("sliderStonefruit", width = "50%",
#                            label="Stone Fruit:",
#                            value=c(0,5), min=0, max=5, step = 1),
#                sliderInput("sliderBerry", width = "50%",
#                            label="Berry:",
#                            value=c(0,5), min=0, max=5, step = 1),
#                sliderInput("sliderFloral", width = "50%",
#                            label="Floral:",
#                            value=c(0,5), min=0, max=5, step = 1),
#                sliderInput("sliderGrassy", width = "50%",
#                            label="Grassy:",
#                            value=c(0,5), min=0, max=5, step = 1),
#                sliderInput("sliderHerbal", width = "50%",
#                            label="Herbal:",
#                            value=c(0,5), min=0, max=5, step = 1),
#                sliderInput("sliderSpice", width = "50%",
#                            label="Select a range for the Spice:",
#                            value=c(0,5), min=0, max=5, step = 1),
#                sliderInput("sliderPine", width = "50%",
#                            label="Pine:",
#                            value=c(0,5), min=0, max=5, step = 1),
#              ),
#              
#              # Main panel for displaying outputs ----
#              mainPanel(
#                gt_output('hop_table_profiles')
#              )
#            )
#   )
# )