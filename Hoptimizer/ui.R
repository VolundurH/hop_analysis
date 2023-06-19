library(shiny)
library(shinyWidgets)
library(gt)


# Define UI for app that draws a histogram ----
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  title = "Hoptimizer",   
  tabPanel(title = "Countries", 
           fluidPage(

             chooseSliderSkin("Flat", color = "forestgreen"),

             # App title ----
             titlePanel("Hops by country"),

             # Sidebar layout with input and output definitions ----
             sidebarLayout(

               # Sidebar panel for inputs ----
               sidebarPanel(

                 # Input: Choose a country -----
                 selectInput("inputCountry",
                             label="Select a country to start:",
                             choices = c(unique(hop_aromas$country)),
                             multiple = TRUE),
                 
                 # add table with all here
                 gt_output('all_hops')
               ),
               # Main panel for displaying outputs ----
               mainPanel(
                 gt_output('hop_table_country')
                 )
           ))),
  tabPanel(title = "Aroma profiles",
           fluidPage(
    chooseSliderSkin("Flat", color = "forestgreen"),
    
    # App title ----
    titlePanel("Hops by aroma profiles"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Choose purpose ----
        selectInput("inputPurpose",
                    label="Select a purpose:",
                    choices = c("Any", unique(hop_aromas$hop_purpose)),
                    selected = 'Any'),
        
        # Input: Aroma profile sliders -----
              h6("Select a range for the following profiles:"),
              sliderInput("sliderCitrus", width = '50%',
                          label= "Citrus:",
                          min=0, max=5, value=c(0,5), step = 1),
              sliderInput("sliderTropicalfruit",
                          label="Tropical Fruit:", width = '50%',
                          value=c(0,5), min=0, max=5, step = 1),
              sliderInput("sliderStonefruit",
                          label="Stone Fruit:", width = '50%',
                          value=c(0,5), min=0, max=5, step = 1),
              sliderInput("sliderBerry",
                          label="Berry:", width = '50%',
                          value=c(0,5), min=0, max=5, step = 1),
              sliderInput("sliderFloral",
                          label="Floral:", width = '50%',
                          value=c(0,5), min=0, max=5, step = 1),
              sliderInput("sliderGrassy",
                          label="Grassy:", width = '50%',
                          value=c(0,5), min=0, max=5, step = 1),
              sliderInput("sliderHerbal",
                          label="Herbal:", width = '50%',
                          value=c(0,5), min=0, max=5, step = 1),
              sliderInput("sliderSpice",
                          label="Spice:", width = '50%',
                          value=c(0,5), min=0, max=5, step = 1),
              sliderInput("sliderPine",
                          label="Pine:", width = '50%',
                          value=c(0,5), min=0, max=5, step = 1),
            ),
      # Main panel for displaying outputs ----
      mainPanel(
        gt_output('profile_table')
      )
    )))
)