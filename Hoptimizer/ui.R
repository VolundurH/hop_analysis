library(shiny)
library(shinyWidgets)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  navbarPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  
  # App title ----
  titlePanel("Hoptimizer"),
  
  # First tab
  tabPanel("Countries",
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar panel for inputs ----
             sidebarPanel(
               
               # Input: Choose a country -----
               selectInput("inputCountry",
                           label="Select a country:",
                           choices = c(unique(hop_aromas$country)),
                           multiple = TRUE),
               
               # Show table with all
               gt_output('summaryCountry'),
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
               gt_output('hop_table_profiles')
             )
           )
  ),

  
  # Second tab
  tabPanel("Profiles",
           chooseSliderSkin(color = "forestgreen"),
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
               h5("Select a range for the following profiles:"),
               
               sliderInput("sliderCitrus", width = "50%",
                           label="Citrus:",
                           min=0, max=5, value=c(0,5), step = 1),
               sliderInput("sliderTropicalfruit", width = "50%",
                           label="Tropical Fruit:",
                           value=c(0,5), min=0, max=5, step = 1),
               sliderInput("sliderStonefruit", width = "50%",
                           label="Stone Fruit:",
                           value=c(0,5), min=0, max=5, step = 1),
               sliderInput("sliderBerry", width = "50%",
                           label="Berry:",
                           value=c(0,5), min=0, max=5, step = 1),
               sliderInput("sliderFloral", width = "50%",
                           label="Floral:",
                           value=c(0,5), min=0, max=5, step = 1),
               sliderInput("sliderGrassy", width = "50%",
                           label="Grassy:",
                           value=c(0,5), min=0, max=5, step = 1),
               sliderInput("sliderHerbal", width = "50%",
                           label="Herbal:",
                           value=c(0,5), min=0, max=5, step = 1),
               sliderInput("sliderSpice", width = "50%",
                           label="Select a range for the Spice:",
                           value=c(0,5), min=0, max=5, step = 1),
               sliderInput("sliderPine", width = "50%",
                           label="Pine:",
                           value=c(0,5), min=0, max=5, step = 1),
             ),
             
             # Main panel for displaying outputs ----
             mainPanel(
               gt_output('hop_table_profiles')
             )
           )
  )
  )
)