library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(rio)
library(DT)
library(stargazer)
library(shinyWidgets)
library(shiny)


max_co2_per_capita$co2_per_capita

ui <- 
  fluidPage(
    theme = bslib::bs_theme(bootswatch = "cerulean"),
    tabsetPanel(
      tabPanel("Introduction", 
           fluidPage(
             
             titlePanel("Introductory"),
             hr(),
             p("Welcome to the Carbon Emission visualisation app! To use this app, "),
             p("Carbon dioxide (chemical formula CO2) is a chemical compound made up of molecules that each have one carbon atom covalently double bonded to two oxygen atoms. It is found in the gas state at room temperature."),
             p("In the air, carbon dioxide is transparent to visible light but absorbs infrared radiation, acting as a greenhouse gas. It is a trace gas in Earth's atmosphere at 417 ppm (about 0.04%) by volume, having risen from pre-industrial levels of 280 ppm.[9][10] Burning fossil fuels is the primary cause of these increased CO2 concentrations and also the primary cause of global warming and climate change.[11] Carbon dioxide is soluble in water and is found in groundwater, lakes, ice caps, and seawater. When carbon dioxide dissolves in water it forms carbonic acid (H2CO3), which causes ocean acidification as atmospheric CO2 levels increase."),
             p("To understand the county wise increase or decrease in carbon emission, we will analyze owid_co02_data.csv file."),
             p("The analyze the effect we will consider following variables from the data."),
             tags$ul(
               tags$li(tags$b("co2_per_capita")),
               tags$li(tags$b("cement_co2")),
               tags$li(tags$b("co2_growth_abs")),
               tags$li(tags$b("oil_co2"))
             ),
             tags$ul(tags$b("Average Values"),
                     tags$li("The average value of co2_per_capita across all the counties (in the year 2021) is",round(average_co2_per_capita_country,2)),
                     tags$li("The average value of cement_co2 across all the counties (in the year 2021) is",round(average_cement_co2_country,2)),
                     tags$li("The average value of co2_growth_abs across all the counties (in the year 2021) is",round(average_co2_growth_abs_country,2)),
                     tags$li("The average value of oil_co2 across all the counties (in the year 2021) is",round(average_oil_co2_country,2))
             ),
             tags$ul(tags$b("Highest/Lowest Values"),
                     tags$li("The higest value of co2_per_capita across all the counties is",round(max_co2_per_capita$co2_per_capita,2),"in the year",max_co2_per_capita$year,"for country : ",max_co2_per_capita$country),
                     tags$li("The lowest value of co2_per_capita across all the counties is",round(min_co2_per_capita$co2_per_capita,2),"in the year",min_co2_per_capita$year,"for country : ",min_co2_per_capita$country),
                     
                     tags$li("The higest value of cement_co2 across all the counties is",round(max_cement_co2$cement_co2,2),"in the year",max_cement_co2$year,"for country : ",max_cement_co2$country),
                     tags$li("The lowest value of cement_co2 across all the counties is",round(min_cement_co2$cement_co2,2),"in the year",min_cement_co2$year,"for country : ",min_cement_co2$country),
                     
                     tags$li("The higest value of co2_growth_abs across all the counties is",round(max_co2_growth_abs$co2_growth_abs,2),"in the year",max_co2_growth_abs$year,"for country : ",max_co2_growth_abs$country),
                     tags$li("The lowest value of co2_growth_abs across all the counties is",round(min_co2_growth_abs$co2_growth_abs,2),"in the year",min_co2_growth_abs$year,"for country : ",min_co2_growth_abs$country),
                     
                     tags$li("The higest value of oil_co2 across all the counties is",round(max_oil_co2$oil_co2,2),"in the year",max_oil_co2$year,"for country : ",max_oil_co2$country),
                     tags$li("The lowest value of oil_co2 across all the counties is",round(min_oil_co2$oil_co2,2),"in the year",min_oil_co2$year,"for country : ",min_oil_co2$country),
                     
             ),
             p(tags$b("Percentage change in co2_per_capita over last 30 years from 1991 to 2021.")),
             DT::dataTableOutput("pect_change_30_year_co2_per_capita"),
             p(tags$b("Percentage change in cement_co2 over last 30 years from 1991 to 2021.")),
             DT::dataTableOutput("pect_change_30_year_cement_co2"),
             p(tags$b("Percentage change in co2_growth_abs over last 30 years from 1991 to 2021.")),
             DT::dataTableOutput("pect_change_30_year_co2_growth_abs"),
             p(tags$b("Percentage change in oil_co2 over last 30 years from 1991 to 2021.")),
             DT::dataTableOutput("pect_change_30_year_oil_co2")
           )
      ),
      tabPanel("Interactive Visualization",
               fluidPage(
                 
                 titlePanel("Interactive Visualization"),
                 sidebarLayout(
                   
                   sidebarPanel(
                     # selector for Country
                     h3("Bar graph of county wise variable analysis"),
                     selectInput(
                       inputId = "select_year",
                       label = "Select Year",
                       choices = unique(data1$year),
                       selected = '1991',
                       multiple = FALSE
                     ),
                     selectInput(
                       inputId = "select_variable",
                       label = "Select Variable",
                       choices = c("co2_per_capita", "cement_co2","co2_growth_abs","oil_co2"),
                       selected = "co2_per_capita",
                       multiple = FALSE
                     ),
                     br(),
                     br(),
                     br(),
                     h3("Line graph of Year wise variable analysis"),
                     selectInput(
                       inputId = "select_country",
                       label = "Select country",
                       choices =unique(data1$country),
                       selected = "Malawi",
                       multiple = FALSE
                     ),
                     selectInput(
                       inputId = "select_variable1",
                       label = "Select Variable",
                       choices = c("co2_per_capita", "cement_co2","co2_growth_abs","oil_co2"),
                       selected = "co2_per_capita",
                       multiple = FALSE
                     ),
                     dateRangeInput('dateRange',label = "Select year Range : ",format = "yyyy",start = '1991-01-01', end='2021-01-01',startview = "year",separator = " - "),
                     br(),
                     br(),
                     br(),
                     h3("Group bar graph of Year and Country wise variable analysis"),
                     textInput("select_country1", label = "Enter Country", value = "Enter Country Name : ex. Malawi"),
                     selectInput(
                       inputId = "select_year1",
                       label = "Select Year",
                       choices = unique(data1$year),
                       selected = '1991',
                       multiple = FALSE
                     ),
                   ),
                   mainPanel(
                     # epicurve goes here
                     plotlyOutput("plot_bar_country"),
                     p('Caption : The above bar graph shows the relationship between selected variables of CO2 and year for different countries.In the year 2021 the Co2 per Capita has incresased for all the countries.'),
                     plotlyOutput("plot_line_country"),
                     p('Caption : The line bar is controlled by the Country, variable for co2 & year range. From yaer 1991 to 2021, almost all the counties have started reocding their co2 varaibles.'),
                     plotlyOutput("plot_group_bar"),
                     p('Caption : Group bar chart for all the co2 related varaibles by country.')
                   )
                   
                 )
               )
      )
    )
  )



    
   
