library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(DT)
library(plotly)
library(corrplot)
library(caret)
library(stargazer)
library(MASS)
library(caTools)
library(broom)

data <- read.csv("owid-co02-data.csv")

names(data)

library(zoo)
library(dplyr)
cols <- c("co2_per_capita", "cement_co2","co2_growth_abs","oil_co2")

data1 <- data %>% group_by(year) %>% mutate(co2_per_capita = ifelse(is.na(co2_per_capita), mean(co2_per_capita, na.rm = T), co2_per_capita))
data1 <- data1 %>% group_by(year) %>% mutate(cement_co2 = ifelse(is.na(cement_co2), mean(cement_co2, na.rm = T), cement_co2))
data1 <- data1 %>% group_by(year) %>% mutate(co2_growth_abs = ifelse(is.na(co2_growth_abs), mean(co2_growth_abs, na.rm = T), co2_growth_abs))
data1 <- data1 %>% group_by(year) %>% mutate(oil_co2 = ifelse(is.na(oil_co2), mean(oil_co2, na.rm = T), oil_co2))
data1 <- data1[,c("country","year","iso_code","co2_per_capita","cement_co2","co2_growth_abs","oil_co2")] 


average_co2_per_capita_country <- data.frame(data1 %>% filter(year == 2021) %>% summarize(Mean = mean(co2_per_capita))) %>% pull(Mean)
average_cement_co2_country <- data.frame(data1 %>% filter(year == 2021) %>% summarize(Mean = mean(cement_co2))) %>% pull(Mean)
average_co2_growth_abs_country <- data.frame(data1 %>% filter(year == 2021) %>% summarize(Mean = mean(co2_growth_abs))) %>% pull(Mean)
average_oil_co2_country <- data.frame(data1 %>% filter(year == 2021) %>% summarize(Mean = mean(oil_co2))) %>% pull(Mean)

temp <- data.frame(data1 %>% filter(!country %in% c('World','Asia')))

max_co2_per_capita <- data.frame(temp[which.max(temp$co2_per_capita),])
min_co2_per_capita <- data.frame(temp[which.min(temp$co2_per_capita),]) #mul


max_cement_co2 <-data.frame(temp[which.max(temp$cement_co2),])
min_cement_co2 <- data.frame(temp[which.min(temp$cement_co2),]) #mul

max_co2_growth_abs <- data.frame(temp[which.max(temp$co2_growth_abs),])
min_co2_growth_abs <- data.frame(temp[which.min(temp$co2_growth_abs),])


max_oil_co2 <- data.frame(temp[which.max(temp$oil_co2),])
min_oil_co2 <- data.frame(temp[which.min(temp$oil_co2),])

data2 <- data1 %>%
  filter(year %in% c(1991:2021))%>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    co2_per_capita_change = co2_per_capita - lag(co2_per_capita),
    cement_co2_change = cement_co2 - lag(cement_co2),
    co2_growth_abs_change = co2_growth_abs - lag(co2_growth_abs),
    oil_co2_change = oil_co2 - lag(oil_co2)
  )

pect_change_30_year_co2_per_capita <- data.frame(data2 %>% group_by(country) %>% summarise(change_co2_per_capita = ((max(co2_per_capita) - min(co2_per_capita))/min(co2_per_capita))*100))


pect_change_30_year_cement_co2 <- data.frame(data2 %>% group_by(country) %>% summarise(change_cement_co2= ((max(cement_co2) - min(cement_co2))/min(cement_co2))*100))

pect_change_30_year_co2_growth_abs <-data.frame(data2 %>% group_by(country) %>% summarise(change_co2_growth_abs = ((max(co2_growth_abs) - min(co2_growth_abs))/min(co2_growth_abs))*100))

pect_change_30_year_oil_co2 <- data.frame(data2 %>% group_by(country) %>% summarise(change_oil_co2 = ((max(oil_co2) - min(oil_co2))/min(oil_co2))*100))



pacman::p_load("tidyverse", "lubridate", "shiny")
library(plotly)


# define plotting function

plot_bar <- function(data1, year1 = 1991, col_name1 = 'co2_per_capita') {
  
  
  data_fun <- data.frame(data1 %>% filter(year == year1))
  fig <- plot_ly(
    data = data_fun,
    x = unique(data_fun$country),
    y = data_fun[[col_name1]],
    type = "bar"
  )
  fig <- fig %>% layout(title = stringr::str_glue("Bar Graph for - {year1} - and Variable Name - {col_name1} "),
                        xaxis = list(title = ""),
                        yaxis = list(title = ""))
  return(fig)
}



plot_line <- function(data1, country1 = 'Afghanistan', col_name1 = 'co2_per_capita',date1,date2) {
  
  
  dat1 <- format(as.Date(date1, format="%d/%m/%Y"),"%Y")
  dat2 <- format(as.Date(date2, format="%d/%m/%Y"),"%Y")
  
  data1 <- data1 %>% filter(year %in% (as.numeric(dat1):as.numeric(dat2)) )
  view(data1)
  data_fun <- data.frame(data1 %>% filter(country == country1))
  fig <- plot_ly(data_fun, x = data_fun$year, y = data_fun[[col_name1]], name = col_name1, mode = 'lines+markers') 
  fig <- fig %>% layout(title = stringr::str_glue("Line Graph for - {country1} for Variable {col_name1}."),
                        xaxis = list(title = ""),
                        yaxis = list(title = ""))
  return(fig)
}


plot_group_bar <- function(data1, country1 = 'Afghanistan',  year1 = 1991) {
  
  
  data_fun <- data.frame(data1 %>% filter(country == country1))
  data_fun <- data.frame(data_fun %>% filter(year == year1))
  
  fig <- plot_ly(data_fun, x = data_fun$country, y = data_fun$co2_per_capita, type = 'bar', name = 'co2_per_capita')
  fig <- fig %>% add_trace(y = data_fun$cement_co2, name = 'cement_co2')
  fig <- fig %>% add_trace(y = data_fun$co2_growth_abs, name = 'co2_growth_abs')
  fig <- fig %>% add_trace(y = data_fun$oil_co2, name = 'oil_co2')
  fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group') 
  fig <- fig %>% layout(title = stringr::str_glue("Group Bar Graph for - {country1} for year {year1}."),
                        xaxis = list(title = ""),
                        yaxis = list(title = ""))
  return(fig)
}


server <- function(input, output, session) {
  
  output$plot_bar_country <- renderPlotly(
    plot_bar(data1, year1 = input$select_year, col_name =  input$select_variable)
    )
  output$plot_line_country <- renderPlotly(
    plot_line(data1, country1 = input$select_country,col_name1 = input$select_variable1,date1 = input$dateRange[1], date2 = input$dateRange[2])
  )
  output$plot_group_bar <- renderPlotly(
    plot_group_bar(data1, country1 = input$select_country1, year1 = input$select_year1)
  )
  
  output$pect_change_30_year_co2_per_capita <- DT::renderDT(
    pect_change_30_year_co2_per_capita
  )
  output$pect_change_30_year_cement_co2 <- DT::renderDT(
    pect_change_30_year_cement_co2
  )
  output$pect_change_30_year_co2_growth_abs <- DT::renderDT(
    pect_change_30_year_co2_growth_abs
  )
  output$pect_change_30_year_oil_co2 <- DT::renderDT(
    pect_change_30_year_oil_co2
  )
}