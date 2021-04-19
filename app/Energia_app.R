# Import libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(ggrepel)

#############################################
# Data
#############################################
## Energy Data
df <- read.csv("Data_energy_countries.csv", header = TRUE, stringsAsFactors = FALSE, row.names = NULL)
df_renew <- read.csv("Data_energy_electricity_world.csv", header = TRUE, stringsAsFactors = FALSE, row.names = NULL)

countries_all <-  unique(df$Entity)
countries_renew <- unique(df_renew$Entity)
############################################
# User interface
############################################

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(HTML("<title>Energia</title>")),
  navbarPage(
    #
    title = div(tags$a(href = "https://www.facebook.com/An%C3%A1lisis-y-visualizaci%C3%B3n-de-datos-100602148375744",
                       target = "_blank", img(src = "https://raw.githubusercontent.com/DataFeast71/DataFeast/08c51765aac95160b21e4e6652cf39a9d93b0eb1/docs/img/Logo_feast.svg",
                                              style = "margin-top: -15px;padding-right:0px;padding-bottom:0px", 
                                              height = 50))),
    tabPanel(
      title = "Energy: Radar Plot",
      # Input values
      sidebarPanel(
        HTML("<h3>Input parameters</h3>"),
        helpText("Create radar plots with information from renewable energy."),
        # Year
        sliderInput(inputId = "Year",
                    label = "Year",
                    min = min(df$Year),
                    max = max(df$Year), step = 1, animate = FALSE, sep = "",
                    value = 2019),
        # Countries
        selectInput("Country1",
                    label = "Country 1:",
                    choices = countries_all,
                    selected = "Mexico"
                    ),
        selectInput("Country2",
                    label = "Country 2:",
                    choices = countries_all,
                    selected = "Brazil"
                    ),
        selectInput("Country3",
                    label = "Country 3:",
                    choices = countries_all,
                    selected = "China"
                    ),
        selectInput("Country4",
                    label = "Country 4:",
                    choices = countries_all,
                    selected = "Spain"
                    ),
        
        actionButton("SubmitButton",
                     "Submit",
                     class = "btn btn-primary")
      ), # Side Bar
      mainPanel(
        tags$label(h3("Status/Output")),
        textOutput("content1"),
        tableOutput(outputId = "Table1"),
        plotlyOutput(outputId = "plotlyPlot")
      ) # Main panel Tab 1
    ), # Tab Panel 1
    tabPanel(
      title = "Energy: Line Plot",
      
      sidebarPanel(
        HTML("<h3>Input parameters</h3>"),
        helpText("Create line plot with the information from renewable energy along the time."),
        # Input values
        # Countries
        selectInput("Country_ren1",
                    label = "Country 1:",
                    choices = countries_renew,
                    selected = "Mexico"
        ),
        selectInput("Country_ren2",
                    label = "Country 2:",
                    choices = countries_renew,
                    selected = "Brazil"
        ),
        selectInput("Country_ren3",
                    label = "Country 3:",
                    choices = countries_renew,
                    selected = "China"
        ),
        selectInput("Country_ren4",
                    label = "Country 4:",
                    choices = countries_renew,
                    selected = "Spain"
        ),
        actionButton("SubmitButton2",
                     "Submit",
                     class = "btn btn-primary")
      ), # SideBar2
      mainPanel(
        tags$label(h3("Status/Output")),
        textOutput("content2"),
        tableOutput(outputId = "Table2"),
        plotlyOutput(outputId = "plotlyPlot2")
        # plotOutput(outputId = "ggcountries")
        
      )
    )
  )
)

###########################################
# SERVER
##########################################

server <-function(input, output, session){
  # Tab 1
  # Input Data
  Data_energy <- reactive({
    # Countries
    countries <- c(input$Country1,
                   input$Country2,
                   input$Country3,
                   input$Country4)
    #Energy
    df_energy <- df[which((df$Entity %in% countries) & (df$Year == input$Year)),c(-2)]
    df_energy
  }) 
  
  #Radar plot
  Radar_Plot <- function() {
    country1 <- as.numeric(Data_energy()[which(Data_energy()$Entity == input$Country1),-1])
    country1 <- ifelse(is.na(country1), 0, country1)
    
    country2 <- as.numeric(Data_energy()[which(Data_energy()$Entity == input$Country2),-1])
    country2 <- ifelse(is.na(country2), 0, country2)
    
    country3 <- as.numeric(Data_energy()[which(Data_energy()$Entity == input$Country3),-1])
    country3 <- ifelse(is.na(country3), 0, country3)
    
    country4 <- as.numeric(Data_energy()[which(Data_energy()$Entity == input$Country4),-1])
    country4 <- ifelse(is.na(country4), 0, country4)
    
    fig <- plot_ly(
      type = "scatterpolar",
      # mode = "lines+markers",
      fill = "toself"
    )
    fig <- fig %>% 
      add_trace(
        r = country1,
        theta = c("Biofuels", "HidroPower", "Solar", "Wind"),
        name = input$Country1
        # fillcolor='rgb(0,0,0)',opacity = 0.3,
        # marker = list(color='rgb(0,0,0)'),
        # line=list(color='rgb(255,255,255)',dash='dashed')
      ) %>% 
      add_trace(
        r = country2,
        theta = c("Biofuels", "HidroPower", "Solar", "Wind"),
        name = input$Country2, subplot = "polar2"
      ) %>% 
      add_trace(
        r = country3,
        theta = c("Biofuels", "HidroPower", "Solar", "Wind"),
        name = input$Country3, subplot = "polar3"
      ) %>% 
      add_trace(
        r = country4,
        theta = c("Biofuels", "HidroPower", "Solar", "Wind"),
        name = input$Country4, subplot = "polar4"
      )
    
    fig %>% 
      layout(polar = list(
        domain = list(x = c(0,0.46), y = c(0.56,1))),
        polar2 = list(domain = list(x = c(0,0.46), y = c(0,0.44))),
        polar3 = list(domain = list(x = c(0.54,1), y = c(0.56,1))),
        polar4 = list(domain = list(x = c(0.54,1), y = c(0,0.44)))
      )
  }
  
  
  # Status/Output Text Box
  output$content1 <- renderText({
    if(input$SubmitButton > 0) {
      isolate("Analisis Complete.")
      isolate("Showing energy produced from renewable sources in Terawatt-hours (TWh).")
    } else  {
      return("Server is ready to show the Radars plot")
    }
  })
  
  # Table
  output$Table1 <- renderTable({
    if(input$SubmitButton > 0) {
      isolate(Data_energy())
    }
  })
  
  # Plotly
  output$plotlyPlot <- renderPlotly({
    if(input$SubmitButton > 0) {
      isolate(Radar_Plot())
    }
  })
  
  # Tab 2
  Data_renew <- reactive({
    # Countries
    countries <- c(input$Country_ren1,
                   input$Country_ren2,
                   input$Country_ren3,
                   input$Country_ren4)
    #Energy
    df_selected <- df_renew %>% 
      mutate(Selected_Country = if_else(Entity %in% countries, "TRUE", "FALSE"),
             label = if_else(Year == max(Year) & Selected_Country == "TRUE", as.character(Entity), NA_character_))
    # df_energy <- df_renew[which(df_renew$Entity %in% countries),]
    df_selected
  })
  
  #Radar plot
  Line_Plot <- function() {
    plot1 <- ggplot() +
      geom_line(filter(Data_renew(), Selected_Country == "FALSE"), 
                mapping = aes(x = Year, 
                              y = RenewableEnergy, 
                              #group = Entity, 
                              color = Entity), 
                alpha = 0.1, show.legend = FALSE) +
      geom_line(filter(Data_renew(), Selected_Country != "FALSE"),
                mapping = aes(x = Year,
                              y = RenewableEnergy,
                              #group = Entity,
                              color = Entity),
                alpha = 1.0, show.legend = FALSE, size = 1.2) +
      scale_x_continuous(name = "Año", limits = c(1968,2020), breaks = seq(1968,2020,4)) +
      scale_y_continuous(name = "", limits = c(0, 100), breaks = seq(0,100, 10), labels = function(x) paste0(x, "%")) +
      # geom_label_repel(
      #   data = Data_renew() %>%  filter( Selected_Country != "FALSE" & Year == max(Year)),
      #   aes(x = Year, y = RenewableEnergy, label = label),
      #   na.rm = TRUE, size = 3, nudge_x = 1
      # ) +
      #Theme
      theme_classic() +
      theme(
        # Plot
        panel.background = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        # Grid
        panel.grid = element_blank(),
        # Axis X
        axis.line.x.bottom = element_line(size = 1, color = "black"),
        axis.text.x = element_text(size = 11, color = "black", angle = 90, hjust = 0.5, vjust = 0.5),
        # Axis Y
        axis.line.y.left = element_line(size = 1, color = "black"),
        axis.text.y = element_text(size = 11, color = "black"),
      )
    plot1
  }

  # Status/Output Text Box
  output$content2 <- renderText({
    if(input$SubmitButton2 > 0) {
      isolate("Analisis Complete.")
      isolate("Showing % of energy that is from reneweable source.")
    } else  {
      return("Server is ready to show the plots")
    }
  })
  
  # Table
  output$Table2 <- renderTable({
    if(input$SubmitButton2 > 0) {
      isolate(head(Data_renew()))
    }
  })
  
  #Plotly
  output$plotlyPlot2 <- renderPlotly({
    if(input$SubmitButton2 > 0) {
      isolate(ggplotly(Line_Plot()) %>% layout(showlegend = FALSE))
    }
  })
  # output$ggcountries <- renderPlot({
  #   if(input$SubmitButton2 > 0) {
  #     isolate(Line_Plot())
  #   }
  # })
}

##########################################
## Create Shiny app
##########################################
shinyApp(ui = ui, server = server)
