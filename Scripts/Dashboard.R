# application for the final usar in energy consumption task

# libraries and data ----

if(require("pacman")=="FALSE"){
  install.packages("pacman")
}
p_load(shiny, shinydashboard, dplyr, ggplot2, plotly, lubridate)

# Loading data

data <- readr::read_rds("Datasets/CleanData.rds")


# Shiny application ----

ui <- dashboardPage(
  skin = "green",
  
  #Header
  dashboardHeader(
    title = "Electricty consumption task"
  ),
  # Sidebar
  dashboardSidebar(
    menuItem(text = "Total energy consumed", label = "totalEnergy")
  ),
  
  # Body
  dashboardBody(
    box(plotOutput("totalEnergyConsumed"))
  )
)

server <- function(input, output) {
  output$totalEnergyConsumed <- renderPlot(
    data %>% 
               group_by(date(DateTime)) %>% summarise(mean = mean(ActiveEnergy)) %>%
               ggplot(aes(`date(DateTime)`, mean)) + geom_line(color = "firebrick1") + geom_smooth(se = F) + 
               labs(title = "Total active energy consumed by day") + ylab("Watt/h") + xlab("Time") + theme_light()
    
  )
}

shinyApp(ui, server)
