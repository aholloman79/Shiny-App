# app.R

# app.R

library(shiny)
library(plotly)
library(fields)

# Define UI
ui <- fluidPage(
  titlePanel("Warehouse Logistics Analysis 3D Plot"),
  sidebarLayout(
    sidebarPanel(
      p("This app visualizes a 3D surface plot of Shipment Volume, Handling Time, and Cost.")
    ),
    mainPanel(
      plotlyOutput("surfacePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$surfacePlot <- renderPlotly({
    # Assuming your dataset is already loaded in R as 'warehouse_logistics_analysis'
    x <- warehouse_logistics_analysis$Shipment_Volume
    y <- warehouse_logistics_analysis$Handling_Time
    z <- warehouse_logistics_analysis$Cost
    
    # Fit a thin plate spline model
    tps_model <- Tps(cbind(x, y), z)
    
    # Create a grid for the TPS model to predict on
    grid_size <- 40  # Increase for finer resolution
    x_grid <- seq(min(x), max(x), length.out = grid_size)
    y_grid <- seq(min(y), max(y), length.out = grid_size)
    grid <- expand.grid(x_grid, y_grid)
    z_pred <- matrix(predict(tps_model, grid), nrow = grid_size, ncol = grid_size)
    
    # Create a 3D plot using plotly
    plot_ly() %>%
      add_surface(x = ~x_grid, y = ~y_grid, z = ~z_pred, opacity = 0.7, showscale = FALSE) %>%
      add_markers(x = ~x, y = ~y, z = ~z, marker = list(size = 5, color = 'red')) %>%
      layout(scene = list(xaxis = list(title = 'Shipment Volume'),
                          yaxis = list(title = 'Handling Time'),
                          zaxis = list(title = 'Cost')))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
