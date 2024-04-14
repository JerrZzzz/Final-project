
library(shiny)
library(ggplot2)


q2_model <-
  readRDS(file = here::here("models/Q2_model.rds"))


ui <- fluidPage(
  titlePanel("Dynamic Q1 vs Q2 Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      # Slider input for Q1 Time (sec)
      sliderInput("q1Time",
                  "Q1 Time (sec):",
                  min = min(qual2_11$q1sec),
                  max = max(qual2_11$q1sec),
                  value = mean(qual2_11$q1sec))
    ),
    
    mainPanel(
      plotOutput("plot"),
      # Display the expected Q2 Time based on the model
      textOutput("predictedQ2Time")
    )
  )
)

# Define server logic to draw the plot and calculate expected Q2 Time
server <- function(input, output) {
  
  output$plot <- renderPlot({
    # Subset to get the nearest Q1 time
    q1_time <- round(input$q1Time, digits = 2)
    subset_data <- qual2_11[abs(qual2_11$q1sec - q1_time) == min(abs(qual2_11$q1sec - q1_time)), ]
    
    ggplot(qual2_11, aes(x = q1sec, y = q2sec)) +
      geom_point() + 
      geom_smooth(method = "lm", color = "skyblue") +
      labs(title = paste("Q1 vs Q2 - Q1 Time:", q1_time, "sec"),
           x = "Q1 Time (sec)", y = "Q2 Time (sec)") +
      theme_minimal() +
      geom_point(data = subset_data, aes(x = q1sec, y = q2sec), color = "red", size = 4)
  })
  
  # Calculate the expected Q2 time based on linear model
  output$predictedQ2Time <- renderText({
    # Estimate linear model coefficients from the complete dataset
    model <- lm(q2sec ~ q1sec, data = qual2_11)
    q1_time <- input$q1Time
    predicted_q2_time <- predict(model, newdata = data.frame(q1sec = q1_time))
    
    paste("Predicted Q2 Time for Q1 Time", q1_time, "sec is:", round(predicted_q2_time, 2), "sec")
  })
}

# Run the application
shinyApp(ui = ui, server = server)