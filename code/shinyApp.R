library(dplyr)
library(shiny)
library(rpart)

data <- read.csv("../data/BodyFat.csv")
data <- data[rowSums(data == 0) == 0, ]
data <- na.omit(data)
data <- data[-which.max(data$BODYFAT), ]

# Define the selected features
selected_vars <- c('ABDOMEN', 'CHEST', 'BODYFAT')

# Subset the data
data_selected <- data[, selected_vars]

# Split the data into a training and testing dataset
set.seed(42)
train_indices <- sample(1:nrow(data_selected), 0.8 * nrow(data_selected))
train_data <- data_selected[train_indices, ]
test_data <- data_selected[-train_indices, ]

# Decision Tree model
tree_model <- rpart(BODYFAT ~ ., data = train_data)

# Define the Shiny UI
ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML(".title { text-align: center; font-weight: bold; text-decoration: underline; font-family: 'Arial', sans-serif; color: blue;}"),
      HTML("body { background-color: #e6e6e6; }"),  # Set background color for the entire app
      HTML(".calculator-container { display: flex; flex-direction: column; align-items: center; justify-content: center; height: 80vh; }"),
      HTML(".contact-info { position: absolute; bottom: 0; text-align: center; width: 100%; background-color: #f5f5f5; padding: 10px; border-top: 1px solid #ccc; font-family: 'Arial', sans-serif; }"),
      HTML(".center-table { display: flex; justify-content: center; }")
    )
  ),
  titlePanel(HTML("<h1 class='title'>Body Fat Calculator</h1>")),
  
  HTML("<div style='text-align: center;' font-family: 'Arial';>
    <p>There are various methods to estimate body fat percentage like skinfold calipers, body circumference, or complex methods like 3-D body scan. In the below prototype, we use abdomen and chest circumferences to calculate the % bodyfat. It is important to note that aside from autopsy, all methods of estimating body fat can have error rates as low as 2-3% or as high as 10-15%, depending on the method used.</p>
  </div>"),
  
  div(class = "calculator-container",
      sidebarLayout(
        sidebarPanel(
          # Input fields for the selected features
          numericInput("abdomen", "Abdomen Circumference (in cm):", value = NULL),
          numericInput("chest", "Chest Circumference (in cm):", value = NULL),
          
          actionButton("predictButton", "Predict Body Fat"),
          br(),
          br(),
          # Container for the "Predicted Body Fat"
          wellPanel(
            verbatimTextOutput("predictionText")
          )
        ),
        
        mainPanel(
          p(h3("Body Fat Norms for Men (By the American Council on Exercise):")),
          tableOutput("bodyFatNorms")
        )
      )
  ),
  
  div(class = "contact-info",
      "Maintained by: Vaishnavi Borwankar | Email: borwankar.vaishnavi@gmail.com"
  )
)

# Define the server logic
server <- function(input, output) {
  observeEvent(input$predictButton, {
    req(input$abdomen, input$chest)  # Check for empty fields
    
    # Define the minimum threshold values
    min_abdomen <- 30
    min_chest <- 20
    
    # Check if any of the input values are below the minimum threshold
    if (input$abdomen < min_abdomen || input$chest < min_chest) {
      showModal(modalDialog(
        title = "Invalid Input",
        paste("Please enter valid values within the specified range. Minimum values are:",
              "Abdomen (>= 30), and Chest (>= 20)"),
        easyClose = TRUE
      ))
    } else {
      # Create a data frame with user inputs for making predictions
      user_inputs <- data.frame(
        ABDOMEN = input$abdomen,
        CHEST = input$chest,
        BODYFAT = NA
      )
      
      # Make predictions using the Decision Tree model
      predicted_body_fat <- predict(tree_model, newdata = user_inputs)
      
      output$predictionText <- renderText({
        paste("Predicted Body Fat:", round(predicted_body_fat, 2), "%")
      })
    }
  })
  
  output$bodyFatNorms <- renderTable({
    bodyFatNorms <- data.frame(
      Description = c("Essential Fat", "Athletes", "Fitness", "Acceptable", "Obesity"),
      Men = c("2-5%", "6-13%", "14-17%", "18-24%", ">25%")
    )
    colnames(bodyFatNorms) <- c("Description", "Men")
    bodyFatNorms
  }, rownames = FALSE)
  
  # Add CSS style to the table
  outputOptions(output, "bodyFatNorms", suspendWhenHidden = FALSE)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
