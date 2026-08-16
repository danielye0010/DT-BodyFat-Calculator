library(dplyr)
library(shiny)
library(rpart)

data_path <- if (file.exists("data/BodyFat.csv")) "data/BodyFat.csv" else "../data/BodyFat.csv"
data <- read.csv(data_path)
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
      HTML("body { background-color: #e6e6e6; }"),
      HTML(".calculator-container { display: flex; flex-direction: column; align-items: center; justify-content: center; height: 80vh; }"),
      HTML(".contact-info { position: absolute; bottom: 0; text-align: center; width: 100%; background-color: #f5f5f5; padding: 10px; border-top: 1px solid #ccc; font-family: 'Arial', sans-serif; }"),
      HTML(".center-table { display: flex; justify-content: center; }")
    )
  ),
  titlePanel(HTML("<h1 class='title'>Body Fat Calculator</h1>")),
  
  HTML("<div style='text-align: center;' font-family: 'Arial';>
    <p>This statistical prototype estimates body-fat percentage from abdomen and chest circumferences using a decision-tree model trained on the project dataset. The estimate is intended as a demonstration of predictive modeling and interactive deployment rather than a clinical measurement.</p>
  </div>"),
  
  div(class = "calculator-container",
      sidebarLayout(
        sidebarPanel(
          numericInput("abdomen", "Abdomen Circumference (in cm):", value = NULL),
          numericInput("chest", "Chest Circumference (in cm):", value = NULL),
          
          actionButton("predictButton", "Predict Body Fat"),
          br(),
          br(),
          wellPanel(
            verbatimTextOutput("predictionText")
          )
        ),
        
        mainPanel(
          p(h3("Reference Body-Fat Categories for Men")),
          tableOutput("bodyFatNorms")
        )
      )
  ),
  
  div(class = "contact-info",
      "STAT 628 project implementation | Shiny app maintained by Vaishnavi Borwankar"
  )
)

# Define the server logic
server <- function(input, output) {
  observeEvent(input$predictButton, {
    req(input$abdomen, input$chest)
    
    min_abdomen <- 30
    min_chest <- 20
    
    if (input$abdomen < min_abdomen || input$chest < min_chest) {
      showModal(modalDialog(
        title = "Invalid Input",
        paste("Please enter valid values within the specified range. Minimum values are:",
              "Abdomen (>= 30), and Chest (>= 20)"),
        easyClose = TRUE
      ))
    } else {
      user_inputs <- data.frame(
        ABDOMEN = input$abdomen,
        CHEST = input$chest,
        BODYFAT = NA
      )
      
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
  
  outputOptions(output, "bodyFatNorms", suspendWhenHidden = FALSE)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
