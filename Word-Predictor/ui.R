#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(
    navbarPage(title = strong("Data Science Specialization Coursera"),
        tabPanel(title = strong("Word Predictor"),
                 fluidPage(theme = shinytheme("cerulean"),
                           tags$head(
                               tags$style(
                                   HTML(
                                       '
        body {
          background-image: url("https://images.unsplash.com/photo-1528459801416-a9e53bbf4e17?q=80&w=2024&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"); /* Replace with the path to your image */
          background-size: cover;
          background-repeat: no-repeat;
        }
        '
                                   )
                               )
                           ),    
                     # Application title
                     titlePanel(("Enter Your Scentence📋")),
                     
                     # Sidebar with a slider input for number of bins
                     sidebarLayout(
                         sidebarPanel(
                             textAreaInput("input_text",label = "", placeholder = "Enter Text", value = "", width = "400px",height = "300px"),
                             actionButton("predictBtn", "Predict"),
                             hr(),
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                             style = "background-color: white;",
                             h4("The predicted word will be visible here:"),
                             div(strong("How to Use:")),
                             div("1. Enter a phrase or sentence in the provided text input box."),
                             div("2. Click the ",strong("Predict")," button to trigger the prediction process."),
                             div("3. The predicted words are displayed in the output area."),
                             br(),
                             verbatimTextOutput("predictionText"),
                             
                         )
                     )
                 )
        ),
        tabPanel("About",
                 
                 HTML("<h2><strong><em>Word Prediction Application</em></strong></h2>"),
                 div(("The Word Prediction App is a Shiny application that predicts the 
                      next word in a given phrase or sentence. It employs a predictive model, 
                      trained on a dataset, to provide real-time suggestions as users type. 
                      This app is a project for the Data Science Capstone Course in the Data 
                      Science Specialization by Johns Hopkins University on Coursera. 
                      The goal is to create and deploy the Shiny application on Rstudio's servers.")),
                 style = "background-color: white;",
                 br(),
                 div(strong("Note: "),"The background image adds a touch of aesthetics to the app, making the experience both functional and visually pleasing."),
                 "All code files can be found in my ",
                 a("Githup Repository", href = "https://github.com/A-Vdev/Coursera-Data-Science-Specialization-Capstone-Project")
                 )
        
        
    )
)


