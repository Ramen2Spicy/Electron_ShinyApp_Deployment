library(shiny)
library(shinyjs)
library(rsconnect)
library(shinyShortcut) 

b64 <- base64enc::dataURI(file="images.png", mime="image/jpeg")

# Define UI ----
ui <- fluidPage(
  
  useShinyjs(),
  
  br(),
  
  img(src = b64),
  
  br(),
  
  titlePanel("Communist Calculator"),
  
  br(),
  
  fluidRow(
    
    column(12, 
           helpText(HTML("Welcome comrade! Praise to our great leader, the motherland's brightest scientists have developed this tool that will help you redistribute the means of resources. <br>Use it to serve your country well and crush the enemies of the people! ")))),
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      numericInput(5,
                   inputId = "com", 
                   label = "Communist Amount $",
                   value = "0"),
      
      numericInput(inputId = "cap",
                   label = "Capitalist Amount $",
                   value = "0"),
      
      numericInput(inputId = "n_com",
                   label = "Number of Comrades",
                   value = "5"),
      
      numericInput(inputId = "n_cap",
                   label = "Number of Capitalists",
                   value = "1")
    ),
    
    
    fluidRow(
      
      column(3,
             br(),
             br(),
             actionButton("Submit", "Submit"),
             br(),
             br(),
             actionButton("Reset", "Reset"),
             br(),
             br(),
             actionButton("Close", "Close")
      )
    )
    ),
  mainPanel(textOutput("com_text"),
            textOutput("cap_text"))
  )



# Define server logic 
server <- function(input, output) {
  
  observe({
    com <<- input$com 
    cap <<- input$cap
    n_com <<- input$n_com
    n_cap <<- input$n_cap
  },
    )
  
  observeEvent(input$Submit, {
    
    tryCatch({
      
      if (n_com == 0 & n_cap == 0){
        error_message <- "There must be one communist or one capitalist!"
        stop()
      } else {
        com_pay <- round((com/n_com) + (cap/(n_com + n_cap)),2)
        cap_pay <- round((cap/(n_com + n_cap)),2)
        
      }
      
      showModal(modalDialog(
        title = "Results",
        paste0("Comrade: $", com_pay),
        paste0("Capitalist: $", cap_pay),
        easyClose = TRUE,
        footer = NULL,
        
      ))  
    },
      error = function(e){
      showNotification(error_message, '', type = "error")
        removeModal()
      }
  )
  })
  
  observeEvent(input$Reset, {
    reset("com")
    reset("cap")
    reset("n_com")
    reset("n_cap")
  })
  
  observeEvent(input$Close, {
    rm(list=ls()[! ls() %in% c()])
    stopApp()
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)


setwd("C:/Users/Tianyang Zhao/Desktop/Communist Calculator")
