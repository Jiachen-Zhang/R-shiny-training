library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("R-Shiny Assignment"),
  h5("by Jiachen"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1","Upload claims data (.xlsx)", accept = c(".xlsx")),
      sliderInput("tail","Select Tail factor", min = 1, max = 2, value = 1.1),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Imported Claims data",
          h4("Imported Claims Data"),
          tableOutput("contents")
        ),
        tabPanel("Claims Development Triangle",
          h4("Cumulative Paid Claims ($)"),
          tableOutput("triangle")
        ),
        tabPanel("Visualization", 
          h4("Plot of Cumulative Paid Claims Projection"),
          plotOutput("plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  claims.data <- reactive({
    req(input$file1)
    inFile <- input$file1
    read_excel(inFile$datapath, range = "Assignment!B3:D9")
  })
  
  claims.triangle <- reactive({
    df <- claims.data()
    number_loss_year <- n_distinct(df$`Loss Year`)
    number_dev_year <- max(df$`Development Year`)
    triangle <- matrix(0, nrow = number_loss_year, ncol = number_dev_year, byrow = TRUE)
    loss_years <- sort(unique(df$`Loss Year`))
    dev_years <- c(1:number_dev_year)
    rownames(triangle) <- loss_years
    colnames(triangle) <- dev_years
    
    for (row in 1:nrow(df)){
      LYear <- as.character(df[row, 1])
      DYear <- as.character(df[row, 2])
      triangle[LYear, DYear] <- as.double(df[row, 3])
    }
    
    cum.triangle <- t(apply(triangle, 1, cumsum))
    n <- number_dev_year
    dev_factors <- sapply(1:(n-1),
                          function(i){
                            sum(cum.triangle[c(1:(n-i)),i+1])/sum(cum.triangle[c(1:(n-i)),i])
                          })
    dev_factors <- c(dev_factors, input$tail)
    full.triangle <- cbind(cum.triangle, "4" = rep(0, number_loss_year))
    
    for(k in 1:n){
      full.triangle[(n-k+1):n, k+1] <- full.triangle[(n-k+1):n,k] * dev_factors[k]
    }
    round(full.triangle)
    colnames(full.triangle) <- paste("Dev Year", colnames(full.triangle), sep = " ")
    rownames(full.triangle) <- paste("Loss Year", rownames(full.triangle), sep = " ")
    full.triangle
  })
  
  claims.plot <- reactive({
    full.triangle <- claims.triangle()
    Development = rep(c(1:(number_dev_year+1)), times = number_loss_year)
    Loss = as.character(rep(loss_years, each = number_dev_year + 1))
    Amount = round(c(t(full.triangle)))
    
    df_plot = data.frame(Development, Loss, Amount)
    ggplot(df_plot, aes(x=Development, y=Amount, group=Loss, color=Loss)) +
      geom_point(size = 1.5) +
      geom_text(label=Amount, hjust=0.5, vjust=-1, 
                check_overlap = TRUE, size = 3, color = "black") +
      geom_line(linewidth = 0.5) +
      xlab("Development Year") +
      ylab("Amount ($)") +
      labs(title = "Cumulative Paid Claims", color = "Loss Year") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$contents <- renderTable({
    claims.data()
  }, digits = 0, align = "ccc")
  
  output$triangle <- renderTable({
    claims.triangle()
  }, rownames = TRUE, digits = 0, width = 800)
  
  output$plot <- renderPlot({
    claims.plot()
  })
}

shinyApp(ui, server)