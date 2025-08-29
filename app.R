library(shiny)
library(shinyBS)
library(shinyjs)
library(ggplot2)
library(splines)
library(survival)
library(dplyr)
library(ggeffects)
library(flexsurv)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        color: darkblue;
      }
      .sidebar {
        background-color: #f0f8ff;
        padding: 10px;
        border-radius: 5px;
      }
      #summary_output {
        white-space: normal;
        font-size: 16px;
      }
      .center-title {
        text-align: center;
      }
      .subtitle {
        font-size: 14px;
        color: darkblue;
      }
      .title-row {
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .title-row h2 {
        margin-right: 10px;
      }
      .button-container {
        margin-top: 20px;
        display: flex;
        justify-content: space-between;
      }
      .share-icons {
        display: flex;
        justify-content: space-around;
        gap: 15px;
        margin-top: 20px;
      }
      .share-button {
        display: flex;
        align-items: center;
        justify-content: space-around;
        font-size: 14px;
        padding: 10px;
        border: none;
        width: 120px;
        cursor: pointer;
        color: white;
        border-radius: 5px;
        text-align: center;
      }
      .share-twitter { background-color: #1DA1F2; }
      .share-facebook { background-color: #3b5998; }
      .share-instagram { background-color: #E4405F; }
      .share-icon {
        margin-right: 8px;
        width: 20px;
        height: 20px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      h4("Current Physical Activity"),
      sliderInput("light_activity", "Light (mins/day)", min = 0, max = 600, value = 300, step = 15),
      sliderInput("moderate_activity", "Moderate (mins/day)", min = 0, max = 90, value = 30, step = 10),
      
      h4("Enter information for BMI", 
         actionButton("bmi_info", label = "?")), 
      bsPopover("bmi_info", "What is BMI?", 
                "BMI (Body Mass Index) is a value derived from the mass (weight) and height of an individual. It is a widely used measure to categorize individuals into weight categories: underweight, healthy weight, overweight, and obese.", 
                placement = "right", trigger = "click"), 
      
      numericInput("height", "Height", value = 170),
      radioButtons("height_unit", "Height Unit", choices = list("Centimeters" = "cm", "Feet/Inches" = "feet")),
      numericInput("weight", "Weight", value = 60),
      radioButtons("weight_unit", "Weight Unit", choices = list("Kilograms" = "kg", "Pounds" = "lbs")),
      
      verbatimTextOutput("bmi_output"),
      verbatimTextOutput("bmi_category"),
      
      div(class = "button-container",
          actionButton("share_button", "Share"),
          actionButton("summary_info", label = "Show Summary"),
          downloadButton("downloadPlot", "Download Plot")
      ),
      
      radioButtons("text_size", "Text Size", choices = list("Small" = "12px", "Medium" = "16px", "Large" = "20px"), selected = "12px")
    ),
    
    
    mainPanel(
      fluidRow(
        div(class = "title-row", 
            h2("Physical Activity and Survival Analysis"),
            actionButton("graph_info", label = "?")
        )
      ),
      div(class = "center-title subtitle", "Explore the impact of light and moderate physical activity on survival chances"),
      
      bsModal("graph_modal", "How it works", "graph_info", size = "large", 
              HTML("The point on the curve shows your current Light Physical Activity level. Moving along the curve reflects how changes in your activity level affect your survival chances, as indicated by the Y-axis. For more information, please visit: <a href='https://docs.google.com/document/d/1gAHHr6T-7c3OnEdKlWruTIl6e0xKY21YnMkQshgXia8/edit?tab=t.0' target='_blank'>this document</a>.")
      ),
      
      fluidRow(
        column(12, plotOutput("survivalPlot", height = "600px", width = "100%"))
      ),
      
      fluidRow(
        class = "sidebar",
        h4("Demographic Factor"),
        column(4, radioButtons("age_group", "Age group", choices = list("40-49" = "40-49", "50-59" = "50-59", "60-69" = "60-69"))),
        column(4, radioButtons("sex", "Sex", choices = list("Male" = "male", "Female" = "female"))),
        column(4, radioButtons("smoking", "Smoking", choices = list("Never smoker" = "never", "Ex smoker" = "ex", "Current smoker" = "current")))
      )
    )
  )
)


server <- function(input, output, session) {
  app_link <- "https://physical-activity-g-19-1.shinyapps.io/application/"
  
  observeEvent(input$share_button, {
    showModal(modalDialog(
      title = "Share this App to",
      easyClose = TRUE,
      footer = NULL,
      tags$div(
        class = "share-icons",
        
        # Twitter Share Button
        tags$button(
          class = "share-button share-twitter",
          onclick = sprintf("window.open('https://twitter.com/intent/tweet?text=Check out my physical activity analysis!&url=%s', '_blank')", app_link),
          tags$img(src = "icons/twitter.png", height = "20px", class = "share-icon"),
          "Twitter"
        ),
        
        # Facebook Share Button
        tags$button(
          class = "share-button share-facebook",
          onclick = sprintf("window.open('https://www.facebook.com/sharer/sharer.php?u=%s', '_blank')", app_link),
          tags$img(src = "icons/facebook.png", height = "20px", class = "share-icon"),
          "Facebook"
        ),
        
        # Instagram Share Button
        tags$button(
          class = "share-button share-instagram",
          onclick = sprintf("window.open('https://www.instagram.com?text=Check out my physical activity analysis!&url=%s', '_blank')", app_link),
          tags$img(src = "icons/instagram.png", class = "share-icon"),
          "Instagram"
        )
      )
    ))
  })
  
  
  # Load file and check if loaded successfully
  predictions <- tryCatch({
    readRDS("data.rds")
  }, error = function(e) {
    message("Error loading file: ", e)
    return(NULL)
  })
  
  if (is.null(predictions)) {
    stop("Data file could not be loaded. Please check the file path.")
  } else {
    message("Data file loaded successfully.")
  }
  
  # BMI Output
  bmi_value<- reactive({
    height <- input$height
    weight <- input$weight
    
    if (input$height_unit == "feet") {
      height <- height / 0.39370 / 100 
    } else {
      height <- height / 100  
    }
    
    if (input$weight_unit == "lbs") {
      weight <- weight * 0.453592  
    }
    
    bmi <- weight / (height^2)
  })
  
  bmi_category_value <- reactive({
    bmi <- bmi_value()
    ifelse(bmi < 18.5, "underweight",
           ifelse(bmi >= 18.5 & bmi < 24.9, "healthy",
                  ifelse(bmi >= 25 & bmi < 29.9, "overweight", "obese")))
  })
  
  output$bmi_output <- renderText({
    paste("Calculated BMI:", round(bmi_value(), 2))
  })
  
  output$bmi_category <- renderText({
    paste("BMI Category:", bmi_category_value())
  })
  
  summary_content <- reactive({
    req(input$age_group, input$sex, input$smoking)
   
    # Compose summary text
    summary_text <- paste(
      "For a", 
      "<strong>", input$age_group, "</strong>", 
      "year old", 
      "<strong>", input$sex, "</strong>", 
      "who is a", 
      "<strong>", input$smoking, "</strong>", 
      "<strong>", "smoker", "</strong>", 
      "with a BMI category of", 
      "<strong>",bmi_category_value(), "</strong>", 
      ", the current moderate physical activity level is", 
      "<strong>", input$moderate_activity, "</strong>", 
      "minutes per week, and light physical activity level is", 
      "<strong>", input$light_activity, "</strong>", 
      "(mins/day)"
    )
  })
  
  # Observe and dynamically add popover with summary content
  observeEvent(input$summary_info, {
    showModal(
      modalDialog(
        title = "Summary",
        HTML(summary_content()),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })
  
  
  observe({
    text_size_css <- paste0("body {font-size: ", input$text_size, ";}")
    
    # Remove existing style related to text size only, and add updated CSS for font size
    removeUI(selector = "style#dynamicTextSize", immediate = TRUE)  
    insertUI(selector = "head", where = "beforeEnd", 
             ui = tags$style(HTML(text_size_css), id = "dynamicTextSize")) 
  })
  
  # Plot rendering with only title (no subtitle in the plot)
  output$survivalPlot <- renderPlot({
    mvpa_input <- input$moderate_activity
    lpa_input <- input$light_activity
    age_input <- input$age_group
    sex_input <- input$sex
    smoke_input <- input$smoking
   
    if (is.null(predictions)) {
      return(NULL)
    }
    
    age <- c("40-49","50-59","60-69")
    sex <- c("male","female")
    smoker <- c("never","ex","current")
    bmi_cat <- c("healthy","underweight","overweight","obese")
    grid <- expand.grid(age, sex, smoker, bmi_cat)
    colnames(grid) <- c("age","sex","smoker","bmi")
    mvpa_list <- seq(0, 90, 10)
    
    index_1 <- which(grid$age == age_input & grid$sex == sex_input & grid$smoker == smoke_input & grid$bmi == bmi_category_value())
    index_2 <- which(mvpa_list == mvpa_input)
    
    if(length(index_1) == 0 || length(index_2) == 0) {
      message("Invalid indices for grid selection")
      return(NULL)
    }
    
    if (length(predictions) < index_1 || length(predictions[[index_1]]) < index_2) {
      message("Invalid predictions length")
      return(NULL)
    }
    
    p <- predictions[[index_1]][[index_2]][[1]]
    var <- predictions[[index_1]][[index_2]][[2]]
    
    if(is.null(p) || !("LPA" %in% colnames(p)) || !("estimate" %in% colnames(p))) {
      message("Invalid prediction data")
      return(NULL)
    }
    
    ref_index <- which(p$LPA == lpa_input)
    if(length(ref_index) == 0) ref_index <- 1
    
    p$ref <- p$estimate - p[ref_index,]$estimate
    p$ref.se <- do.call(rbind, lapply(seq(1, nrow(p)), function(x, var, ref_index) {
      sqrt(var[x, x] + var[ref_index, ref_index] - 2 * var[x, ref_index])
    }, var = var, ref_index = ref_index))
    
    # Plot with annotation of the intersection point coordinates
    ggplot(p, aes(x = LPA, y = ref)) + 
      geom_line(colour = "darkblue") + 
      geom_ribbon(aes(ymin = ref - 1.96 * ref.se, ymax = ref + 1.96 * ref.se), alpha = 0.3, fill = "skyblue") +
      annotate("point", x = lpa_input, y = p[ref_index,]$ref, colour = "darkblue", shape = 18, size = 2.5) +
      geom_vline(xintercept = lpa_input, linetype = "dashed", color = "blue") +
      theme_minimal() +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +  # Adjust the margins
      labs(x = "Light-Intensity Physical Activity (mins/day)", 
           y = "Change in Probability of Survival (%)")
  })
  
  # Add download functionality
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("survival_plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      # Open the PNG device and set the file output
      png(file, width = 800, height = 600, res = 96)
      
      # Recreate the plot that is displayed in the app
      mvpa_input <- input$moderate_activity
      lpa_input <- input$light_activity
      age_input <- input$age_group
      sex_input <- input$sex
      smoke_input <- input$smoking

      # Validate predictions data
      if (!is.null(predictions)) {
        age <- c("40-49","50-59","60-69")
        sex <- c("male","female")
        smoker <- c("never","ex","current")
        bmi_cat <- c("healthy","underweight","overweight","obese")
        grid <- expand.grid(age, sex, smoker, bmi_cat)
        colnames(grid) <- c("age","sex","smoker","bmi")
        mvpa_list <- seq(0, 90, 10)
        
        index_1 <- which(grid$age == age_input & grid$sex == sex_input & grid$smoker == smoke_input & grid$bmi == bmi_category_value())
        index_2 <- which(mvpa_list == mvpa_input)
        
        if(length(index_1) > 0 && length(index_2) > 0 && length(predictions) >= index_1 && length(predictions[[index_1]]) >= index_2) {
          
          p <- predictions[[index_1]][[index_2]][[1]]
          var <- predictions[[index_1]][[index_2]][[2]]
          
          if(!is.null(p) && "LPA" %in% colnames(p) && "estimate" %in% colnames(p)) {
            ref_index <- which(p$LPA == lpa_input)
            if(length(ref_index) == 0) ref_index <- 1
            
            p$ref <- p$estimate - p[ref_index,]$estimate
            p$ref.se <- do.call(rbind, lapply(seq(1, nrow(p)), function(x, var, ref_index) {
              sqrt(var[x, x] + var[ref_index, ref_index] - 2 * var[x, ref_index])
            }, var = var, ref_index = ref_index))
            
            # Generate the plot and save to file
            plot <- ggplot(p, aes(x = LPA, y = ref)) + 
              geom_line(colour = "darkblue") + 
              geom_ribbon(aes(ymin = ref - 1.96 * ref.se, ymax = ref + 1.96 * ref.se), alpha = 0.3, fill = "skyblue") +
              annotate("point", x = lpa_input, y = p[ref_index,]$ref, colour = "darkblue", shape = 18, size = 2.5) +
              geom_vline(xintercept = lpa_input, linetype = "dashed", color = "blue") +
              theme_minimal() +
              labs(x = "Light-Intensity Physical Activity (mins/day)", y = "Change in Probability of Survival (%)")
            
            print(plot)
          }
        }
      }
      
      # Close the device
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
