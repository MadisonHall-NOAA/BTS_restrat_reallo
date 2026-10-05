library(shiny)
library(dplyr)

# Make sure plot_list and plot_index are loaded in your global environment 
# before running the app.
source("plot_list_setup.R")

ui <- navbarPage(
  title = "RESTRATIFICATION RESULTS",
  
  # ==========================================
  # TAB 1: PLOT NAVIGATOR
  # ==========================================
  tabPanel("PLOT EXPLORER",
           sidebarLayout(
             position = "right",
             
             sidebarPanel(
               width = 3,
               h4("Explore Plots by Filtering."),
               p("Select a species and wait for all plots to populate before making further refinements. Plot loading is slow for first rendering but loads faster on recall. Leave Season and Descriptor blank to include all."),
               
               selectInput("response", "Response Variable:",
                           choices = sort(unique(plot_index$RESPONSE)),
                           multiple = FALSE),
               
               selectInput("common", "Common Name:",
                           choices = c("Choose...", sort(unique(plot_index$COMMON))),
                           multiple = FALSE),
               
               selectInput("season", "Season:",
                           choices = sort(unique(plot_index$SEASON)),
                           multiple = TRUE),
               
               selectInput("descriptor", "Plot type:",
                           choices = sort(unique(plot_index$DESCRIPTOR)),
                           multiple = TRUE)
             ),
             
             mainPanel(
               width = 9,
               # CSS wrapping ensures the left panel is independently scrollable
               div(style = "height: 85vh; overflow-y: auto; padding-right: 15px;",
                   uiOutput("dynamic_plots")
               )
             )
           )
  ),
  
  # ==========================================
  # TAB 2: PLOT COMPARISON
  # ==========================================
  tabPanel("PLOT COMPARISON",
           sidebarLayout(
             position = "right",
             
             sidebarPanel(
               width = 3,
               h4("Compare Specific Plots of interest"),
               p("Add or remove figures for comparison in the window below. There are 2069 plots in total; the full list may take several minutes to populate when first opening the app"),
               # choices = NULL is crucial here for server-side selectize
               selectizeInput("compare_figs", "Enter/Select FIG Numbers:",
                              choices = NULL, 
                              multiple = TRUE,
                              width = "100%",
                              options = list(placeholder = 'Type FIG...'))
             ),
             
             mainPanel(
               width = 9,
               # Scrollable area for the comparison plots on the left
               div(style = "height: 85vh; overflow-y: auto; padding-right: 15px;",
                   uiOutput("comparison_plots")
               )
             )
           )
  )
)


server <- function(input, output, session) {
  
  # ==========================================
  # SERVER-SIDE SELECTIZE LOGIC
  # ==========================================
  # This listens to the "compare_figs" input and handles the search 
  # on the R server rather than in the user's browser, vastly improving speed.
  updateSelectizeInput(session, "compare_figs", 
                       choices = sort(unique(plot_index$FIG)), 
                       server = TRUE)
  
  # ==========================================
  # SERVER LOGIC: PLOT NAVIGATOR
  # ==========================================
  
  # 1. Filter the plot_index based on user selections
  filtered_figs <- reactive({
    df <- plot_index
    
    if (!is.null(input$response) && length(input$response) > 0) {
      df <- df %>% filter(RESPONSE %in% input$response)
    }
    
    if (!is.null(input$common) && length(input$common) > 0) {
      df <- df %>% filter(COMMON %in% input$common)
    }
    
    if (!is.null(input$season) && length(input$season) > 0) {
      df <- df %>% filter(SEASON %in% input$season)
    }
    
    if (!is.null(input$descriptor) && length(input$descriptor) > 0) {
      df <- df %>% filter(DESCRIPTOR %in% input$descriptor)
    }
    
    return(df$FIG)
  })
  
  # 2. Build the UI dynamically for the Navigator tab
  output$dynamic_plots <- renderUI({
    figs <- filtered_figs()
    
    if (length(figs) == 0) {
      return(h4("No plots match your selected filters."))
    }
    
    plot_ui_list <- lapply(figs, function(fig) {
      div(style = "margin-bottom: 40px; border-bottom: 1px solid #eee; padding-bottom: 20px;",
         # h4(paste("Figure", fig)), 
          h4(paste("Figure", fig, ";", "Common = ", plot_index[fig, 3])),
          plotOutput(outputId = paste0("plot_nav_", fig), height = "500px")
      )
    })
    
    do.call(tagList, plot_ui_list)
  })
  
  
  # ==========================================
  # SERVER LOGIC: PLOT COMPARISON
  # ==========================================
  
  output$comparison_plots <- renderUI({
    figs <- input$compare_figs
    
    if (is.null(figs) || length(figs) == 0) {
      return(h4(style = "color: grey;", "Please enter the figure numbers that you wish to compare."))
    }
    
    # Generate plot outputs. Wrapped in inline-block divs to render side-by-side
    comp_ui_list <- lapply(figs, function(fig) {
      div(style = "display: block; width: 100%; margin-bottom: 20px; 
              border: 1px solid #ccc; padding: 10px; background-color: #fcfcfc;",
      #style = "display: block; width: 98%; margin: 1%; vertical-align: top; 
         # border: 1px solid #ccc; padding: 10px; background-color: #fcfcfc;",
        # h4(paste("Figure:", fig)),
          h4(paste("Figure", fig, ";", "Common = ", plot_index[fig, 3])),
          plotOutput(outputId = paste0("plot_comp_", fig), height = "400px")
      )
    })
    
    do.call(tagList, comp_ui_list)
  })
  
  
  # ==========================================
  # RENDER PLOTS (Shared generation logic)
  # ==========================================
  
  lapply(plot_index$FIG, function(fig) {
    
    # Render for Navigator
    output[[paste0("plot_nav_", fig)]] <- renderPlot({
      plot_list[[as.character(fig)]]
    })
    
    # Render for Comparison
    output[[paste0("plot_comp_", fig)]] <- renderPlot({
      plot_list[[as.character(fig)]]
    })
    
  })
}

shinyApp(ui, server)
