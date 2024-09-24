library(shiny)
library(dplyr)
library(shinyWidgets)
library(reactable)
library(tidyr)
library(plotly)
library(shinycssloaders)
library(bslib)  # For theming

# Load the data
data <- tryCatch({
  read.csv("data/results_with_crew.csv", stringsAsFactors = FALSE, na.strings = c("", "NA", "NULL"))
}, error = function(e) {
  message("Error loading data: ", e)
  NULL
})

# Ensure data is loaded
if (is.null(data)) {
  stop("Failed to load data.")
}

# Ensure 'startYear' and 'endYear' are numeric
data$startYear <- as.numeric(data$startYear)
data$endYear <- as.numeric(data$endYear)

# Verify the columns in the loaded data
required_columns <- c("primaryTitle", "startYear", "endYear", "rank", "averageRating", "numVotes",
                      "directors", "writers", "genres", "Title_IMDb_Link")
missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns) > 0) {
  stop("The following required columns are missing from the data: ",
       paste(missing_columns, collapse = ", "))
}

# Extract unique TV show titles, director names, writer names, and genres for virtualSelectInput choices
unique_titles <- unique(as.character(data$primaryTitle))
unique_directors <- unique(unlist(strsplit(as.character(data$directors), ",\\s*")))
unique_writers <- unique(unlist(strsplit(as.character(data$writers), ",\\s*")))
unique_genres <- sort(unique(unlist(strsplit(as.character(data$genres), ",\\s*"))))

# Create a Bootstrap theme using bslib
my_theme <- bs_theme(
  bootswatch = "cosmo",  # Choose a cohesive Bootswatch theme
  bg = "#FFFFFF",        # Background color
  fg = "#333333",        # Foreground color
  primary = "#2780E3",   # Primary color matching Cosmo theme
  secondary = "#373A3C", # Secondary color
  success = "#3FB618",
  info = "#9954BB",
  warning = "#FF7518",
  danger = "#FF0039",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat")
)

# Define UI
ui <- fluidPage(
  theme = my_theme,
  tags$head(
    tags$style(HTML("
      .vscomp-dropbox-container {
        z-index: 99999 !important;
      }
      .form-group {
        margin-bottom: 0px !important;
      }
      .reset-button-container {
        margin-top: 20px;
      }
      .navbar-nav > li > a {
        color: #FFFFFF !important;
        padding: 10px;
      }
      h1, h2, h3, h4, h5, h6 {
        color: #333333;
      }
    "))
  ),
  # Header with links
  fluidRow(
    column(8,
           h1("IMDb Data Dashboard", style = "margin: 3px 0;")
    ),
    column(4,
           tags$ul(class = "nav navbar-nav navbar-right",
                   tags$li(
                     tags$a(href = "https://www.linkedin.com/in/tiagoadrianunes/",
                            target = "_blank",
                            icon("linkedin"),
                            " LinkedIn")
                   ),
                   tags$li(
                     tags$a(href = "https://github.com/TiagoAdriaNunes/imdb_top_5000_tv_shows/",
                            target = "_blank",
                            icon("github"),
                            " GitHub")
                   )
           )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      HTML("<h3>Top 5000 TV Shows<br>Last Update: 2024-09-23</h3>"),
      virtualSelectInput(
        inputId = "primaryTitle", 
        label = "Title",
        choices = unique_titles, 
        multiple = TRUE, 
        search = TRUE,
        zIndex = 0,
        noOfDisplayValues = 3,
        optionsCount = 5,
        showValueAsTags = TRUE,
        showSelectedOptionsFirst = TRUE,
        position = "bottom left",
        placeholder = "Enter TV show title..."
      ),
      virtualSelectInput(
        inputId = "director", 
        label = "Director", 
        choices = unique_directors, 
        multiple = TRUE, 
        search = TRUE,
        showSelectedOptionsFirst = TRUE,
        zIndex = 0,
        noOfDisplayValues = 3,
        optionsCount = 5,
        showValueAsTags = TRUE,
        position = "bottom left",
        placeholder = "Enter director name..."
      ),
      virtualSelectInput(
        inputId = "writer", 
        label = "Writer", 
        choices = unique_writers, 
        multiple = TRUE, 
        search = TRUE,
        showSelectedOptionsFirst = TRUE,
        zIndex = 0,
        noOfDisplayValues = 3,
        optionsCount = 5,
        showValueAsTags = TRUE,
        position = "bottom left",
        placeholder = "Enter writer name..."
      ),
      virtualSelectInput(
        inputId = "genre", 
        label = "Genre (Max: 3)", 
        choices = unique_genres, 
        multiple = TRUE, 
        search = TRUE,
        showSelectedOptionsFirst = TRUE,
        showValueAsTags = TRUE,
        zIndex = 0,
        noOfDisplayValues = 3,
        optionsCount = 5,
        maxValues = 3,
        position = "bottom left",
        placeholder = "Select genres (Max: 3)..."
      ),
      sliderInput("yearRange", "Year Range",
                  min = min(data$startYear, na.rm = TRUE), 
                  max = max(data$endYear, na.rm = TRUE), 
                  value = c(min(data$startYear, na.rm = TRUE), max(data$endYear, na.rm = TRUE)),
                  sep = ""),
      sliderInput("rank", "Rank", 
                  min = min(data$rank, na.rm = TRUE), 
                  max = max(data$rank, na.rm = TRUE), 
                  value = c(min(data$rank, na.rm = TRUE), max(data$rank, na.rm = TRUE))),
      sliderInput("rating", "Average Rating", 
                  min = min(data$averageRating, na.rm = TRUE), 
                  max = max(data$averageRating, na.rm = TRUE), 
                  value = c(min(data$averageRating, na.rm = TRUE), max(data$averageRating, na.rm = TRUE))),
      sliderInput("votes", "Number of Votes",
                  min = min(data$numVotes, na.rm = TRUE), 
                  max = max(data$numVotes, na.rm = TRUE), 
                  value = c(min(data$numVotes, na.rm = TRUE), max(data$numVotes, na.rm = TRUE))),
      sliderInput("num_results", "Number of Rows to Display in Graph", 
                  min = 1, max = 20, value = 10),
      div(class = "reset-button-container", 
          actionButton("reset", "Reset Sliders", icon = icon("redo")))
    ),
    mainPanel(
      h2("Best Genres by TV Shows"),
      shinycssloaders::withSpinner(
        plotlyOutput("plot_genres_by_tv_shows"), 
        type = 3, 
        color = "#2780E3",
        color.background = "#FFFFFF"
      ),
      reactableOutput("dataTable")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Function to filter data based on user inputs
  filter_data <- function() {
    filtered <- data
    
    # Filter by selected directors
    if (length(input$director) > 0) {
      filtered <- filtered %>% 
        filter(sapply(strsplit(as.character(directors), ",\\s*"), function(d) any(input$director %in% d)))
    }
    
    # Filter by selected writers
    if (length(input$writer) > 0) {
      filtered <- filtered %>% 
        filter(sapply(strsplit(as.character(writers), ",\\s*"), function(w) any(input$writer %in% w)))
    }
    
    # Filter by selected titles
    if (length(input$primaryTitle) > 0) {
      filtered <- filtered %>% filter(primaryTitle %in% input$primaryTitle)
    }
    
    # Filter by year range (startYear and endYear)
    filtered <- filtered %>%
      filter((startYear >= input$yearRange[1] & startYear <= input$yearRange[2]) |
               (endYear >= input$yearRange[1] & endYear <= input$yearRange[2]),
             rank >= input$rank[1] & rank <= input$rank[2],
             averageRating >= input$rating[1] & averageRating <= input$rating[2],
             numVotes >= input$votes[1] & numVotes <= input$votes[2])
    
    # Filter by genres
    if (length(input$genre) > 0) {
      filtered <- filtered %>%
        filter(sapply(strsplit(as.character(genres), ",\\s*"), function(g) all(input$genre %in% g)))
    }
    
    filtered
  }
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    filter_data()
  })
  
  # Output data table
  output$dataTable <- renderReactable({
    reactable(filteredData() %>%
                select(Title_IMDb_Link, startYear, endYear, rank, averageRating, numVotes, genres), 
              columns = list(
                Title_IMDb_Link = colDef(name = "Title/IMDb Link", html = TRUE, minWidth = 220),
                startYear = colDef(name = "Start Year", minWidth = 50),
                endYear = colDef(
                  name = "End Year",
                  minWidth = 50,
                  cell = function(value) {
                    if (is.na(value)) {
                      ""
                    } else {
                      as.character(value)
                    }
                  }
                ),
                rank = colDef(name = "Rank", minWidth = 50),
                averageRating = colDef(name = "Average Rating", minWidth = 70),
                numVotes = colDef(name = "Votes", minWidth = 80),
                genres = colDef(name = "Genres", minWidth = 150)
              ),
              searchable = FALSE,
              compact = TRUE,
              defaultPageSize = 10,
              pageSizeOptions = c(10, 25, 50, 100),
              showPageSizeOptions = TRUE,
              bordered = TRUE,
              striped = TRUE,
              highlight = TRUE)
  })
  
  # Function to create genre plot
  create_plot <- function(data, x, y, x_title, y_title) {
    plot_ly(
      data = data,
      x = x,
      y = y,
      type = "bar",
      marker = list(color = "#2780E3"),
      orientation = "h"
    ) %>%
      layout(
        xaxis = list(title = x_title),
        yaxis = list(title = y_title)
      ) %>%
      config(displayModeBar = FALSE)
  }
  
  # Plot: Best Genres by TV Shows
  output$plot_genres_by_tv_shows <- renderPlotly({
    plot_data <- filteredData() %>%
      separate_rows(genres, sep = ",\\s*") %>%
      group_by(genres) %>%
      summarise(show_count = n()) %>%
      arrange(desc(show_count)) %>%
      head(input$num_results) %>%
      mutate(genres = factor(genres, levels = rev(unique(genres))))
    
    create_plot(plot_data, ~show_count, ~genres, "Number of TV Shows", "Genre")
  })
  
  # Reset button functionality
  observeEvent(input$reset, {
    updateSliderInput(session, "yearRange", value = c(min(data$startYear, na.rm = TRUE), max(data$endYear, na.rm = TRUE)))
    updateSliderInput(session, "rank", value = c(min(data$rank, na.rm = TRUE), max(data$rank, na.rm = TRUE)))
    updateSliderInput(session, "rating", value = c(min(data$averageRating, na.rm = TRUE), max(data$averageRating, na.rm = TRUE)))
    updateSliderInput(session, "votes", value = c(min(data$numVotes, na.rm = TRUE), max(data$numVotes, na.rm = TRUE)))
    updateSliderInput(session, "num_results", value = 10)
  })
}

# Run the app
shinyApp(ui, server)
