library(shiny)
library(shinydashboard)
library(dplyr)
library(shinyWidgets)
library(reactable)
library(tidyr)
library(plotly)
library(shinycssloaders)
library(duckdb)

# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Define the path to the results file
results_file <- "data/results_with_crew.csv"

# Get file creation/modification date for Last Update display
file_date <- if (file.exists(results_file)) {
  format(file.mtime(results_file), "%Y-%m-%d")
} else {
  "N/A"
}
print(paste("File last modified on:", file_date))

# Load the data into DuckDB
dbExecute(
  con,
  "CREATE TABLE IF NOT EXISTS results_with_crew
AS SELECT * FROM 'data/results_with_crew.csv'"
)

# Example SQL query
data <- dbGetQuery(con, "SELECT * FROM results_with_crew")

# Ensure numeric columns are properly converted
data$startYear <- as.numeric(data$startYear)
data$endYear <- as.numeric(data$endYear)
data$rank <- as.numeric(data$rank)
data$averageRating <- as.numeric(data$averageRating)
data$numVotes <- as.numeric(data$numVotes)

# Set default values for min/max to handle NA values
min_start_year <- min(data$startYear, na.rm = TRUE)
max_end_year <- max(data$endYear, na.rm = TRUE)
if (!is.finite(min_start_year)) min_start_year <- 1900
if (!is.finite(max_end_year)) max_end_year <- 2025

min_rank <- min(data$rank, na.rm = TRUE)
max_rank <- max(data$rank, na.rm = TRUE)
if (!is.finite(min_rank)) min_rank <- 1
if (!is.finite(max_rank)) max_rank <- 5000

min_rating <- min(data$averageRating, na.rm = TRUE)
max_rating <- max(data$averageRating, na.rm = TRUE)
if (!is.finite(min_rating)) min_rating <- 0
if (!is.finite(max_rating)) max_rating <- 10

min_votes <- min(data$numVotes, na.rm = TRUE)
max_votes <- max(data$numVotes, na.rm = TRUE)
if (!is.finite(min_votes)) min_votes <- 0
if (!is.finite(max_votes)) max_votes <- 1000000

# Verify the columns in the loaded data
required_columns <- c(
  "primaryTitle",
  "startYear",
  "endYear",
  "rank",
  "averageRating",
  "numVotes",
  "directors",
  "writers",
  "genres",
  "Title_IMDb_Link"
)
missing_columns <- setdiff(required_columns, names(data))
if (length(missing_columns) > 0) {
  stop(
    "The following required columns are missing from the data: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Extract unique TV show titles, director names,
# and writer names for virtualSelectInput choices
unique_titles <- unique(as.character(data$primaryTitle))
unique_directors <- unique(unlist(strsplit(as.character(data$directors), ",\\s*")))
unique_writers <- unique(unlist(strsplit(as.character(data$writers), ",\\s*")))
unique_genres <- sort(unique(unlist(strsplit(
  as.character(data$genres), ",\\s*"
))))

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "IMDb TV Shows Dashboard",
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://www.linkedin.com/in/tiagoadrianunes/",
        target = "_blank",
        icon("linkedin"),
        "LinkedIn",
        style = "color: white; padding: 10px;"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://github.com/TiagoAdriaNunes/imdb_top_5000_tv_shows",
        target = "_blank",
        icon("github"),
        "GitHub",
        style = "color: white; padding: 10px;"
      )
    )
  ),
  dashboardSidebar(
    tags$head(tags$style(
      HTML(
        "
        .vscomp-dropbox-container {
          z-index: 99999 !important;
        }
        .sidebar-menu > .menu-item {
          margin-bottom: 0px !important;
        }
        .form-group {
          margin-bottom: 0px !important;
        }
      "
      )
    )),
    sidebarMenu(
      menuItem(
        HTML(paste0("Top 5000 TV Shows<br>Last Update: ", file_date)),
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      fluidRow(
        column(
          width = 12,
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
          )
        )
      ),
      sliderInput(
        "yearRange",
        "Year Range",
        min = min_start_year,
        max = max_end_year,
        value = c(min_start_year, max_end_year)
      ),
      sliderInput(
        "rank",
        "Rank",
        min = min_rank,
        max = max_rank,
        value = c(min_rank, max_rank)
      ),
      sliderInput(
        "rating",
        "Average Rating",
        min = min_rating,
        max = max_rating,
        value = c(min_rating, max_rating)
      ),
      sliderInput(
        "votes",
        "Number of Votes",
        min = min_votes,
        max = max_votes,
        value = c(min_votes, max_votes)
      ),
      sliderInput(
        "num_results",
        "Number of results to Display in Charts",
        min = 1,
        max = 20,
        value = 10
      ),
      div(class = "reset-button-container", actionButton("reset", "Reset filters", icon = icon("redo")))
    )
  ),
  dashboardBody(
    useBusyIndicators(
      spinners = TRUE,
      # Show spinners on calculating outputs
      pulse = TRUE,
      # Show pulsing banner when app is busy
      fade = TRUE # Fade recalculating outputs
    ),
    fluidRow(
      box(
        title = "Best Directors, Writers, and Genres by TV Shows",
        width = 12,
        fluidRow(
          column(
            width = 4,
            shinycssloaders::withSpinner(
              plotlyOutput("plot_directors_by_shows"),
              type = 3,
              color = "#427ea6",
              color.background = "#FFFFFF"
            )
          ),
          column(
            width = 4,
            shinycssloaders::withSpinner(
              plotlyOutput("plot_writers_by_shows"),
              type = 3,
              color = "#427ea6",
              color.background = "#FFFFFF"
            )
          ),
          column(
            width = 4,
            shinycssloaders::withSpinner(
              plotlyOutput("plot_genres_by_shows"),
              type = 3,
              color = "#427ea6",
              color.background = "#FFFFFF"
            )
          )
        )
      )
    ),
    reactableOutput("dataTable")
  )
)

# Define server logic
server <- function(input, output, session) {
  # Function to filter data
  filter_data <- function() {
    filtered <- data
    
    # Filter by directors
    if (length(input$director) > 0) {
      filtered <- filtered %>%
        filter(sapply(strsplit(as.character(directors), ",\\s*"), function(d)
          any(input$director %in% d)))
    }
    # Filter by writers
    if (length(input$writer) > 0) {
      filtered <- filtered %>%
        filter(sapply(strsplit(as.character(writers), ",\\s*"), function(w)
          any(input$writer %in% w)))
    }
    # Filter by TV show titles
    if (length(input$primaryTitle) > 0) {
      filtered <- filtered %>% filter(primaryTitle %in% input$primaryTitle)
    }
    # Filter by year range (startYear and endYear)
    filtered <- filtered %>%
      filter(
        (startYear >= input$yearRange[1] & startYear <= input$yearRange[2]) |
          (endYear >= input$yearRange[1] & endYear <= input$yearRange[2]) |
          (is.na(endYear) & startYear >= input$yearRange[1] & startYear <= input$yearRange[2]),
        rank >= input$rank[1] & rank <= input$rank[2],
        averageRating >= input$rating[1] & averageRating <= input$rating[2],
        numVotes >= input$votes[1] & numVotes <= input$votes[2]
      )
    
    # Filter by genres
    if (length(input$genre) > 0) {
      filtered <- filtered %>%
        filter(sapply(strsplit(as.character(genres), ",\\s*"), function(g)
          all(input$genre %in% g)))
    }
    
    filtered
  }
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    filter_data()
  })
  
  output$dataTable <- renderReactable({
    reactable(
      filteredData() %>%
        select(
          Title_IMDb_Link,
          startYear,
          endYear,
          rank,
          averageRating,
          numVotes,
          directors,
          writers,
          genres
        ),
      columns = list(
        Title_IMDb_Link = colDef(
          name = "Title/IMDb Link",
          html = TRUE,
          minWidth = 220
        ),
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
        numVotes = colDef(
          name = "Number of Votes",
          minWidth = 80,
          format = colFormat(
            separators = TRUE,
            digits = 0,
            locales = "en-US"
          )
        ),
        directors = colDef(name = "Directors", minWidth = 150),
        writers = colDef(name = "Writers", minWidth = 200),
        genres = colDef(name = "Genres", minWidth = 180)
      ),
      searchable = FALSE,
      compact = TRUE,
      defaultPageSize = 10,
      pageSizeOptions = c(10, 25, 50, 100),
      showPageSizeOptions = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE
    )
  })
  
  # Function to create empty plot with message
  create_empty_plot <- function(message = "No Data Available") {
    plot_ly() %>%
      add_annotations(
        text = message,
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 20, color = "#427ea6")
      ) %>%
      layout(xaxis = list(visible = FALSE),
             yaxis = list(visible = FALSE)) %>%
      config(displayModeBar = FALSE)
  }
  
  # Plot: Best Directors by TV Shows
  output$plot_directors_by_shows <- renderPlotly({
    # Get filtered data
    plot_data <- tryCatch({
      filteredData() %>%
        separate_rows(directors, sep = ",\\s*") %>%
        filter(!is.na(directors), directors != "", directors != "-") %>%
        group_by(directors) %>%
        summarise(show_count = n()) %>%
        arrange(desc(show_count)) %>%
        head(input$num_results) %>%
        mutate(directors = factor(directors, levels = rev(unique(directors))))
    }, error = function(e) {
      print(paste("Error in directors plot:", e$message))
      NULL
    })
    
    # Check if data exists and has rows
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(create_empty_plot("No Director Data Available"))
    }
    
    # Create plot with data
    plot_ly(
      data = plot_data,
      x = ~ show_count,
      y = ~ directors,
      type = "bar",
      # Explicitly specify type
      marker = list(color = "#427ea6"),
      orientation = "h"
    ) %>%
      layout(
        xaxis = list(title = "Number of TV Shows"),
        yaxis = list(title = "Director")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Plot: Best Writers by TV Shows
  output$plot_writers_by_shows <- renderPlotly({
    # Get filtered data
    plot_data <- tryCatch({
      filteredData() %>%
        separate_rows(writers, sep = ",\\s*") %>%
        filter(!is.na(writers), writers != "", writers != "-") %>%
        group_by(writers) %>%
        summarise(show_count = n()) %>%
        arrange(desc(show_count)) %>%
        head(input$num_results) %>%
        mutate(writers = factor(writers, levels = rev(unique(writers))))
    }, error = function(e) {
      print(paste("Error in writers plot:", e$message))
      NULL
    })
    
    # Check if data exists and has rows
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(create_empty_plot("No Writer Data Available"))
    }
    
    # Create plot with data
    plot_ly(
      data = plot_data,
      x = ~ show_count,
      y = ~ writers,
      type = "bar",
      # Explicitly specify type
      marker = list(color = "#427ea6"),
      orientation = "h"
    ) %>%
      layout(xaxis = list(title = "Number of TV Shows"),
             yaxis = list(title = "Writer")) %>%
      config(displayModeBar = FALSE)
  })
  
  # Plot: Best Genres by TV Shows
  output$plot_genres_by_shows <- renderPlotly({
    # Get filtered data
    plot_data <- tryCatch({
      filteredData() %>%
        separate_rows(genres, sep = ",\\s*") %>%
        filter(!is.na(genres), genres != "", genres != "-") %>%
        group_by(genres) %>%
        summarise(show_count = n()) %>%
        arrange(desc(show_count)) %>%
        head(input$num_results) %>%
        mutate(genres = factor(genres, levels = rev(unique(genres))))
    }, error = function(e) {
      print(paste("Error in genres plot:", e$message))
      NULL
    })
    
    # Check if data exists and has rows
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(create_empty_plot("No Genre Data Available"))
    }
    
    # Create plot with data
    plot_ly(
      data = plot_data,
      x = ~ show_count,
      y = ~ genres,
      type = "bar",
      # Explicitly specify type
      marker = list(color = "#427ea6"),
      orientation = "h"
    ) %>%
      layout(xaxis = list(title = "Number of TV Shows"),
             yaxis = list(title = "Genre")) %>%
      config(displayModeBar = FALSE)
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session = session, "yearRange", value = c(min_start_year, max_end_year))
    updateSliderInput(session = session, "rank", value = c(min_rank, max_rank))
    updateSliderInput(session = session, "rating", value = c(min_rating, max_rating))
    updateSliderInput(session = session, "votes", value = c(min_votes, max_votes))
    updateSliderInput(session = session, "num_results", value = 10)
    updateVirtualSelect(session = session,
                        "primaryTitle",
                        selected = character(0))
    updateVirtualSelect(session = session, "director", selected = character(0))
    updateVirtualSelect(session = session, "writer", selected = character(0))
    updateVirtualSelect(session = session, "genre", selected = character(0))
  })
}

# Run the app
shinyApp(ui, server)
