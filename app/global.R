# Load required libraries using box
box::use(
  DBI[dbConnect, dbExecute, dbGetQuery],
  duckdb[duckdb],
  logger[log_info]
)

# Connect to DuckDB
con <- dbConnect(duckdb(), dbdir = ":memory:")

# Remove existing file if it exists
results_file <- "data/imdb_top_5000_tv_shows.csv"
if (file.exists(results_file)) {
  file.remove(results_file)
  log_info("Removed existing file: {results_file}")
}

# Download the file from GitHub
github_url <- paste0(
  "https://raw.githubusercontent.com/TiagoAdriaNunes/imdb_top_5000_tv_shows/",
  "main/app/data/imdb_top_5000_tv_shows.csv"
)
dir.create(dirname(results_file), showWarnings = FALSE, recursive = TRUE)
download.file(github_url, results_file, mode = "wb")
log_info("File downloaded from GitHub to: {results_file}")

# Get file creation/modification date for Last Update display
file_date <- format(file.mtime(results_file), "%Y-%m-%d")
log_info("File last modified on: {file_date}")

# Load the data into DuckDB
dbExecute(
  con,
  "CREATE TABLE IF NOT EXISTS imdb_top_5000_tv_shows
AS SELECT * FROM 'data/imdb_top_5000_tv_shows.csv'"
)

# Retrieve data from DuckDB
data <- dbGetQuery(con, "SELECT * FROM imdb_top_5000_tv_shows")

# Ensure numeric columns are properly converted
data$startYear <- as.numeric(data$startYear)
data$endYear <- as.numeric(data$endYear)
data$rank <- as.numeric(data$rank)
data$averageRating <- as.numeric(data$averageRating)
data$numVotes <- as.numeric(data$numVotes)

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

# Extract unique values for filter choices
unique_titles <- unique(as.character(data$primaryTitle))
unique_directors <- unique(unlist(strsplit(as.character(data$directors), ",\\s*")))
unique_writers <- unique(unlist(strsplit(as.character(data$writers), ",\\s*")))
unique_genres <- sort(unique(unlist(strsplit(
  as.character(data$genres),
  ",\\s*"
))))

# Constants
chart_color <- "#427ea6"
spinner_type <- 3
spinner_color <- "#427ea6"
spinner_bg_color <- "#FFFFFF"
