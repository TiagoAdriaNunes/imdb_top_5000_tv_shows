box::use(
  dplyr[filter],
  plotly[add_annotations, config, layout, plot_ly],
)

#' @export
filter_data <- function(data, director, writer, primary_title, year, rank, rating, votes, genre) {
  filtered <- data

  # Filter by directors
  if (length(director) > 0) {
    filtered <- filtered |>
      filter(sapply(strsplit(as.character(directors), ",\\s*"), function(d) {
        any(director %in% d)
      }))
  }

  # Filter by writers
  if (length(writer) > 0) {
    filtered <- filtered |>
      filter(sapply(strsplit(as.character(writers), ",\\s*"), function(w) {
        any(writer %in% w)
      }))
  }

  # Filter by TV show titles
  if (length(primary_title) > 0) {
    filtered <- filtered |>
      filter(.data$primaryTitle %in% primary_title)
  }

  # Extract range values to avoid variable name collisions with column names
  year_min <- year[1]
  year_max <- year[2]
  rank_min <- rank[1]
  rank_max <- rank[2]
  rating_min <- rating[1]
  rating_max <- rating[2]
  votes_min <- votes[1]
  votes_max <- votes[2]

  # Filter by range inputs (TV shows use both startYear and endYear)
  filtered <- filtered |>
    filter(
      (.data$startYear >= year_min & .data$startYear <= year_max) |
        (.data$endYear >= year_min & .data$endYear <= year_max) |
        (is.na(.data$endYear) & .data$startYear >= year_min & .data$startYear <= year_max),
      .data$rank >= rank_min & .data$rank <= rank_max,
      .data$averageRating >= rating_min & .data$averageRating <= rating_max,
      .data$numVotes >= votes_min & .data$numVotes <= votes_max
    )

  # Filter by genres (must match ALL selected genres)
  if (length(genre) > 0) {
    filtered <- filtered |>
      filter(sapply(strsplit(as.character(genres), ",\\s*"), function(g) {
        all(genre %in% g)
      }))
  }

  filtered
}

#' @export
create_empty_plot <- function(message = "No Data Available", chart_color = "#427ea6") {
  plot_ly() |>
    add_annotations(
      text = message,
      x = 0.5,
      y = 0.5,
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      font = list(size = 20, color = chart_color)
    ) |>
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    ) |>
    config(displayModeBar = FALSE)
}
