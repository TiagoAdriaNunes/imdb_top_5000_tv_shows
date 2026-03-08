# IMDb Top 5000 TV Shows Dashboard

[![IMDb TV Shows Pipeline](https://github.com/TiagoAdriaNunes/imdb_top_5000_tv_shows/actions/workflows/imdb-tv-shows-pipeline.yml/badge.svg)](https://github.com/TiagoAdriaNunes/imdb_top_5000_tv_shows/actions/workflows/imdb-tv-shows-pipeline.yml)

This GitHub repository contains the code for a Shiny dashboard written in R that allows users to explore IMDb data, specifically the top 5000 TV shows. The dashboard provides various filters to help users search for TV shows based on title, director, writer, genre, year range, rank, average rating, and number of votes. Check the dashboard: [https://tiagoadrianunes.shinyapps.io/IMDB_TOP_5000_TV_SHOWS/](https://tiagoadrianunes.shinyapps.io/IMDB_TOP_5000_TV_SHOWS/)

## Features

- **Interactive Data Exploration**: Filter and visualize TV show data
- **Performance Optimized**: Uses DuckDB for efficient data processing
- **Visualizations**: View top directors, writers, and genres with interactive charts
- **IMDb Weighted Ranking**: Implements IMDb's Bayesian weighted average formula

## Quick Start

```bash
# Clone repository
git clone https://github.com/TiagoAdriaNunes/imdb_top_5000_tv_shows.git
cd imdb_top_5000_tv_shows

# Install dependencies
install.packages("renv")
renv::restore()

# Run analysis script
source("imdb_tv_shows_analysis.R")

# Launch Shiny app 
shiny::runApp()
```

## Data Processing

The `imdb_tv_shows_analysis.R` script:
1. Downloads and processes IMDb datasets (title basics, ratings, crew info)
2. Applies the IMDb weighted rating formula (WR = (v/(v+m)) × R + (m/(v+m)) × C)
3. Creates relationships between TV shows, directors, and writers
4. Exports processed data for the Shiny dashboard

## Dashboard Features

- **Smart Filtering**: Find TV shows by any combination of criteria
- **Interactive Charts**: Visualize directors, writers, and genres by TV show count
- **Detailed TV Show Information**: View comprehensive data including ratings, years, and links to IMDb

## Author

- [Tiago Adrian Nunes](https://www.linkedin.com/in/tiagoadrianunes/)

## License

MIT License. See the `LICENSE` file for details.

Information courtesy of  
IMDb  
(https://www.imdb.com).  
Used with permission.

## Contributing

Contributions are welcome! Please fork this repository and submit pull requests for any enhancements or bug fixes.
