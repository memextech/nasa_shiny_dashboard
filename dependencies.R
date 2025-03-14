# List of required packages
packages <- c(
  "shiny",
  "httr",
  "jsonlite",
  "leaflet",
  "dplyr",
  "config",
  "plotly",
  "markdown"
)

# Install missing packages
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}