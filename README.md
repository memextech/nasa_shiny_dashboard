# Space Exploration Dashboard

An interactive R Shiny dashboard that visualizes space-related data using NASA's Open APIs.

## Features

- Daily Astronomy Picture with description
- Near-Earth Objects tracking
- Mars Rover latest photos
- International Space Station location tracking
- Space weather monitoring
- Interactive timeline of space missions

## Prerequisites

- R >= 4.1.0
- RStudio (recommended for development)
- Required R packages (installed automatically via renv)

## Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/space-exploration-dashboard.git
cd space-exploration-dashboard
```

2. Install required packages using renv:
```R
install.packages("renv")
renv::restore()
```

3. Create a config.yml file in the config directory with your NASA API key:
```yaml
default:
  nasa_api_key: "YOUR_API_KEY"
```

## Usage

Run the application locally:
```R
shiny::runApp()
```

Or visit the deployed version at: [URL to be added]

## Project Structure

```
.
├── R/                  # R functions and modules
│   ├── modules/       # Shiny modules
│   └── utils.R        # Utility functions
├── config/            # Configuration files
├── data/             # Data files
├── tests/            # Unit tests
├── www/              # Static assets
│   ├── css/         # CSS files
│   └── js/          # JavaScript files
├── app.R            # Main application file
├── renv.lock        # Package dependencies
└── README.md        # This file
```

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- NASA Open APIs
- R Shiny community
- Contributors and maintainers

