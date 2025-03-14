# Space Explorer Dashboard

## Project Overview
This R Shiny dashboard integrates multiple space-related data sources:
- NASA's Astronomy Picture of the Day (APOD)
- Mars Rover photos
- Near-Earth Object tracking
- International Space Station (ISS) location

### Structure
```
project/
├── R/
│   ├── modules/           # Shiny modules for each feature
│   │   ├── apod_module.R  # Astronomy Picture of the Day
│   │   ├── mars_module.R  # Mars Rover photos
│   │   ├── neo_module.R   # Near-Earth Objects
│   │   ├── iss_module.R   # ISS Tracking
│   │   └── dashboard_module.R  # Main dashboard view
│   └── utils.R           # Utility functions
├── www/
│   ├── css/             # Stylesheets
│   └── assets/          # Static assets (images)
├── docs/               # Documentation
├── data/              # Cached data
├── .env              # Environment variables (not in git)
├── config.yml        # Configuration
└── app.R            # Main application file
```

## Setup Instructions

### Environment Setup
1. Copy `.env.example` to `.env`
2. Get NASA API key:
   - Visit https://api.nasa.gov/
   - Sign up for an API key
   - Add to .env: `NASA_API_KEY=your_key_here`

### Local Development
```bash
# Install R dependencies
Rscript -e "install.packages(c('renv', 'shiny', 'httr', 'jsonlite', 'leaflet', 'dplyr', 'config', 'plotly'))"

# Initialize renv
Rscript -e "renv::init()"

# Run the app locally
Rscript -e "shiny::runApp()"
```

### Deployment
```bash
# First time setup
R -e "rsconnect::setAccountInfo(name='account_name', token='token', secret='secret')"

# Set environment variables on shinyapps.io
R -e "rsconnect::setProperty('NASA_API_KEY', 'your_key_here')"

# Deploy
R -e "rsconnect::deployApp()"
```

## Development Rules

### Git Workflow
- Don't commit unless told to
- If told to commit: check status, add files, and commit
- Don't merge or push changes unless told to

### File Editing
- Always perform precise file editing with search/replace format
- Use SEARCH/REPLACE blocks for all file changes
- Ensure SEARCH blocks exactly match existing content

### Image Sizing
- Dashboard tiles: 280px height for containers
- Images: 260px max-height
- Social share image: 1200x630px (1.91:1 aspect ratio)

### Loading States
- Show loading spinners for APOD and Mars tiles
- Let leaflet handle its own loading for ISS map
- Keep loading states consistent across similar components

### Meta Tags
- Use GitHub raw URLs for static assets
- Include OpenGraph and Twitter card meta tags
- Maintain proper social preview dimensions
