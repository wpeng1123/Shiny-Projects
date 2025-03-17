# Safety Narrative Generator

A Shiny application for generating standardized safety narratives from clinical trial data.

## Features

- **Data Import**: Support for CSV and Excel files with flexible separator options
- **Customizable Templates**: Edit and save narrative templates with variable placeholders
- **Batch Processing**: Generate narratives for multiple subjects at once
- **Export Options**: Download narratives in text format with proper formatting
- **Visualization**: Interactive visualization of adverse events with filtering options
- **Summary Statistics**: Calculate and display subject counts and exposure-adjusted incidence rates

## Required Data Format

Your input file should contain the following columns:

- `USUBJID`: Subject identifier
- `AGE`: Subject age
- `SEX`: Subject gender
- `AETERM`: Adverse event term
- `ASTDT`: AE start date
- `AESER`: Seriousness assessment
- `AEREL`: Relatedness to study drug
- `AEOUT`: Event outcome

## Installation

1. Clone this repository:
```bash
git clone https://github.com/yourusername/safety-narrative-generator.git
cd safety-narrative-generator
```

2. Install required R packages:
```R
install.packages(c("shiny", "ggplot2", "DT", "dplyr", "tidyr", "bslib", "devtools", "readxl"))
```

3. Run the application:
```R
shiny::runApp()
```

## Usage

1. Navigate to the "Data Input" tab and upload your data file(s)
2. Review or modify the narrative template in the "Template" tab
3. Generate and view narratives in the "Narratives" tab
4. Explore data visualizations in the "Visualizations" tab
5. Download all narratives using the "Download All Narratives" button

## Contact

For questions or support, please contact: pengvirginia@gmail.com # Shiny-Projects
