### Project Overview
>https://lamvinam.github.io/data-viz-post-sentiment/

<br><br>

### Repository Structure and Key Files
- code/explore.Rmd: This R Markdown file contains the R code used for initial data exploration and transformation. Running this file (code chunks) will process the raw data and generate the two CSV files used for visualization.
- data_source/sentiment.csv: This file contains the raw dataset used as input for the R transformation script.
- tableau_workbook.twb: This is the main Tableau workbook file containing all dashboards and visualizations.
  
<br><br>

### Setup and Running the Code

To replicate the data transformation steps:

1. Ensure you have R and RStudio installed.

2. Install the necessary R packages (e.g., dplyr, lubridate, rmarkdown).

3. Open the code/explore.Rmd file in RStudio.

4. Ensure sentiment.csv is in the expected location or update the file path in the Rmd.

5. Run the code chunks in the Rmd file to perform the transformations and generate metrics_all_games.csv and metrics_single_game.csv.

To explore the visualizations locally:

1. Ensure you have Tableau Desktop installed.

2. Open the tableau_workbook.twb file in Tableau Desktop.

3. The workbook is configured to connect to the generated CSV files.
