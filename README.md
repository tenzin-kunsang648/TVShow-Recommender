## URL for webpage
https://tenzin-kunsang.shinyapps.io/project-kunsang-gormally-mohamed/

## URL to screencast (if not embedded in webpage):
https://carleton.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=0c0563ec-9ccf-4ee9-b72c-abd4017f2a9d

## List of files in the repository

*finalProject.rmd*: This file creates the `data.csv` file that we use in our Shiny app. It scrapes urls of the tv shows from kaggle, and then maps our function that grabs all of the relevant show data over those urls. We then combine our own scraped data with the original dataset from Kaggle, clean it, and write it to `data.csv`

*tv_shows_kaggle.csv*: This file contains the original data from Kaggle, from which we use the year of the show and the coded columns for which show is available on which service. 

*tv_data.csv*: this file contains the raw scraped info from our urls, which we wrote to a csv file because the process was quite long and we didn't want to run it every time. 

*data.csv*: This file contains the cleaned and joined data that we use for our Shiny app
