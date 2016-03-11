<h4>Census Viewer / Tabulator</h4>

<br>
<h5> Introduction </h5>
This repository hosts a web-based application that displays a table of counts and an interactive bar chart based on real census data collected from secondary schools around New Zealand. The app can be accessed via the following link: http://docker.stat.auckland.ac.nz/spawn/?application=cas. 

This project is supported by Census At School / Tatauranga Ki Te Kura Aotearoa, which aims to be the first port of call for New Zealand educators seeking information and support for their teaching of statistics. For more information, please visit  http://new.censusatschool.org.nz/ to find a large number of original quality teaching resources including workshops, presentations, classroom activities, research papers, interactive data analysis tools, real student data sets and essential links to other statistics websites. 

<h5> Instructions for Manual Testing </h5>
To run the app *manually* in R, you can either clone the repository or download and extract the .zip file and start an R 
session in the directory in which the .zip was extracted. You must ensure that the following R packages are installed:

| Package  | Version | 
|:---------|:--------|
|[devtools]| 1.7.0   |
|[DT]      | 0.1     |
|[markdown]| 0.7.4   |
|[rCharts] | 0.4.5   |
|[RMySQL]  | 0.10.4  |
|[shiny]   | 0.12.1  |

[devtools]: <https://github.com/hadley/devtools>
[DT]: <https://github.com/rstudio/DT>
[markdown]: <https://github.com/rstudio/rmarkdown>
[rCharts]: <https://github.com/cpi2025/rCharts>
[RMySQL]: <https://github.com/rstats-db/RMySQL>
[shiny]: <https://github.com/rstudio/shiny>

Note that the app may not work correctly with different package versions. Below is some code that might be of some use to testers.

```{r}
###  Code to install the required packages.s
install.packages("devtools")
install.packages("markdown")
install.packages("shiny")
install.packages("DT")
devtools::install_github("ramnathv/rCharts")

###  Code to run the app. Change the path as appropriate.
shiny::runApp("path/to/app/directory")
```

Note that the app has only been tested on R versions >= 3.2.0 ("Full of Ingredients"). Any questions, suggestions, or bug reports may be forwarded to the  <a href="mailto:cpar137@aucklanduni.ac.nz">developer</a>.

<h5> Future Work </h5>

- [ ] *Advanced* tab for customization;
- [ ] Make it possible to "swap" x and y;
- [ ] Hack table so users can edit it directly;
- [ ] Add feedback form;

<h5> Update History </h5>
- [x] Fix help modal button;
- [x] Allow users to specify boundary points / conditions as raw expressions;
- [x] Set appropriate boundary points for numeric variables;
- [x] Ordering of factor levels - needs rethinking
- [x] Automated update process (DockerHub);
- [x] Change cleaning structure so that less data is read in from db;
- [x] Change table load structure for speed up;
- [x] Fix table resize issue (interactivity lost);
- [x] Add *remove* button for variables;
- [x] Fix empty character string bug;
- [x] Added 2009 and 2011 tables (with recycled variables);
- [x] Clean up nvd3 to avoid label overlapping;
- [x] Fix known reactivity issues, e.g. when changing "year";
- [x] Remove authentication details;
- [x] Pull data directly from CAS database;
- [x] Work on valid points from the Merriman list;
- [x] Fix factor level sorting bug;
- [x] Add reactive help text to (internally) redesigned app;
- [x] Alter reactivity: replace reactive expressions with observers;
- [x] Store all reactive values in one structure;
- [x] Write full variable information file for 2015 data;
- [x] Write general cleaning function to be sourced from "global.R";
- [x] Update census 2013 version (new data cleaning mechanism);
- [x] Send .md outlining cleaning strategy to AC;
- [x] Write script for data cleaning;
- [x] Randomize column order for data samples;
- [x] Testing on Windows/Mac OS.
