<h4> Census Viewer </h4>
<h5> Last Updated: July 27, 2015. </h5>

This git repository hosts a simple RShiny application that displays a table of counts and a side-by-side bar chart based on census data from secondary schools around New Zealand (currently only the 2013 data). To test the app, try running the following in R:

```{r}
###  If you don't have the packages "devtools", "rCharts", "shiny",
###  or "DT", install them.
install.packages("devtools")
devtools::install_github("ramnathv/rCharts")
install.packages("shiny")
install.packages("DT")

###  Download and extract the .zip file for the app and open up R in
###  the directory in which you extracted the file. Then try running:
shiny::runApp()
```

<h5> Dev Notes </h5>
- Pull data from CaS database;
- If data is discrete, don't group using cut;
- Docker script: avoid using devtools to install rCharts.
- ~~Update census 2013 version (new data mechanism)~~
- ~~Send .md outlining cleaning strategy to AC;~~
- ~~Write script for data cleaning;~~
- ~~Randomize column order for data samples;~~
- ~~Testing on Windows/Mac OS;~~


<h5> What is Census At School? </h5>
Census At School New Zealand / Tatauranga Ki Te Kura Aotearoa aims to be the first port of call for New Zealand teachers looking for information and support for their teaching of statistics. For more information, please visit  http://new.censusatschool.org.nz/ to find a large number of original quality teaching resources including workshops, presentations, classroom activities, research papers, interactive data analysis tools, real student data sets and essential links to other statistics websites. 
