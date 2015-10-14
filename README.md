<h4> Census Viewer </h4>
<h6> Last Updated: October 14, 2015. </h6>

<h5> Introduction </h5>
This repository hosts a web-based application that displays a table of counts and an interactive bar chart based on real census data collected from secondary schools around New Zealand. The app can be accessed via the following link: http://docker.stat.auckland.ac.nz/spawn/?application=cas. 

This project is supported by Census At School / Tatauranga Ki Te Kura Aotearoa, which aims to be the first port of call for New Zealand educators seeking information and support for their teaching of statistics. For more information, please visit  http://new.censusatschool.org.nz/ to find a large number of original quality teaching resources including workshops, presentations, classroom activities, research papers, interactive data analysis tools, real student data sets and essential links to other statistics websites. 

<h5> Instructions for Manual Testing </h5>
To run the app *manually* in R, you can either simply clone the repository or download and extract the zip file and start an R session in the directory in which the zip was extracted. You must ensure that the following R packages are installed:

- devtools v1.7.0;
- DT v0.1;
- markdown v0.7.4;
- rCharts v0.4.5;
- RMySQL v. 0.10.4;
- shiny v0.12.1.

Below is some code that might be of some use to testers.

```{r}
###  Code to install the required packages.
install.packages("devtools")
install.packages("markdown")
install.packages("shiny")
install.packages("DT")
devtools::install_github("ramnathv/rCharts")

###  Code to run the app.
shiny::runApp()
```

Note that CAS has only been tested on R versions >= 3.2.0 ("Full of Ingredients"). Any questions, suggestions, or bug reports may be forwarded to Chris Park <cpar137@aucklanduni.ac.nz>.

<h5> Development Notes </h5>
- Set appropriate boundary points for numeric variables;
- ~~Remove authentication details;~~
- ~~Pull data directly from CAS database;~~
- ~~Work on valid points from the Merriman list;~~
- ~~Fix factor level sorting bug;~~
- ~~Add reactive help text to (internally) redesigned app;~~
- ~~Alter reactivity: replace reactive expressions with observers;~~
- ~~Store all reactive values in one structure;~~
- ~~Write full variable information file for 2015 data;~~
- ~~Write general cleaning function to be sourced from "global.R";~~
- ~~Update census 2013 version (new data cleaning mechanism);~~
- ~~Send .md outlining cleaning strategy to AC;~~
- ~~Write script for data cleaning;~~
- ~~Randomize column order for data samples;~~
- ~~Testing on Windows/Mac OS.~~
