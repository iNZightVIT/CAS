<h4> Census Viewer </h4>
<h6> Last Updated: December 4, 2015. </h6>

<h5> Introduction </h5>
This repository hosts a web-based application that displays a table of counts and an interactive bar chart based on real census data collected from secondary schools around New Zealand. The app can be accessed via the following link: http://docker.stat.auckland.ac.nz/spawn/?application=cas. 

This project is supported by Census At School / Tatauranga Ki Te Kura Aotearoa, which aims to be the first port of call for New Zealand educators seeking information and support for their teaching of statistics. For more information, please visit  http://new.censusatschool.org.nz/ to find a large number of original quality teaching resources including workshops, presentations, classroom activities, research papers, interactive data analysis tools, real student data sets and essential links to other statistics websites. 

<h5> Instructions for Manual Testing </h5>
To run the app *manually* in R, you can either clone the repository or download and extract the .zip file and start an R session in the directory in which the .zip was extracted. You must ensure that the following R packages are installed:
- devtools v1.7.0;
- DT v0.1;
- markdown v0.7.4;
- rCharts v0.4.5;
- RMySQL v0.10.4;
- shiny v0.12.1.
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

Note that the app has only been tested on R versions >= 3.2.0 ("Full of Ingredients"). Any questions, suggestions, or bug reports may be forwarded to Chris Park <cpar137@aucklanduni.ac.nz>.

<h5> Recent Updates </h5>
- [ ] Make it possible to "swap" x and y;
- [ ] Hack table so users can edit it directly;
- [ ] Add screenshots/flash to help.md, incl. adding/removing bars;
- [ ] Add feedback form;
- [ ] Consider adding *Advanced* tab for customized tables/plots;
- [ ] Set appropriate boundary points for numeric variables;
- [ ] Ordering of factor levels - needs rethinking
- [x] Automated update process (DockerHub);
- [x] Change cleaning structure so that less data is read in from db;
- [x] Change table load structure for speed up;
- [x] Fix table resize issue (interactivity lost);
- [x] Add *remove* button for variables;

<h5> Update History </h5>;
- ~~Fix empty character string bug;~~
- ~~Added 2009 and 2011 tables (with recycled variables);~~
- ~~Clean up nvd3 to avoid label overlapping;~~
- ~~Fix known reactivity issues, e.g. when changing "year";~~
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
