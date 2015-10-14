##  nvd3 testing.
##
## a = oneway(census2013[, 1])$table
## b = data.frame(a)
## labs = colnames(b)
## form = formula(paste(labs[2], '~', labs[1]))
## nPlot(form, data = b, type = "discreteBarChart")
## nPlot(Freq ~ data, data = b, type = "discreteBarChart")
## p1 = nPlot(x = "data", y = "Freq", data = b, type = "discreteBarChart")
## p1$chart(color = c('#5CB8E6', '#E87F5B'))
## p1
## c = twoway(census2013[, 1:2])$table
## d = data.frame(c)
## p2 = nPlot(Freq ~ allergiesdairy, group = "allergieseggs",
##       data = d, type = "multiBarChart")



###  Footer 
footer.html =
    '<div id = "wrapper">
      <div id = "footer">
       <span style = "float:left;">
          <font size="2">Copyright 2015 | All Rights Reserved</font>
       </span>
       <div class="icons" style="float:right; margin: 0 5px;">
         <i class="fa fa-facebook-square fa-2x"></i>
       </div>
       <div class="icons" style="float:right; margin: 0 5px;">
         <i class="fa fa-twitter fa-2x"></i>
       </div>
       <div class="icons" style="float:right; margin: 0 5px;">
         <i class="fa fa-github fa-2x"></i>
       </div>
      </div>
     </div>' 



###  Load R files
load.modules = function(path) {
    filepaths =
        list.files(pattern = "[.]R$",
                   path = path,
                   full.names = TRUE,
                   recursive = TRUE)
    
    invisible(lapply(filepaths, source))
}
