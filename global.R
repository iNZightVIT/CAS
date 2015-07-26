###------------------------------###
###  Global Functions / Vectors  ###
###------------------------------###
###
###  Read in data

###  Load labels
source("clean2013.R")

###  One way frequency table
oneway.freq = 
    function(data, labs = NULL) {
        ##  Tabulate
        data.table = table(factor(data))
        data.df1 = data.frame(data.table)
        
        ##  Extract Sums, Percentages
        sum.freq = sum(data.table)
        perc.tab = round(data.table / sum.freq * 100, 2)
        prop.tab = paste0(perc.tab, "%")
        Total = c(sum.freq, "100%")
        
        ##  Matrix
        data.mat =
            rbind(cbind(
                "Totals" = data.table,
                "Percentages" = prop.tab), "Totals" = Total)
        
        ##  Output
        data.df = data.frame(data.mat)
        if (!is.null(labs)) {
            colnames(data.df) =
                paste0(labs, ": ", colnames(data.df))
            rownames(data.df) =
                paste0(labs, ": ", rownames(data.df))
        }
        list(data = data.df, alt.data = data.df1)
    }

###  Two way frequency table
twoway.freq =
    function(data, labs = NULL) {
        
        ##  Tabulate
        data.table = table(data)
        sum.table = addmargins(data.table, c(1, 2))
        data.df1 = data.frame(data.table)
        
        ##  Extract Sums
        row.sums = rowSums(data.table)
        col.sums = colSums(data.table)
        tot.sums = sum(data.table)

        ##  Compute percentages
        row.perc = round(row.sums / tot.sums * 100, 2)
        col.perc = round(col.sums / tot.sums * 100, 2)
        row.text = paste0(row.perc, "%")
        col.text = paste0(col.perc, "%")
               
        ##  Matrix
        data.mat1 = 
            cbind(data.table, "Totals" = row.sums)
        data.mat2 =
            cbind(data.mat1, "Percentages" = row.text)
        data.mat3 =
            rbind(data.mat2, c(col.sums, tot.sums, "100%"))
        data.mat4 =
            rbind(data.mat3, c(col.text, "100%", ""))
        n = nrow(data.mat4)
        rownames(data.mat4)[(n-1):n] = c("Totals", "Percentages")
        
        ##  Output
        data.df2 = as.data.frame(data.mat4)
        cols = colnames(data.df2)
        rows = rownames(data.df2)
        if (!is.null(labs)) {
            rownames(data.df2) = paste0(labs[1], ": ", rows)
            colnames(data.df2) = paste0(labs[2], ": ", cols)
        }
        list(data = data.df2, alt.data = data.df1,
             table = data.table, sum.table = sum.table)
    }


###  Load Module R files
load.modules = function(path) {
    filepaths =
        list.files(pattern = "[.]R$",
                   path = path,
                   full.names = TRUE,
                   recursive = TRUE)
    
    invisible(lapply(filepaths, source))
}

###  Selectize Input
selectize.input =
    function(inputId, label = NULL, choices, options = NULL,
             multiple = FALSE) {
             ## , width = "175px") {
        
        ##  Select Input
        selectizeInput(inputId = inputId,
                       label = label,
                       choices = choices,
                       multiple = multiple,
                       ## width = width,
                       options = options)
    }

###  dateRangeInput/numericInput wrapper
dateNum.input =
    function(inputId, start = NULL, end = NULL, format = "yyyy",
             startview = "year", value = 1, min = NULL, max = NULL,
             label = NULL, date = TRUE, numeric = !date) {
         
        ##  Diagnostic Check
        stopifnot(is.character(inputId), is.numeric(value),
                  is.null(start) || is.numeric(start),
                  is.null(end) || is.numeric(end),
                  is.character(format), is.character(startview),
                  is.null(min) || is.numeric(min),
                  is.null(max) || is.numeric(max),
                  is.null(label) || is.character(label),
                  is.logical(date) && is.logical(numeric))
         
        ##  Date Input
        if (date) 
            dateRangeInput(inputId = inputId,
                           label = label,
                           start = start,
                           end = end,
                           format = format,
                           startview = startview)
        
        ##  Numeric Input
        else
            numericInput(inputId = inputId,
                         label = label,
                         value = value,
                         min = min,
                         max = max)
    }


###  Arguments for tabPanel
tab.args =
    function(title = "", iname, iclass = "fa-2x") {
        
        ##  Diagnostic Check
        stopifnot(is.character(title),
                  is.character(iname),
                  is.character(iclass))

        ##  Title and Icon
        title = title
        icon = icon(name = iname, class = iclass)
    }


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


## a = oneway.freq(census2013[, 1])$table
## b = data.frame(a)
## labs = colnames(b)
## form = formula(paste(labs[2], '~', labs[1]))
## nPlot(form, data = b, type = "discreteBarChart")


## nPlot(Freq ~ data, data = b, type = "discreteBarChart")
## p1 = nPlot(x = "data", y = "Freq", data = b, type = "discreteBarChart")
## p1$chart(color = c('#5CB8E6', '#E87F5B'))
## p1

## c = twoway.freq(census2013[, 1:2])$table
## d = data.frame(c)

## p2 = nPlot(Freq ~ allergiesdairy, group = "allergieseggs",
##       data = d, type = "multiBarChart")

