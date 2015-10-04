###  Utility Functions 
###
###  Last Modified  :  October 3, 2015.

source("clean.R")

###  Function for handling factors.
set.factor =
    function(data, var, lvls, type) {
        if (any(var %in% colnames(lvls))) {
            if (identical(length(var), 1L)) {
                ii = which(var == colnames(lvls))
                if (type == "categorical") 
                    lvls = unique(data)                    
                as.factor(data, levels = lvls[, ii])
            }
            else {
                for (i in 1:length(var)) {
                    ii = which(var[i] == colnames(lvls))
                    if (length(ii > 0)) {
                        if (type[i] == "categorical") 
                            lvls = unique(data[, var[i]])
                        data[, var[i]] = as.factor(data[, var[i]],
                                levels = lvls[, ii])
                    }
                }
                data
            }
        }
        else
            if (length(var) == 1) 
                as.factor(data)
            else
                data
    }

###  One way frequency table
oneway = 
    function(data, var, lvls, type, labs = NULL) {
        ##  Set factor
        data.factor = set.factor(data, var, lvls, type)
        
        ##  Tabulate
        data.table = table(data.factor)
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
twoway =
    function(data, var, lvls, type, labs = NULL) {

        ##  Set factor
        data = set.factor(data, var, lvls, type)
        
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
    function(inputId, label = NULL, choices,
             options = NULL, multiple = FALSE) {
             ## , width = "175px") {
        
        ##  Select Input
        selectizeInput(inputId = inputId,
                       label = label,
                       choices = choices,
                       multiple = multiple,
                       selected = 1,
                       ## width = width,
                       options = options)
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


###  Function for pasting.
name.func = function(x, y, sep = ": ") paste0(x, sep, y)

###  Functions for help texts.
add.title = function(a) h4(a)
add.text1 = function(a, b) h5(paste0(a, b))
add.text2 = function(a, b, c) h5(paste0(a, b, " (", c, ")"))

###  Function for displaying page help.
###
###  We form a closure to encase the helper functions to ensure
###  that they are only visible in the local scope. "modal.help"
###  is returned as the value of the call the "local".
modal.help =
    local({
        modal.head =
            function(button, title) {
                paste0(
                    paste0('<!-- Trigger the modal with a button -->
                           <button type="button" 
                                   class="btn btn-xs btn-primary"
                                   data-toggle="modal" 
                                   data-target="#help">',
                           button, '</button>'),
                    paste0('<!-- Modal -->
                            <div id="help" class="modal fade" role="dialog">
                               <div class="modal-dialog">
                                    <!-- Modal content-->
                                    <div class="modal-content">
                                      <div class="modal-header">
                                        <button type="button" 
                                                class="close" 
                                                data-dismiss="modal">
                                          &times;
                                        </button>
                                        <h4 class="modal-title">',
                           title,'</h4> </div>'))
            }
        
        modal.tail =
            function(file, sheet) {
                paste0('<div class="modal-body">',                       
                       markdownToHTML(
                           file = file,
                           options = "",
                           stylesheet = sheet), '</div>',
                       '<div class="modal-footer">
                        <button type="button" 
                                class="btn btn-xs btn-primary" 
                                data-dismiss="modal">
                           Close
                        </button>
                      </div> </div> </div> </div>')
            }

        function(button, title, file = "help.md", sheet = "www/cosmo.css") 
            HTML(paste0(modal.head(button, title), modal.tail(file, sheet)))
    })

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
