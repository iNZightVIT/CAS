###  Global Functions / Values 
###
###  Last Modified  :  July 29, 2015.

###  Global parameters
col1 = "#5CB8E6"
col2 = "#F5F5F5"
exts = c("Scroller", "ColReorder")
sample.sizes = c("All", "5000", "2000", "1000")
options(RCHART_LIB = "nvd3")


###  Function for handling factors.
set.factor =
    function(data, var, lvls) {
        if (any(var %in% colnames(lvls))) {
            if (identical(length(var), 1L)) {
                ii = which(var == colnames(lvls))
                factor(data, levels = lvls[, ii])
            } else {
                for (i in 1:length(var)) {
                    ii = which(var[i] == colnames(lvls))
                    if (length(ii > 0))
                        data[, var[i]] = factor(data[, var[i]],
                                levels = lvls[, ii])
                }
                data
            }
        }
        else
            if (length(var) == 1) 
                factor(data)
            else
                data
    }

###  One way frequency table
oneway = 
    function(data, var, lvls, labs = NULL) {
        ##  Set factor
        data.factor = set.factor(data, var, lvls)
        
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
    function(data, var, lvls, labs = NULL) {
        ##  Set factor
        data = set.factor(data, var, lvls)
        
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

###  Cleaning Functions
clean.data =
    function(data, var) {
        ##  Input check
        stopifnot(is.character(data), is.character(var))
        
        ##  Function to extract columns.
        get.var =
            function(col, lab.ind = "label", data = varInfo) {
                labs = data[, lab.ind]
                ii = data[, col]
                names(ii) = labs
                ii
            }
        
        ##  Function to truncate numeric variables.
        truncate =
            function(x) {
                x.breaks =
                    unique(as.integer(seq(0, max(x), length = 6)))
                cuts =
                    cut(x, breaks = x.breaks, dig.lab = 10,
                        include.lowest = TRUE)
                lvls = levels(cuts)
                pattern = "^\\[|^\\(|\\]$|\\)$"
                clean = gsub(",", "-", gsub(pattern, "", lvls))
                levels(cuts) = clean
                list(cuts = cuts,
                     lvls = clean)
            }

        ##  Read variable info file and extract relevant columns.
        varInfo = as.matrix(read.csv(var))
        vars = get.var("variable")
        ques = get.var("question", lab.ind = "variable")
        unit = get.var("unit", lab.ind = "variable")
        type = get.var("type", lab.ind = "variable")

        ##  Read in relevant data.
        censusData = read.csv(data)[, vars]

        ##  Column check.
        if (!all(vars %in% colnames(censusData)))
            stop("Column names don't match!")

        ##  Label generation.
        cats = unique(varInfo[, "category"])
        cat.func = function(x) which(varInfo[, "category"] == x)
        cat.rows = sapply(cats, cat.func)
        all.labs = lapply(cat.rows, function(x) vars[x])

        ##  Help and unit strings.
        help.texts = lapply(cat.rows, function(x) ques[x])
        unit.texts = lapply(cat.rows, function(x) unit[x])

        ##  Data cleaning
        clean = function(x) which(is.na(x) | is.null(x) |
                                   x == "" | x == "NULL")
        ii = apply(censusData, 2, clean)
        if (length(ii) > 0) {
            jj = sort(unique(unlist(ii)))
            censusData = censusData[-jj, ]
        }
        
        ##  Handle numeric columns
        which.num = which(type == "float" | type == "integer")
        if (any(which.num)) {
            census.df = data.frame(censusData)
            census.num = census.df[, which.num]
            census.num = apply(census.num, 2, as.numeric)
            
            ##  Wipe negative values.
            fn.zero = function(x) any(x < 0)
            ii.zero = apply(census.num, 2, fn.zero)
            if (any(ii.zero)) {
                which.zero = which(ii.zero)
                census.num[, which.zero] =
                    apply(census.num[, which.zero], 2,
                          function(x) {
                              ii = which(x < 0)
                              x[ii] = 0
                              x[!ii] = x
                          })
            }

            ##  Coerce into integer then truncate.
            int.trunc = function(x) truncate(as.integer(x))$cuts
            int.level = function(x) truncate(as.integer(x))$lvls
            census.lvl = apply(census.num, 2, int.level)
            census.num = apply(census.num, 2, int.trunc)
                     
            ##  Replace.
            census.df[, which.num] = census.num
        }
        ##  Return.
        list(data = census.df,
             labs = all.labs,
             help = help.texts,
             unit = unit.texts,
             lvls = census.lvl)
    }           

###  Clean data
census2013 = clean.data("data2013.csv", "var2013.csv")
census2015 = clean.data("data2015.csv", "var2015.csv")

###  Function for pasting.
name.func = function(x, y, sep = ": ") paste0(x, sep, y)

###  Functions for help texts.
add.title = function(a) h4(a)
add.text1 = function(a, b) h5(paste0(a, b))
add.text2 = function(a, b, c) h5(paste0(a, b, " (", c, ")"))

##  nvd3
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

