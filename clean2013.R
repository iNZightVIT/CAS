###  ----------------------------------------------------------------------
###  (Automated) Data Cleaning Example
###
###  Last Modified: July 27, 2015.
###  ----------------------------------------------------------------------
###
###  This file consists of a collection of R function that automatically
###  cleans the data, given a CSV file of variable information.
###
###  The file to be cleaned ("data2013.csv") is a small subset of
###  the full 2013 census data, consisting only of responses to questions
###  corresponding to two categories ("Ethnic" and "Gender").

###  First, we read in the data. 
census2013 = read.csv("data2013.csv")[, -1]

###  We coerce the data into a character matrix, since data frames are
###  rather clunky structures that eat up a lot of memory.
census2013 = as.matrix(census2013)

###  Next, we read in the variable information from "var2015.csv".
varInfo = as.matrix(read.csv("var2013.csv"))

get.var =
    function(col, lab.ind = "label", data = varInfo) {
        labs = data[, lab.ind]
        ii = data[, col]
        names(ii) = labs
        ii
    }

vars = get.var("variable")
ques = get.var("question", lab.ind = "variable")
unit = get.var("unit", lab.ind = "variable")
type = get.var("type", lab.ind = "variable")

census2013 = census2013[, vars]

###  We check that the variable names are valid.
all(vars %in% colnames(census2013))

###  Having checked the validity of the variable names, we proceed to create
###  a list of labels for each category.
cats = unique(varInfo[, "category"])

###  We extract the rows that match to each category.
cat.rows = sapply(cats, function(x) which(varInfo[, "category"] == x))
all.labs = lapply(cat.rows, function(x) vars[x])

###  Next, we do the same thing to extract the question and units.
help.texts = lapply(cat.rows, function(x) ques[x])
unit.texts = lapply(cat.rows, function(x) unit[x])

rm(vars, unit, ques)

###  Next, we check for improper values in the data set.
clean = function(x) which(is.na(x) | is.null(x) | x == "" | x == "NULL")
ii = apply(census2013, 2, clean)
if (length(ii) > 0) {
    jj = sort(unique(unlist(ii)))
    census2013 = census2013[-jj, ]
}

###  Handle numeric columns.
ii.num =
    suppressWarnings(
        apply(census2013, 2, function(x) any(!is.na(as.numeric(x)))))


all(which(ii.num) == which(type == "float" | type == "integer"))

if (any(ii.num)) {
    which.num = which(ii.num)
    census2013 = data.frame(census2013)
    census2013[, which.num] = apply(census2013[, which.num], 2, as.numeric)
    
    ##  Clean numeric data.
    ii.zero =
        apply(census2013[, which.num], 2, function(x) any(x < 0))
    
    if (any(ii.zero)) {
        which.zero = which(ii.zero)
        census2013[, which.zero] =
            apply(census2013[, which.zero], 2,
                  function(x) {
                      ii = which(x < 0)
                      x[ii] = 0
                      x[!ii] = x
                  })
    }
    ##  Truncate numeric variables.
    census2013[, which.num] =
        lapply(census2013[, which.num], as.integer)
    
    truncate =
        function(x) {
            x.breaks = unique(as.integer(seq(0, max(x), length = 6)))
            cuts =
                cut(x, breaks = x.breaks, dig.lab = 10,
                    include.lowest = TRUE)
            lvls = levels(cuts)
            pattern = "^\\[|^\\(|\\]$|\\)$"
            clean = gsub(",", "-", gsub(pattern, "", lvls))
            levels(cuts) = clean
            cuts
        }
    
    census2013[, which.num] = 
        lapply(census2013[, which.num], truncate)
}


###  Finally, we save the cleaned data set as an Rda for use in
###  the CaS shiny app.
census2013 = data.frame(census2013)









