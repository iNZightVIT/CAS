###  A general function for cleaning census data sets pulled from the
###  the "stat_censusatschool" database. We use nested scope to avoid
###  namespace clutter, due to the large number of helper functions.
###  
###  Note that helper functions are listed in the order in which they
###  are called by the outer function "clean.data".

clean.data =
    local({
        ##  Creates a database connection object then extract desired table.
        ##  Note that the function that retrieves the table from the data-
        ##  base is wrapped with "suppressWarnings" to hide the resulting
        ##  harmless (but annoying) warning message. Authentication details
        ##  are read in from "config.R", which is created automatically by
        ##  the docker container using "config.sample".
        get.table =
            function(table, query = "SELECT * FROM") {
                source("config.R", local = TRUE)
                db.conn =
                    dbConnect(MySQL(), user = user,
                              password = password,
                              host = host, dbname = dbname)
                
                if (anyNA(match(table, dbListTables(db.conn))))
                    stop(paste0("The table '", table,
                                "' is not in the '", dbname,
                                "' database."))
                else {
                    query = paste(query, table)
                    suppressWarnings(
                        dbGetQuery(conn = db.conn, statement = query))
                }
            }
        
        ##  Extracts columns from a data frame / matrix.
        extract.cols =
            function(cols, data, labels = "variable") {
                labs = data[, labels]
                ii = data[, cols]
                names(ii) = labs
                ii                
            }

        ##  Generate all the labels to be used in the actual CAS app,
        ##  including all the row labels, units, and help text.
        gen.labs =
            function(var.tab, var.col, vars, info) {
                all.cats = var.tab[, var.col[5]]
                grp.cats = unique(all.cats)
                ind.cats = function(x) which(all.cats == x)
                ext.cats = function(x, n, y = info) y[, n][x]
                row.cats = sapply(grp.cats, ind.cats)
                help.texts = lapply(row.cats, function(x) ext.cats(x, 1))
                unit.texts = lapply(row.cats, function(x) ext.cats(x, 2))
                all.labs = lapply(row.cats, function(x) vars[x])
                list(all.labs = all.labs,
                     help.texts = help.texts,
                     unit.texts = unit.texts)                    
            }
        
        ##  Clean character columns.
        clean.char =
            function(census, type, k) {
                check.def =
                    function(x) {
                        ii = which(is.na(x) | is.null(x))
                        jj = which(identical(x, "") | identical(x, "NULL"))
                        unique(sort(c(ii, jj)))
                    }

                ii = apply(census, 2, check.def)
                if (length(ii) > 0) {
                    ii = unique(unlist(ii))
                    census = census[-ii, ]
                    if (ncol(census) < k) {
                        subs = colnames(census)
                        type = type[subs]
                    }
                }
                
                ##  Output
                list(census = census, type = type)
            }        
        
        ##  Clean numeric columns.
        clean.num =
            function(census, type, k) {               
                ##  Handles negative values.
                handle.negs =
                    function(census.num) {
                        force.zero =
                            function(x) {
                                ii = which(x < 0)
                                x[ii] = 0
                                x[!ii] = x
                            }                        
                        ii.negs = apply(census.num, 2, function(x) any(x < 0))
                        if (any(ii.negs)) {
                            which.negs = which(ii.negs)
                            census.num[, which.negs]  =
                                apply(census.num[, which.negs], 2, force.zero)
                        }
                        census.num
                    }
                 
                ##  Handles outliers.
                handle.outs =
                    function(census, census.num, ii.outs) {
                        outs = unique(unlist(ii.outs))
                        census = census[-outs, ]
                        census.num = census.num[-outs, ]

                        ##  Output
                        list(census = census, census.num = census.num)
                    }             
               
                
                ##  Handles doubles. The somewhat unusual indentation is
                ##  to enforce a 80 character limit on each line of code.
                handle.dble =
                    function(census.num) {
                        ##  String handling for long numbers.
                        long.num =
                            function(x) {
                                if (!is.character(x))
                                    x = as.character(x)
                                nx = round(as.numeric(x), 7)
                                if (nx < 10000) 
                                    as.character(round(nx, digits = 7))
                                else {                          
                                    n = nchar(x)
                                    if (n <= 6)
                                        x = paste0(substring(x, 1, 3), "K")
                                    else if (n <= 9) {
                                        m = n - 7
                                        pre = substring(x, 1, m + 1)
                                        suf = substring(x, m + 2, m + 3)
                                        x = paste0(pre, ".", suf, "M")
                                    }
                                    else
                                        stop("x > 100 million")
                                }
                            }
                        ##  Function to truncate doubles.
                        trunc.dbles =
                            function(x, sep = "-",
                                     probs = seq(0, 1, by = .1)) {
                                
                                breaks = unique(quantile(x, probs = probs))
                                cuts = cut(x, breaks = breaks, 
                                    dig.lab = 10, include.lowest = TRUE)
                                lvls = levels(cuts)
                                pattern = "\\]|\\(|\\["
                                lvls =
                                    gsub(",", sep,
                                         gsub(pattern, "", lvls))                
                                if (any(nchar(lvls) > 11)) {
                                    splits =
                                        lapply(lvls,
                                               function(x)
                                                   unlist(strsplit(x, sep)))
                                    splits =
                                        sapply(unlist(splits), long.num)
                                    lefts = c(TRUE, FALSE)
                                    lvls =
                                        paste(splits[lefts],
                                              splits[!lefts], sep = sep)
                                }
                                levels(cuts) = lvls

                                ##  Output
                                list(cuts = cuts, lvls = lvls)
                            }
                        

                        ##  "handle.dble" starts here.
                        int.cols =
                            apply(census.num, 2, function(x) all(round(x) == x))
                        non.cols = !int.cols
                        if (length(non.cols) > 0)
                            census.num[, non.cols] =
                                round(census.num[, non.cols], 1)
                        census.lvl =
                            apply(census.num, 2,
                                  function(x) trunc.dbles(x)$lvls)
                        census.num =
                            apply(census.num, 2,
                                  function(x) trunc.dbles(x)$cuts)

                        ##  Output
                        list(census.num = census.num,
                             census.lvl = census.lvl)
                    }                        

                ## "clean.num" starts here.
                check =
                    function(type, a, b)
                        sapply(type,
                               function(x) identical(x, a) | identical(x, b))
                ii.num = check(type, "float", "integer")
                jj.num = !ii.num
                which.num = which(ii.num)
                census[, jj.num] = apply(census[, jj.num], 2, factor)
                census.num = census[, which.num]
                
                ##  Negatives
                if (length(which.num) > 0) 
                    census.num = handle.negs(census.num)
                
                ##  Outliers
                ii.outs =
                    unlist(apply(census.num, 2,
                                 function(x) which(abs(x) > 5 * mean(x))))
                if (length(ii.outs) > 0) {
                    outs = handle.outs(census, census.num, ii.outs)
                    census = outs$census
                    census.num = outs$census.num
                }                
                
                ##  Doubles
                dbles = handle.dble(census.num)
                census.num = dbles$census.num
                census.lvl = dbles$census.lvl

                census[, which.num] = census.num
                if (ncol(census) < k) {
                    subs = colnames(census)
                    type = type[subs]
                }

                ##  Output
                list(census = census,
                     type = type,
                     lvl = census.lvl)
            }       
        
        ##  Outer function 
        function(data, vars) {
            ##  Extract data and variable tables.
            dat.tab = get.table(table = data)
            var.tab = get.table(table = vars)

            ##  Variable handling.
            var.col = c("variable", "question", "unit", "type", "category")
            if (anyNA(match(var.col, colnames(var.tab))))
                stop("There was a problem with accessing the variable table.")

            ##  We extract the columns we need from the variable information
            ##  spreadsheet and store them as a character matrix. Note that
            ##  we pull the third column and store it separately as a vector
            ##  named "type" later on, since this is repeatedly updated by
            ##  subsequent calls to helper functions.
            vars = extract.cols(var.col[1], var.tab, label = "label")
            info =
                sapply(var.col[2:4],
                       function(x) extract.cols(x, data = var.tab))
            type = info[, 3]
            
            ##  Extract the desired columns (provided by "vars"), then check
            ##  if all the column names actually match.
            census = dat.tab[, vars]
            k = ncol(census)
            if (anyNA(match(vars, colnames(census))))
                stop("Column name mismatch!")
            
            ##  Generate all the labels, including row labels, unit info, as
            ##  well as help texts displayed above graphical output in the
            ##  output panel of the CAS app.
            labels = gen.labs(var.tab, var.col, vars, info)

            ##  Clean character values. Note that "type" is updated here since
            ##  some columns may have been removed during the cleaning process.
            chars = clean.char(census, type, k)
            type = chars$type
            census = chars$census
            
            ##  Clean numeric values. Note that "type" is updated here since
            ##  some columns may have been removed during the cleaning process.
            nums = clean.num(census, type, k)

            ##  Output
            list(data = nums$census,
                 help = labels$help.texts,
                 labs = labels$all.labs,
                 lvls = nums$lvl,
                 type = nums$type,
                 unit = labels$unit.texts)    
        }
        
    })

###  Call.
data = "response2015"
vars = "var2015"
census2015 = clean.data(data, vars)

data = "response2013"
vars = "var2013"
census2013 = clean.data(data, vars)

