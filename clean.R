###  Extract table from DB.
get.table =
  function(table, query = "SELECT * FROM") {
    source("config.R", local = TRUE)
    db.conn = dbConnect(MySQL(), user = user,
                        password = password,
                        host = host, dbname = dbname)    
    if (anyNA(match(table, dbListTables(db.conn)))) {
      stop(paste0("The table '", table, "' is not in the '",
                  dbname, "' database."))
    } else {
      query = paste(query, table)
      suppressWarnings(dbGetQuery(conn = db.conn, statement = query))
    }
   
  }

###  Extract columns.
get.cols =
  function(cols, data, label = "variable") {
    x = data[, cols]
    names(x) = data[, label]
    x
  }

###  Generate labels.
gen.labs =
  function(tab, col, avars, info) {
    vars = tab[, col[5]]
    grps = unique(vars)
    rows = sapply(grps, function(i) which(vars == i))
    inds = function(i, n, x = info) x[, n][i]
    list(all.labs   = lapply(rows, function(i) avars[i]),
         help.texts = lapply(rows, function(i) inds(i, 1)),
         unit.texts = lapply(rows, function(i) inds(i, 2)))
  }

###  Concatenate large numbers.
conc.str =
  function(str, cond, unit) {
    ii = which(cond)
    if (identical(unit, "K"))
      m = 1
    else if (identical(unit, "M"))
      m = 4
    else
      stop("Unrecognized unit.")    
    raw = strsplit(str[ii], "")
    proc = lapply(raw,
                  function(x) {
                    n = length(x)
                    c(x[-((n - m):n)], unit)
                  })
    post = sapply(proc,
                  function(x) {
                    n = length(x)
                    y1 = paste(x[1:(n - 2)], collapse = "")
                    y2 = paste(x[(n - 1):n], collapse = "")
                    paste(y1, y2, sep = ".")
                  })
    str[ii] = post
    unlist(str)
  }

###  Clean missing values.
clean.nas =
  function(data, type) {
#    ind = apply(data, 2,
#                function(x) {
#                  unique(sort(c(which(is.na(x) | is.null(x)),
#                                which(x == ""  | x == "NULL"))))
#                })
#    if (length(ind) > 0) {
#      subs = data[-unique(unlist(ind)), ]
#      if (ncol(subs) < ncol(data))
#        type = type[colnames(subs)]
#      data = subs
#    }
    list(data = data, type = type)
  }

###  Clean numeric values.
clean.num =
  function(data, type) {
    ##  Sort through the data.
    nums = sapply(type, function(x) identical(x, "float") |
                                    identical(x, "integer"))
    inum = which(nums)
    
    ##  Coerce non-numeric columns to be a factor.
    data[, !inum] = apply(data[, !inum], 2, as.factor)
    
    ##  Clean numeric columns within the data.
    if (length(inum) > 0) {
      subs = data[, inum]
      
      ##  Handle negative counts.
      negs = apply(subs, 2, function(x) any(x < 0))
      if (any(negs)) {
        inds = which(negs)
        subs[, inds] = apply(subs[, inds], 2,
                             function(x) {
                               ii = which(x < 0)
                               x[ii] = 0
                               x[!ii] = x
                             })        
      }
      
      ##  Handle outliers.
#      outs = apply(subs, 2, function(x) any(abs(x) > 5 * mean(x))) 
#      if (any(outs)) {
#        inds = unique(unlist(which(outs)))
#        subs = subs[-inds, ]
#        data = data[-inds, ]
#      }
      
      ##  Clean decimal places.
      ints = apply(subs, 2, function(x) all(round(x) == x, na.rm = T))
      if (!all(ints)) 
        subs[, !ints] = round(subs[, !ints], 1)

      ##  Generate factor levels.
      options(scipen = 20)

      ##  Convert matrix into a list to hold factors.
      nc = ncol(subs)
      flist = vector("list", nc)
      flist = lapply(1:nc, function(i) flist[[i]] = subs[, i])      
      cuts = lapply(flist,                   
                    function(x) {                      
                      brks = unique(floor(hist(x, plot = FALSE)$breaks))
                      step = diff(brks)
                      if (identical(length(unique(step)), 1L))
                        brks = c(brks, brks[length(brks)] + step[1])
                      cuts = cut(x, breaks = brks, dig.lab = 10,
                                 include.lowest = TRUE, right = FALSE)
                    })

      #if (any(unlist(lapply(cuts, anyNA))))
      #  stop("There has been an error in the binning process.")
      
      lvls = lapply(cuts,
                    function(x) {
                      lvls = levels(x)
                      raws = gsub("\\[|\\]|\\)", "", lvls)
                      ends = strsplit(raws, ",")
                      enda = unlist(lapply(ends, function(x) x[1]))
                      endb = unlist(lapply(ends, function(x) x[2]))
                      if (is.null(as.numeric(enda)) ||
                          is.null(as.numeric(endb)))
                        stop("Boundary error.")
                      numa = as.numeric(enda) 
                      numb = as.numeric(endb)
                      if (any(numa >= 1e+3)) {
                        enda = conc.str(str = enda, unit = "K",
                                        cond = numa >= 1e+3 &
                                               numa < 1e+6)
                        endb = conc.str(str = endb, unit = "K",
                                        cond = numb >= 1e+3 &
                                               numb < 1e+6)
                        if (any(numa >= 1e+6)) {
                         enda = conc.str(str = enda, unit = "M",
                                         cond = numa >= 1e+6)
                         endb = conc.str(str = endb, unit = "M",
                                         cond = numb >= 1e+6)       
                       }                    
                      }                     
                      out = paste0("[", enda, ", ", endb, ")")
                      n = length(out)
                      c(out[-n], gsub("\\)", "\\]", out[n]))
                   })      
    } else {
      cuts = type = lvls = NULL
    }
    
    ##  Seems odd to perform the coercion now, but this saves
    ##  vast amounts of copying...
    data = as.data.frame(data)
    
    ##  Append to full data set.
    if (identical(nc, length(inum)))
      for (i in 1:nc) 
        data[, inum[i]] = cuts[[i]]
    else
      stop("Length mismatch.")      
    
    ##  Return output.
    list(data = data, type = type, lvls = lvls)      
  }

###  Clean dataset.
clean.data =
  function(data, cols) {
    ##  Query database and variable file.
    require(RMySQL)
    tab = read.csv("alldata-aug2019.csv")
  
    ## tab is data, var is the excel file
    tab = get.table(table = data)
    var = as.matrix(read.csv("var2019.csv"))
    
    ##  Handle variables.
    vlabs = c("variable", "question", "unit", "type", "category")
    if (anyNA(match(vlabs, colnames(var))))
      stop("There was a problem with accessing the table.")
    
    ##  Extract relevant parameters.
    vars = get.cols(vlabs[1], var, label = "label")
    info = sapply(vlabs[2:4], function(x) get.cols(x, data = var))
    type = info[, 3]
    
    ##  Extract desired columns.
    subs = tab[, vars]
    if (anyNA(match(vars, colnames(subs))))
      stop("Columns don't seem to match.")

    ##  Generate labels.
    labs = gen.labs(var, vlabs, vars, info)

    ##  Clean missing values.
    clean = clean.nas(subs, type)
    type = clean$type
    subs = clean$data
    #data = subs
    ##  Clean numeric columns.
    clean = clean.num(subs, type)
    
    ##  Clean up on exit.
    on.exit(lapply(dbListConnections(MySQL()), dbDisconnect))
    
    ##  Return output.
    list(data = clean$data,
         lvls = clean$lvls,
         type = clean$type,
         labs = labs$all.labs,
         help = labs$help.texts,
         unit = labs$unit.texts)    
  }

###  Clean.
census2015 = clean.data("response2015", "var2015.csv")
census2013 = clean.data("response2013", "var2013.csv")
census2011 = clean.data("response2011tidy", "var2011.csv")
census2009 = clean.data("response2009tidy3", "var2009.csv")

###  Save time.
all = list(census2015, census2013, census2011, census2009)
lab = paste0(seq(2015, 2009, by = -2), ".Rda")
n = length(all)

###  Ensure that parameters match.
all(unlist(
  lapply(1:n, function(i) with(all[[i]], length(type) == ncol(data)))))

###  Ensure that there are no missing values.
all(!unlist(lapply(1:n, function(i) apply(all[[i]]$data, 2, anyNA))))

###  Save.
invisible(
  lapply(1:n,
         function(i) 
           lapply(all[i],
                  function(x)
                    saveRDS(x, file = paste0("./data/", lab[i])))))

## census2015 = readRDS("2015.Rda")
## census2013 = readRDS("2013.Rda")
## census2011 = readRDS("2011.Rda")
## census2009 = readRDS("2009.Rda")


result = list(data = data,
              lvls = lvls,
              type = type,
              labs = labs$all.labs,
              help = labs$help.texts,
              unit = labs$unit.texts)
saveRDS(result, "2019.Rda")
