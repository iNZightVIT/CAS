###  Census Viewer
###
###  Last Modified  :  September 26, 2015.

###  Load the required packages and utility functions.
library(DT)
library(markdown)
library(rCharts)
library(shiny)
source("global.R")

###  Set global parameters.
col1 = "#5CB8E6"
col2 = "#F5F5F5"
exts = c("Scroller", "ColReorder")
sample.sizes = c("All", "5000", "2000", "1000")
options(RCHART_LIB = "nvd3", shiny.deprecation.messages = FALSE)

###  Set up the server function. Note use of the explicit definition of
###  the "session" argument for the download handler.
server =
    function(input, output, session) {        
        ##  Dynamic container in which to store reactive values.
        vals = reactiveValues()

        ##  Basic input check.
        check.input =
            function(input) {
                wipe = function(x) !(is.na(x) & is.null(x))
                if (length(input) > 0)
                    all(sapply(input, wipe))
                else
                    FALSE
            }
        
        ##  Set reactive values and set up data switch mechanism.
        ##  Further data to be added, as appropriate.
        observe({
            input$data
            if (check.input(input$data)) {
                inputData =
                    switch(input$data,
                           "2015" = census2015,
                           "2013" = census2013)
                vals$all.labs = inputData$labs
                vals$data = inputData$data
                vals$help = inputData$help
                vals$unit = inputData$unit
                vals$lvls = inputData$lvls
                vals$type = inputData$type
                vals$nrow = nrow(inputData$data)
                vals$xlsx = inputData$data[, input$vars]
            }
        })
        
        ##  Variable handler.
        observe({
            input$vars
            if (check.input(input$vars)) 
                vals$vars = input$vars            
            else 
                vals$vars = ""            
        })
        
        ##  Set data, groups, labels, and frequencies.
        observe({
            input$vars
            input$sample
            
            ##  Sampling.
            if (check.input(input$sample)) {
                if (identical(input$sample, "All"))
                    vals$samp =
                        sample(1:vals$nrow, vals$nrow)
                else
                    vals$samp =
                        sample(1:vals$nrow,
                               as.numeric(input$sample))
                vals$subs = vals$data[vals$samp, ]
            }

            ##  Grouping.
            isolate({
                if (check.input(input$vars) & all(vals$vars != "")) {
                    n = length(input$vars)
                    g.buf = l.buf = character(n)
                    x.any = function(x, y = input$vars) any(x == y[i])
                    for (i in seq_len(n)) {
                        var = sapply(vals$all.labs, x.any)
                        grp = names(which(var))
                        sub = vals$all.labs[[grp]]
                        ind = which(sub == input$vars[i])
                        lab = names(sub)[[ind]]
                        g.buf[i] = grp
                        l.buf[i] = lab
                    }
                    
                    ##  Assignment and subsetting.
                    vals$grps = g.buf
                    vals$labs = l.buf
                    vals$cols = vals$subs[, input$vars]
                    vals$lvls = vals$lvls[input$vars]
                    vals$type = vals$type[input$type]
                    
                    ##  Determine frequency form. Note the use of the explicit
                    ##  integer suffix so we can match the length of input$vars
                    ##  exactly. The 4 byte save in memory is a bonus (at the
                    ##  expense of readability of course).
                    if (identical(length(input$vars), 1L)) 
                        vals$freq = oneway(vals$cols, input$vars,
                            vals$lvls, vals$type, vals$labs)
                    else                     
                        vals$freq = twoway(vals$cols, input$vars,
                            vals$lvls, vals$type, vals$labs)
                }
            })
        })
        
        
        ##  Variable selection
        output$vars = renderUI({
            selectizeInput(inputId = "vars",
                           label = NULL,
                           multiple = TRUE,
                           ## selected = "gender",
                           options = list(maxItems = 2),
                           choices = vals$all.labs)
        })        
        
        ##  Text
        output$text1 = renderText({
            length(vals$samp)
        })
        
        ##  Table
        ##
        ##  While the code looks somewhat complex, it is simply a list of
        ##  arguments to a single function ("renderDataTable") to customize
        ##  the way it displays data. The code wrapped by the "JS" function
        ##  and in quotes is written in Javascript.
        ##
        ##  The empty string in the first argument of "formatStyle" is a dirty
        ##  hack to force the table to look like a frequency table, as opposed
        ##  to a table of raw data (exploiting the fact that the (1, 1)-th cell
        ##  has no name, i.e. the name is empty). Feel free to change this
        ##  if you can come up with a better way...
        output$table = renderDataTable({
            validate(need(try(length(input$vars) >= 1), ""))
            DT::datatable(
                vals$freq$data,
                class = "cell-border compact stripe",
                style = "bootstrap",
                extensions = exts,
                rownames = TRUE,
                selection = "none",
                options = list(
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css(
                        {'background-color': '#F5F5F5',
                         'color': '#4A4A4A',
                         'font-weight': 'bold'});",
                        "}"),                    
                    pageLength = 3,
                    autowidth = FALSE,
                    deferRender = TRUE,
                    scrollCollapse = TRUE,
                    scrollY = 400,
                    colReorder = list(realtime = TRUE),
                    dom = 'T<"clear">rRtS')
                ) %>%                
                formatStyle(' ',
                            color = '#4A4A4A',
                            backgroundColor = '#F5F5F5',
                            fontWeight = 'bold')
        })
        
        ##  Plot
        output$barplot = renderChart({
            
            ##  Basic input check.
            freq = vals$freq$alt.data
            validate(need(try(length(input$vars) >= 1), ""))
            validate(need(try(!is.null(freq)), ""))

            ##  Assign
            name = names(freq)
            labs = vals$labs                        
            
            ##  Loop through 'nvar' to assign appropriate names.
            nvar = length(input$vars)
            for (i in seq_len(nvar)) {
                if (ncol(freq) < 7 & nchar(labs[i] < 12))
                    freq[, i] = name.func(labs[i], freq[, i])
                else
                    freq[, i] = name.func(substring(labs[i], 1, 11),
                            freq[, i])
            }
            
            ##  Set formula.
            form = formula(name.func(name[nvar + 1], name[1], " ~ "))
            
            ##  Draw plots.
            ##
            ##  A dirty hack is used here to force the plot to fit in the 
            ##  plot window (since it occupies the whole screen by default).
            ##  It is set to take on the width of an "empty plot" of height
            ##  zero (but suitable width).            
            plot.width = session$clientData[["output_plot1_width"]]

            ##  Please forgive me for the L's.
            if (identical(nvar, 1L)) {
                p1 <<- nPlot(form,
                             data = freq,
                             type = "discreteBarChart",
                             width = plot.width)
                p1$yAxis(axisLabel = "Frequency")
                p1$addParams(dom = "barplot")
                p1
            } else {
                p2 <<- nPlot(form,
                             data = freq,
                             group = name[2],
                             type = "multiBarChart",
                             width = plot.width)
                p2$yAxis(axisLabel = "Frequency")
                p2$addParams(dom = "barplot")
                p2
            }
        })

        ##  Help Text
        ##
        ##  This one's a bit of a beast...You can re-do the whole thing if you
        ##  feel strongly about it. At the end of the day, all it does is
        ##  display appropriate text above either the plot or table.
        help.text =
            function(x, tab = TRUE) {
                ##  Set up appropriate descriptions.
                text = ifelse(tab, "table.", "plot.")
                desc1 = ifelse(tab, "One-way Table of Counts", "One-variable Bar Plot")
                desc2 = ifelse(tab, "Two-way Table of Counts", "Two-variable Bar Plot")
                
                ##  Check 1.
                validate(
                    need(try(length(x) >= 1),
                         paste0("\nPlease select a variable to create a ", text)))

                ##  Extract parameters.
                unit = vals$unit
                help = vals$help
                grps = vals$grps
                labs = vals$labs
                vars = input$vars
                
                ##  Check 2.
                pars = list(unit, help, grps, labs, vars)
                appl = lapply(pars, function(x) !is.null(x))

                ##  Begin adding help text.
                if (all(unlist(appl))) {
                    ##  Add text
                    if (identical(length(input$vars), 1L)) {
                        title = add.title(desc1)
                        quest = add.text1("Question: ", help[[grps]][vars])
                        vals$group = add.text1("Category: ", grps)
                        vals$units = add.text2("Variable: ", labs,
                            unit[[grps]][vars])
                        HTML(paste(title, "<hr>",
                                   quest, vals$group, vals$units), "<br>")
                    } else {
                        title = add.title(desc2)
                        quest1 = add.text1("Question 1: ",
                            help[[grps[1]]][vars[1]])
                        vals$group1 = add.text1("Category 1: ", grps[1])
                        vals$units1 = add.text2("Variable 1: ", labs[1],
                            unit[[grps[1]]][vars[1]])
                        quest2 = add.text1("Question 2: ",
                            help[[grps[2]]][vars[2]])
                        vals$group2 = add.text1("Category 2: ", grps[2])
                        vals$units2 = add.text2("Variable 2: ", labs[2],
                            unit[[grps[2]]][vars[2]])
                        HTML(paste(title, "<hr>",
                                   quest1, vals$group1, vals$units1, "<br>",
                                   quest2, vals$group2, vals$units2, "<br>"))
                    }
                }
            }
        
        ##  Table Help
        output$tableHelp = renderUI({
            input$vars
            help.text(input$vars)
        })

        ##  Plot Help
        output$plotHelp = renderUI({
            input$vars
            help.text(input$vars, tab = FALSE)
        })                

        ##  Edit column name for the single-variable xlsx table.
        observe({
            input$vars
            if (identical(length(input$vars), 1L)) {
                vals$xlsx = cbind(vals$xlsx)
                colnames(vals$xlsx) = input$vars
            }
        })
        
        ##  Download        
        output$downloadFile = downloadHandler(            
            filename = function(x) paste0(input$data, ".csv"),
            content = function(file) write.csv(vals$xlsx, file, row.names = FALSE))
    }

##  Set up the user interface. This part is trivial.
ui =
    fluidPage(
        titlePanel("Census Viewer"),
        theme = "cosmo.css",
        responsive = FALSE,
        column(width = 2,
               br(),
               wellPanel(
                   h5(helpText("Data Set:")),                   
                   selectize.input(inputId = "data",
                                   choices =
                                       c("2015",
                                         "2013")),
                   h5(helpText("Sample Size:")),                   
                   selectize.input(inputId = "sample",
                                   choices = sample.sizes),
                   h5(helpText("Select variable(s):")),           
                   uiOutput("vars"),                   
                   downloadLink("downloadFile", h5("Download Data")),
                   modal.help("Help", "Census Viewer"))),
        column(width = 10,
               br(),
               tabsetPanel(
                   tabPanel("Table",
                            br(),
                            uiOutput("tableHelp"),
                            DT::dataTableOutput(outputId = "table",
                                                width = "100%",
                                                height = "auto")),
                   tabPanel("Plot",
                            br(),
                            uiOutput("plotHelp"),
                            showOutput("barplot", "nvd3"),
                            plotOutput("plot1", height = 0))
                   )
               )
        )

###  Call app.
shinyApp(ui, server)




