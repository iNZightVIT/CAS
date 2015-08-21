###  Census Viewer
###
###  Last Modified  :  August 21, 2015.

###  Load required packages and global script.
library(DT)
library(shiny)
library(rCharts)
source("global.R")

###  Server.
server =
    function(input, output, session) {        
        ##  Store reactive values.
        vals = reactiveValues()

        ##  Input check.
        check.input =
            function(input) {
                wipe = function(x) !(is.na(x) & is.null(x))
                if (length(input) > 0)
                    all(sapply(input, wipe))
                else
                    FALSE
            }
        
        ##  Set reactive values and select data set.
        observe({
            input$data
            if (check.input(input$data)) {
                inputData =
                    switch(input$data,
                           "Census 2015" = census2015,
                           "Census 2013" = census2013)
                vals$all.labs = inputData$labs
                vals$data = inputData$data
                vals$help = inputData$help
                vals$unit = inputData$unit
                vals$lvls = inputData$lvls
                vals$nrow = nrow(inputData$data)
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
                    
                    ##  Frequency data.
                    if (identical(length(input$vars), 1L)) 
                        vals$freq = oneway(vals$cols, input$vars,
                                           vals$lvls, vals$labs)
                    else                     
                        vals$freq = twoway(vals$cols, input$vars,
                                           vals$lvls, vals$labs)
                    
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
                    scrollY = 200,
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
            freq = vals$freq$alt.data
            validate(need(try(length(input$vars) >= 1), ""))
            validate(need(try(!is.null(freq)), ""))

            ##  Assign
            name = names(freq)
            labs = vals$labs                        
            
            ##  Length
            nvar = length(input$vars)

            ##  Loop
            for (i in seq_len(nvar)) {
                if (ncol(freq) < 7 & nchar(labs[i] < 12))
                    freq[, i] = name.func(labs[i], freq[, i])
                else
                    freq[, i] = name.func(substring(labs[i], 1, 11),
                                          freq[, i])
            }
            
            ##  Formula
            form = formula(name.func(name[nvar + 1], name[1], " ~ "))
            
            ##  Draw
            plot.width = session$clientData[["output_plot1_width"]]
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
       
        ##  Download
        output$downloadFile = downloadHandler(
            filename = function(x) paste0(input$data, ".csv"),
            content = function(file) write.csv(vals$cols, file))
    }

##  User Interface.
ui =
    fluidPage(
        titlePanel("Census Viewer"),
        theme = "cosmo.css",
        column(width = 2,
               br(),
               wellPanel(
                   h5(helpText("Data Set:")),                   
                   selectize.input(inputId = "data",
                                   choices =
                                       c("Census 2015",
                                         "Census 2013")),
                   h5(helpText("Sample Size:")),                   
                   selectize.input(inputId = "sample",
                                   choices = sample.sizes),
                   h5(helpText("Select variable(s):")),           
                   uiOutput("vars"),
                   downloadLink("downloadFile", h5("Download Data")))),
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




