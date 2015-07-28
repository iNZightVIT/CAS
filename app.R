###  Census Viewer
###
###  Last Modified  :  July 28, 2015

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
                vals$nrow = nrow(inputData$data)
            }
        })

        ##  Sample data.
        observe({
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
        })
        
        ##  Variable handler.
        observe({
            input$vars
            if (check.input(input$vars)) 
                vals$vars = input$vars            
            else 
                vals$vars = ""            
        })
        
        ##  Set groups, labels, and frequencies.
        observe({
            input$vars
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
                    vals$freq = oneway(vals$cols, vals$labs)
                else                     
                    vals$freq = twoway(vals$cols, vals$labs)
            }
        })
        
        
        ##  Variable selection
        output$vars = renderUI({
            selectizeInput(inputId = "vars",
                           label = NULL,
                           multiple = TRUE,
                           selected = "gender",
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
            
            ##  Plot
            ##
            ##  This "plot.width" is dirty..but does the job.
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
                                       c("Census 2013",
                                         "Census 2015")),
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
                            DT::dataTableOutput(outputId = "table",
                                                width = "100%",
                                                height = "auto")),
                   tabPanel("Plot",
                            br(),
                            showOutput("barplot", "nvd3"),
                            plotOutput("plot1", height = 0))
                   )
               )
        ) 



shinyApp(ui, server)




