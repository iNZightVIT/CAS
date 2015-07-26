###---------------------###
###  Census At School   ###
###---------------------###
###
###  Last Modified  :  July 13, 2015.
###
###  Description
###  -----------
###  One-file shiny application for the Census at School project.
###  The app currently only uses the 2013 census data.

###  Source
library(DT)
library(shiny)
library(rCharts)
source("global.R")

###  Initial Paramters
col1 = "#5CB8E6"
col2 = "#F5F5F5"
exts = c("Scroller", "ColReorder")
census.labs = colnames(census2013)
census.rows = nrow(census2013)
census.cols = ncol(census2013)
sample.sizes = c("All", "7500", "5000", "2000", "1000")
options(RCHART_LIB = "nvd3")

###  Shiny Server
server =
    function(input, output, session) {
        ##  Input
        inputData = reactive({
            input$vars
            if (identical(input$sample, "All")) 
                n = sample(1:census.rows, census.rows)            
            else 
                n = sample(1:census.rows, as.numeric(input$sample))
            data = census2013[n, ]            
            switch(input$data,
                   "Census 2013" = data)
        })
        
        ##  Variable
        updateSelectizeInput(session,
                             inputId = "vars",
                             choices = all.labs)

        ##  Data
        subData = reactive({
            validate(
                need(try(length(input$vars) >= 1),
                     "Please select a variable."))
            inputData()[, input$vars]
        })

        ##  Labels
        check.vars =
            function() {
                validate(
                    need(try(length(input$vars) >= 1),
                         "Please select a variable."))
                n = length(input$vars)
                grps <<- labs <<- character(n)
                for (i in seq_len(n)) {
                    find.var = function(x) any(x == input$vars[i])
                    detect = sapply(all.labs, find.var)
                    group = names(which(detect))
                    vars = all.labs[[group]]
                    ii = which(vars == input$vars[i])
                    grps[i] <<- group
                    labs[i] <<- names(vars)[[ii]]
                }
                lab1 <<- labs[1]
                grp1 <<- grps[1]
                var1 <<- input$vars[1]            
                if (identical(length(input$vars), 2L)) {
                    lab2 <<- labs[2]                
                    grp2 <<- grps[2]
                    var2 <<- input$vars[2]
                }
            }           
        
        ##  Table
        output$table = renderDataTable({
            validate(
                need(try(length(input$vars) >= 1), ""))
            check.vars()
            DT::datatable(
                if (identical(length(input$vars), 1L))
                    oneway.freq(subData(), labs)$data
                else
                    twoway.freq(subData(), labs)$data,
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
            validate(
                need(try(length(input$vars) >= 1), ""))
            check.vars()
            if (identical(length(input$vars), 1L)) 
                raw = oneway.freq(subData(), labs)$alt.data
            else 
                raw = twoway.freq(subData(), labs)$alt.data
            temps = names(raw)            
            m = length(temps)
            n = length(input$vars) 
            for (i in seq_len(n)) {
                if (ncol(raw) < 7 & nchar(labs[i]) < 13) {
                    raw[, i] = paste0(labs[i], ": ", raw[, i])      
                } else {
                    name = substring(labs[i], 1, 12)
                    raw[, i] = paste0(name, ": ", raw[, i])                   
                }
            }
            form = formula(paste(temps[n + 1], "~", temps[1]))
            if (identical(length(input$vars), 1L)) {
                p1 <<-
                    nPlot(form,
                          data = raw,
                          type = "discreteBarChart",
                          width = session$clientData[["output_plot1_width"]])                
                ## p1$chart(color = c('#5CB8E6', '#E87F5B'))
                p1$yAxis(axisLabel = "Frequency")
                p1$addParams(dom = 'barplot')
                p1
            } else {
                p2 <<-
                    nPlot(form,
                          data = raw,
                          group = temps[2],
                          type = "multiBarChart",
                          width = session$clientData[["output_plot1_width"]])
                ## p2$chart(color = c('#5CB8E6', '#E87F5B'),
                ##          margin = list(left = 100))
                p2$yAxis(axisLabel = "Frequency")
                p2$addParams(dom = 'barplot')
                ## p2$save('plot.html', standalone = TRUE)
                p2
            }
        })                

        ##  Download File
        output$downloadFile = downloadHandler(
            filename = function(x) paste0(input$data, ".csv"),
            content = function(file) write.csv(subData(), file))
        
        ##  Table Help Text
        output$tableHelp = renderUI({
            input$vars
            validate(
                need(try(length(input$vars) >= 1),
                     "\nPlease select a variable to create a table."))
            check.vars()
            if (identical(length(input$vars), 1L)) {
                validate(
                    need(try(identical(length(input$vars), 1L)), ""))
                s.text = h4("One-way Table of Counts")
                q.text =
                    h5(paste0("Survey Question: ", help.texts[[grp1]][var1]))
                c.text = h5(paste0("Variable Category: ", grp1))
                v.text =
                    h5(paste0("Variable: ", labs, " (",
                              unit.texts[[grp1]][var1], ")"))
                HTML(paste(s.text, "<hr>", q.text, c.text, v.text), "<br>")
            } else {
                validate(
                    need(try(identical(length(input$vars), 2L)), ""))
                s.text = h4("Two-way Table of Counts")
                q1.text =
                    h5(paste0("Survey Question 1: ", help.texts[[grp1]][var1]))
                q2.text =
                    h5(paste0("Survey Question 2: ", help.texts[[grp2]][var2]))
                v1.text =
                    h5(paste0("Variable 1: ", lab1, " (",
                              unit.texts[[grp1]][var1], ")"))
                v2.text = 
                    h5(paste0("Variable 2: ", lab2," (",
                              unit.texts[[grp2]][var2], ")"))
                c1.text = h5(paste0("Variable Category 1: ", grp1))
                c2.text = h5(paste0("Variable Category 2: ", grp2))
                HTML(paste(s.text, "<hr>",
                           q1.text, c1.text, v1.text, "<br>",
                           q2.text, c2.text, v2.text, "<br>"))
            }
        })

        ##  Plot Help Text
        output$plotHelp = renderUI({
            validate(
                need(try(length(input$vars) >= 1),
                     "\nPlease select a variable create a plot."))
            s.text = h4("Side-by-Side Bar Plot")
            if (identical(length(input$vars), 1L)) {
                validate(
                    need(try(identical(length(input$vars), 1L)), ""))
                c.text = h5(paste0("Variable Category: ", grp1))
                v.text =
                    h5(paste0("Variable: ", lab1, " (",
                              unit.texts[[grp1]][var1], ")"))
                HTML(paste(h4(s.text), "<hr>", c.text, v.text, "<br>"))
            } else {
                validate(
                    need(try(identical(length(input$vars), 2L)), ""))
                c1.text = h5(paste0("Variable Category 1: ", grp1))
                c2.text = h5(paste0("Variable Category 2: ", grp2))
                v1.text =
                    h5(paste0("Variable 1: ", lab1, " (",
                              unit.texts[[grp1]][var1], ")"))
                v2.text =
                    h5(paste0("Variable 2: ", lab2, " (",
                              unit.texts[[grp2]][var2], ")"))                
                HTML(paste(s.text, "<hr>",
                           c1.text, v1.text, "<br>",
                           c2.text, v2.text, "<br>"))
            }
        })
        
        ##  Full Width Plots
        ##
        ##  Source:
        ##  gist.github.com/nassimhaddad/3057e9ac687591fa5138
        output$text = renderText({
            sprintf("width = %s, height = %s",
                    session$clientData[["output_plot1_width"]],
                    session$clientData[["output_plot1_height"]])
        })         
        
    }

###  Shiny UI
ui =
    fluidPage(
        ##  titlePanel("Census At School: Table Maker +"), ##  Add logo here?
        titlePanel("Census Viewer"),
        theme = "cosmo.css", ##  Write one from scratch?
        column(
            width = 2,
            br(),
            wellPanel(
                h5(helpText("Data Set:")),
                selectize.input(inputId = "data",
                                choices = "Census 2013"),
                h5(helpText("Sample Size:")),
                selectize.input(inputId = "sample",
                                choices = sample.sizes),
                h5(helpText("Variables (maximum 2):")),
                selectize.input(inputId = "vars",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(maxItems = 2)),
                ## h5(helpText("Download:")),
                downloadLink("downloadFile", h5("Download Data")))
            ),

        column(
            width = 10,
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

###  Shiny App
shinyApp(ui, server)









