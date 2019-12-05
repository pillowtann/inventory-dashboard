
# DEPLOY APP

#library(rsconnect)
#rsconnect::deployApp("D:/CKC/inv_red_flag/2. Threshold Setting/interactive_threshold/interactive_threshold_app")

# INTERACTIVE-THRESHOLD APP

library(shiny)
#library(ggplot2)
library(readxl)
library(scales)
library(plyr)
library(naniar)
library(tidyr)
#devtools::install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))
#install.packages("rpivotTable")
library(rpivotTable)
library(rvest)
library(DT)
library(formattable)
library(dplyr)
#library(rhandsontable)
library(plotly)

#"D:/CKC/inv_red_flag/2. Threshold Setting/interactive_threshold/interactive_threshold_app/data/inv_launch_dashboard_byac.csv"
data <- read.csv("data/inv_launch_dashboard_byac.csv",check.names=FALSE)
data <- data %>% drop_na(articlecode)
week_data <- read.csv("data/inv_last_2_weeks_sold.csv",check.names=FALSE)
week_data <- week_data %>% drop_na(articlecode)
get_week_number <- as.numeric(strftime(unique(data$soh_date[!is.na(data$soh_date)]), format = "%V"))-1

# Formatting
numericals <- c("4_weeks","8_weeks","13_weeks","utd_sold",
                "rpt_4_weeks","rpt_8_weeks","rpt_13_weeks", "current_soh",
                "initial_qty","last_repeat_qty","utd_order_qty")
data[, numericals] <- data[, numericals] %>% replace_with_na_all(condition = ~.x == "(null)")
week_numericals <- c("lw_nos",	"llw_nos","lw_soldqty","llw_soldqty")
week_data[, week_numericals] <- week_data[, week_numericals] %>% replace_with_na_all(condition = ~.x == "(null)")

#prices <- c("originalprice","currentmarketprice","discountprice","retailcost","originalvalue","currentvalue","salesvalue")
twodp <- c("Sell_Through")

# Data calculations
data$`repeat` <- ifelse(is.na(data$last_repeat_qty), 'No Repeat', 
                        ifelse(data$last_repeat_qty > 0, 'Repeat', 'No Repeat'))

# BY SKU LW SOLDQTY
week_data$lw_soldqty[is.na(week_data$lw_soldqty)]<- 0
week_data <- week_data[order(week_data$category, -week_data$lw_soldqty),]
week_data$LW_Cumulative <- ave(week_data$lw_soldqty, week_data$category, FUN=cumsum)
total_sold <- setNames(aggregate(week_data$lw_soldqty, by=list(week_data$category), FUN=sum),c('category','total_lw_sold'))
week_data <- merge(x = week_data, y = total_sold, by = 'category', all.x = TRUE)
week_data$LW_Cumulative_Perc <- week_data$LW_Cumulative/week_data$total_lw_sold

# BY SKU LW ROS
week_data$lw_nos[is.na(week_data$lw_nos)]<- 0
week_data$lw_ros <- week_data$lw_soldqty/week_data$lw_nos
week_data$lw_ros[is.na(week_data$lw_ros)]<- 0
week_data <- week_data[order(week_data$category, -week_data$lw_ros),]
week_data$LW_ROS_Cumulative <- ave(week_data$lw_ros, week_data$category, FUN=cumsum)
total_ros_sold <- setNames(aggregate(week_data$lw_ros, by=list(week_data$category), FUN=sum),c('category','total_lw_ros'))
week_data <- merge(x = week_data, y = total_ros_sold, by = 'category', all.x = TRUE)
week_data$LW_ROS_Cumulative_Perc <- week_data$LW_ROS_Cumulative/week_data$total_lw_ros

# BY SKU L2W SOLDQTY
week_data$llw_soldqty[is.na(week_data$llw_soldqty)]<- 0
week_data$l2w_soldqty<- week_data$llw_soldqty + week_data$lw_soldqty
week_data <- week_data[order(week_data$category, -week_data$l2w_soldqty),]
week_data$L2W_Cumulative <- ave(week_data$l2w_soldqty, week_data$category, FUN=cumsum)
total_2_sold <- setNames(aggregate(week_data$l2w_soldqty, by=list(week_data$category), FUN=sum),c('category','total_l2w_sold'))
week_data <- merge(x = week_data, y = total_2_sold, by = 'category', all.x = TRUE)
week_data$L2W_Cumulative_Perc <- week_data$L2W_Cumulative/week_data$total_l2w_sold
# BY ROS L2W ROS

# BY ART LW SOLDQTY
week_data$colour_count <- 1
lw_week_data_art <- aggregate( week_data[,c("colour_count", "lw_soldqty","lw_nos")], week_data[,c("category","articlecode")], FUN = sum )
lw_week_data_art$lw_nos <- lw_week_data_art$lw_nos/lw_week_data_art$colour_count
lw_week_data_art <- lw_week_data_art[order(lw_week_data_art$category, -lw_week_data_art$lw_soldqty),]
lw_week_data_art$LW_Cumulative <- ave(lw_week_data_art$lw_soldqty, lw_week_data_art$category, FUN=cumsum)
total_sold <- setNames(aggregate(lw_week_data_art$lw_soldqty, by=list(lw_week_data_art$category), FUN=sum),c('category','total_lw_sold'))
lw_week_data_art <- merge(x = lw_week_data_art, y = total_sold, by = 'category', all.x = TRUE)
lw_week_data_art$LW_Cumulative_Perc <- lw_week_data_art$LW_Cumulative/lw_week_data_art$total_lw_sold

# BY ART LW ROS
lw_week_data_art$lw_ros <- lw_week_data_art$lw_soldqty/lw_week_data_art$lw_nos
lw_week_data_art$lw_ros[is.na(lw_week_data_art$lw_ros)]<- 0
lw_week_data_art <- lw_week_data_art[order(lw_week_data_art$category, -lw_week_data_art$lw_ros),]
lw_week_data_art$LW_ROS_Cumulative <- ave(lw_week_data_art$lw_ros, lw_week_data_art$category, FUN=cumsum)
total_ros_sold <- setNames(aggregate(lw_week_data_art$lw_ros, by=list(lw_week_data_art$category), FUN=sum),c('category','total_lw_ros'))
lw_week_data_art <- merge(x = lw_week_data_art, y = total_ros_sold, by = 'category', all.x = TRUE)
lw_week_data_art$LW_ROS_Cumulative_Perc <- lw_week_data_art$LW_ROS_Cumulative/lw_week_data_art$total_lw_ros

week_data <- merge(x=week_data, y=lw_week_data_art, by='articlecode', all.x=TRUE, suffixes = c("","_art"))

# Functions

# Drop down function from 
dropdownButton <- 
    
    function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
    
    status <- match.arg(status)
    # dropdown button content
    html_ul <- list(
        class = "dropdown-menu",
        style = if (!is.null(width)) 
            paste0("width: ", validateCssUnit(width), ";"),
        lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
    )
    # dropdown button apparence
    html_button <- list(
        class = paste0("btn btn-", status," dropdown-toggle"),
        type = "button", 
        `data-toggle` = "dropdown"
    )
    html_button <- c(html_button, list(label))
    html_button <- c(html_button, list(tags$span(class = "caret")))
    # final result
    tags$div(
        class = "dropdown",
        do.call(tags$button, html_button),
        do.call(tags$ul, html_ul),
        tags$script(
            "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
    )
}





# APP

server <- function(input, output,session) {
    
    ##### Reactive expression to create data frame of all input values
    sliderValues <- reactive({
        
        data.frame(
            Name = c("ABC_Range", "ABC_SKUArt","ABC_ROSSold","ABC_WkPeriod","ST_Range", "ST_SKUArt", "ST_WkPeriod"),
            Value = as.character(c(paste(input$ABCRange, collapse = " "), input$skuart, input$rossold, input$wkperiod, 
                                   paste(input$STRange, collapse = " "), input$ST_skuart, input$ST_wkperiod)),
            stringsAsFactors = FALSE
                ) #close data.frame
        # input$ABCRange[[1]] --> LOWER BOUND (AB)
        # input$ABCRange[[2]] --> UPPER BOUND (BC)
        
    })
    
    # Show the values in text format
    #output$selected_var <- renderText({ 
    #    paste("You have selected", input$ABCRange[[2]])
    #})
    
    # Show the values in an HTML table
    output$values <- renderTable({
        sliderValues()
    })
    
    
    ##### Actual data tables
    interactive_Week_Table <- reactive({
        # Tags user defined range of ABC into table
      
      if (input$skuart == "SKU" & input$rossold == "SoldQty" & input$wkperiod == "LastWeek") {
        week_data$Grading<-ifelse(week_data$LW_Cumulative_Perc <= input$ABCRange[[1]], "A",
                                           ifelse(week_data$LW_Cumulative_Perc <= input$ABCRange[[2]], "B", "C"))
      }
      
      if (input$skuart == "SKU" & input$rossold == "SoldQty" & input$wkperiod == "Last2Weeks") {
        week_data$Grading<-ifelse(week_data$L2W_Cumulative_Perc <= input$ABCRange[[1]], "A",
                                      ifelse(week_data$L2W_Cumulative_Perc <= input$ABCRange[[2]], "B", "C"))
      }
      
      if (input$skuart == "SKU" & input$rossold == "ROS" & input$wkperiod == "LastWeek") {
        week_data$Grading<-ifelse(week_data$LW_ROS_Cumulative_Perc <= input$ABCRange[[1]], "A",
                                      ifelse(week_data$LW_ROS_Cumulative_Perc <= input$ABCRange[[2]], "B", "C"))
      }
      
      if (input$skuart == "Art" & input$rossold == "SoldQty" & input$wkperiod == "LastWeek") {
        week_data$Grading<-ifelse(week_data$LW_Cumulative_Perc_art <= input$ABCRange[[1]], "A",
                                      ifelse(week_data$LW_Cumulative_Perc_art <= input$ABCRange[[2]], "B", "C"))
      }
      
      if (input$skuart == "Art" & input$rossold == "ROS" & input$wkperiod == "LastWeek") {
        week_data$Grading<-ifelse(week_data$LW_ROS_Cumulative_Perc_art <= input$ABCRange[[1]], "A",
                                      ifelse(week_data$LW_ROS_Cumulative_Perc_art <= input$ABCRange[[2]], "B", "C"))
      }
        week_data
    })
    
    interactive_Table <- reactive({
      # Tags user defined range of ABC into table
      data <- merge(x = data, y = interactive_Week_Table(), by = c('articlecode','colour'), all.x = TRUE, suffixes = c("","_week"))
      data <- data[, !(names(data) %in% c('category_week'))] #duplicate column to remove for visual sake
      
      # Tag user defined ST definition
      
      if (input$ST_skuart == "SKU") {
        if (input$ST_wkperiod == "4_weeks") {
        data$`Sell_Through` <- data$`4_weeks`/data$initial_qty
        }
        
        if (input$ST_wkperiod == "8_weeks") {
          data$`Sell_Through` <- data$`8_weeks`/data$initial_qty
        }
        
        if (input$ST_wkperiod == "13_weeks") {
          data$`Sell_Through` <- data$`13_weeks`/data$initial_qty
        }
        
        if (input$ST_wkperiod == "utd") {
          data$`Sell_Through` <- data$`utd`/data$initial_qty
        }
      }
      
      
      if (input$ST_skuart == "Art") {
        data_art <- aggregate( data[, numericals], data[, c("category", "articlecode")], FUN = sum )
        
        if (input$ST_wkperiod == "4_weeks") {
          data_art$`Sell_Through` <- data_art$`4_weeks`/data_art$initial_qty
        }
        
        if (input$ST_wkperiod == "8_weeks") {
          data_art$`Sell_Through` <- data_art$`8_weeks`/data_art$initial_qty
        }
        
        if (input$ST_wkperiod == "13_weeks") {
          data_art$`Sell_Through` <- data_art$`13_weeks`/data_art$initial_qty
        }
        
        if (input$ST_wkperiod == "utd") {
          data_art$`Sell_Through` <- data_art$utd/data_art$initial_qty
        }
        
        data <- merge(x=data, y= data_art[,c('Sell_Through','articlecode')], by = "articlecode", all.x = TRUE)
        #data$Sell_Through <- percent(data$Sell_Through)
        #data <- data %>% formatPercentage(c("Sell_Through","LW_Cumulative_Perc"), 2)
      }
      
      # Tag user defined ST range accepted
      data$`buy_review`<-ifelse( data$`Sell_Through` >= 0 & data$`Sell_Through` <= input$STRange[[1]], "overbuy",
                                    ifelse(data$`Sell_Through` >= 0 & data$`Sell_Through` >= input$STRange[[2]], "underbuy",
                                           ifelse(is.null(data$`Sell_Through`) | data$`Sell_Through` == Inf, "-", 
                                                  ifelse(data$`Sell_Through` >= 0, "as expected", "-"))))
      data
    })
    
    
    interactive_Table_display <- reactive({
      data <- interactive_Table()
      # Display
      if (input$category != "All") {
        data <- data[data$category == input$category,]
      }
      if (input$launch != "All") {
        data <- data[data$launch == input$launch,]
      }
      if (input$rpt != "All") {
        data <- data[data$`repeat` == input$rpt,]
      }
      if (input$article != "All") {
        data <- data[data$article == input$article,]
      }
      data
    })
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(

      DT::datatable({
        
        # Formatting
        data <- interactive_Table_display()
        data$Sell_Through <- percent(data$Sell_Through)
        
        # data[, prices] <- lapply(data[, prices], function(x) paste0("¥",comma(x)))
        
        selected_columns <- if (is.null(input$check2))
        {c('category','launch', 'articlecode','colour','initial_qty', '4_weeks', 'Sell_Through', 'buy_review', 
                               'lw_soldqty', 'Grading', 'current_soh')} 
                            else {input$check2}
        data <- data[, selected_columns, drop = FALSE]

        return(as.datatable(formattable(data, list(
          
          Sell_Through = color_bar("lightblue"),
          buy_review = formatter("span", style = x ~ ifelse(x == "as expected", formattable::style(color = "green", font.weight = "bold"), 
                                                            formattable::style(color = "red", font.weight = "bold"))),
          Grading = formatter("span", style = x ~ ifelse(x == "A", formattable::style(color = "green", font.weight = "bold"), 
                              ifelse(x == "C", formattable::style(color = "red", font.weight = "bold"), 
                                     formattable::style(color = "orange", font.weight = "bold"))))
          ) #close list
          ) #formattable
          )) #close return
        
        
    }
    , filter='top', options = list(autoWidth = TRUE)
    
    ) #close datatable
    
    ) #close rendertable
    
    
    output$trendPlot <- renderPlotly({
      
      # Create summary pivot table to plot
      dataset <- interactive_Table_display()
      
      # Remove rows with NA in Grading and buy_review
      dataset <- dataset[!is.na(dataset$buy_review),]
      dataset <- dataset[!is.na(dataset$Grading),]
      
      # Formatting to pivot plot data
      dataset <- fastDummies::dummy_cols(dataset, select_columns = "Grading")
      current_headers <- lapply(sort(unique(dataset$Grading)), function(x) paste0("Grading_",x))
      needed_headers <- c('Grading_A', 'Grading_B', 'Grading_C')
      
      if (length(current_headers) <= 2) {
        missing_headers <- setdiff(needed_headers,current_headers)
        
        for (col in missing_headers){
          dataset[, col] <- 0
        } # close loop creating headers
        
      } # close if missing headers
      
      
      # Get summary table
      dataset <- setNames(aggregate(dataset[,c('Grading_A', 'Grading_B', 'Grading_C')],
                                    by=list(dataset$buy_review), FUN=sum),
                          c('buy_review','Grading_A', 'Grading_B', 'Grading_C'))
        
        # Stacked bar chart
        p <- plot_ly(dataset, x = ~buy_review, y = ~Grading_C, type = 'bar', name = 'Grading_C', marker = list(color = 'rgb(219,66,66)')) %>%
          plotly::add_trace(y = ~Grading_B, name = 'Grading_B', marker = list(color = "orange")) %>%
          plotly::add_trace(y = ~Grading_A, name = 'Grading_A', marker = list(color = 'green')) %>% #'rgb(63,215,47)'
          plotly::layout(barmode = 'stack',
                         yaxis = list(title = 'Count'), 
                         xaxis = list(categoryorder = "array",
                                      categoryarray = c('underbuy', 'as expected', 'overbuy'))
          )
      
    })
                                        
    # Filter data based on selections
    #output$week_table <- DT::renderDataTable(DT::datatable({
    #    
    #    # Formatting
    #    # IF any
    #    
    #    week_data <- interactive_Week_Table()
    #    # Display
    #    week_data <- week_data
    #    if (input$week_category != "All") {
    #        week_data <- week_data[week_data$category == input$week_category,]
    #    }
    #    if (input$week_launch != "All") {
    #        week_data <- week_data[week_data$launch == input$week_launch,]
    #    }
    #    if (input$week_article != "All") {
    #        week_data <- week_data[week_data$article == input$week_article,]
    #    }
    #    week_data
    #}))
    

    output$rpt_plot <-renderPlotly({
      
      # Create summary pivot table to plot
      dataset <- interactive_Rpt_Pivot()
      dataset$counter <- 1
      dataset <- setNames(aggregate(dataset[, c('4_weeks','lw_soldqty','lw_nos','counter')], 
                                    by = dataset[, c("category", "articlecode", "colour")], FUN=sum),
                          c('category', 'articlecode','colour', '4_weeks', 'lw_soldqty', 'lw_nos', 'count'))
      dataset$lw_nos <- dataset$lw_nos/dataset$count
      
      p <- plot_ly(
        dataset, 
        
        # Axis selection:
        x = ~lw_nos, y = ~lw_soldqty,
        
        # Hover text:
        text = ~paste(articlecode, '<br>', colour, '4WKS:', `4_weeks`),
        
        # Point Appearance:
        color = ~articlecode, #~category,
        size = ~`4_weeks`
      )
      
      
    })
      
      
      
      
    interactive_Rpt_Pivot <- reactive({
      
      # Formatting
      data <- interactive_Table()
      
      data <- data[data$Grading == "A",]
      
      if(input$ST_RPT_switch == TRUE){data <- data[data$buy_review == "underbuy",]}
      
      data <- data %>% drop_na(articlecode)
        
      #selected_columns <- if (is.null(input$check2))
      #{c('articlecode','colour','launch_year','launch_week','initial_qty', '4_weeks','lw_nos','lw_soldqty','llw_soldqty')} 
      #else {input$check2}
      
      #data <- data[, selected_columns, drop = FALSE]
      data
      
    })
    
    
    
    # Clean the html and store as reactive
    summarydf <- eventReactive(
      input$myData,{ # Must use dplyr formatting
        input$myData %>% 
        read_html %>% 
        html_table(fill = TRUE) %>% 
        #Turns out there are two tables in an rpivotTable, we want the second
        .[[2]]  %>%
        mutate(articlecode=replace(articlecode, articlecode=="BAGS", NA))   %>%
        mutate(articlecode=replace(articlecode, articlecode=="SHOES", NA))
    })
  
    
    # Downloadable csv of selected dataset ----
      
    #output$downloadData2 <- DT::renderDataTable({
    #  datatable({summarydf()}, 
    #            filter="top", 
    #            selection="multiple", 
    #            extensions = 'Buttons',
    #            options = list(pageLength = 10, 
    #                           autoWidth = TRUE,
    #                           dom = 'Bfrtip',
    #                           buttons = c('copy', 'csv', 'pdf')
    #            ))
    #})

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("proposed_repeats", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(summarydf(), file, row.names = FALSE)
      }
    )
    
    
    
    # show df as DT::datatable
    
    #output$aSummaryTable <- DT::renderDataTable({
    #  DT::datatable(summarydf(), rownames = FALSE)
    #})
    
    # Whenever the config is refreshed, call back with the content of the table
    output$rpt_table <- renderRpivotTable({
      

      # Set pivottable
      rpivotTable(data = interactive_Rpt_Pivot(), 
                  
                  # Set default pivot values
                  rows = c("category", "articlecode", "colour"),
                  vals = c("lw_soldqty","llw_soldqty"), 
                  aggregatorName = "Sum", 
                  rendererName = "Row Heatmap",
                  
                  # Set auto refresh feed into download button
                  onRefresh = htmlwidgets::JS("function(config) {
                  Shiny.onInputChange('myData', document.getElementById('rpt_table').innerHTML);}")
      )
    })
    

    columnname <- reactive({
      columnname <- colnames(interactive_Table())
      #columnname <- columnname[columnname != "articlecode"];
      #columnname <- columnname[columnname != "colour"];
    })
    
    ##### Column selection dropdown
    
    # Baseline
    observeEvent(input$baseline, {
      
      
      updateCheckboxGroupInput(
        session = session, inputId = "check2", choices = colnames(data), selected = append(input$check2, c("articlecode","colour"))
      )
      
      # Select all / Unselect all
      observeEvent(input$all, {
        if (is.null(input$check2)) {
          updateCheckboxGroupInput(
            session = session, inputId = "check2", selected = colnames(data)
          )
        } else {
          updateCheckboxGroupInput(
            session = session, inputId = "check2", selected = ""
          )
        }
      })
    })
    
    # Original Order
    observeEvent(input$orig, {

        updateCheckboxGroupInput(
            session = session, inputId = "check2", choices = columnname(), selected = append(input$check2, c("articlecode","colour"))
        )
    
      # Select all / Unselect all
      observeEvent(input$all, {
        if (is.null(input$check2)) {
          updateCheckboxGroupInput(
            session = session, inputId = "check2", selected = columnname()
          )
        } else {
          updateCheckboxGroupInput(
            session = session, inputId = "check2", selected = ""
          )
        }
      })
      })
    
    # Sort by Alphabets
    observeEvent(input$a2z, {
      
      columnname <- sort(columnname())
      
      updateCheckboxGroupInput(
            session = session, inputId = "check2", choices = columnname, selected = append(input$check2, c("articlecode","colour"))
        )
    
      # Select all / Unselect all
      observeEvent(input$all, {
        if (is.null(input$check2)) {
          updateCheckboxGroupInput(
            session = session, inputId = "check2", selected = columnname
          )
        } else {
          updateCheckboxGroupInput(
            session = session, inputId = "check2", selected = ""
          )
        }
      })
      
      })
    
    output$res2 <- renderPrint({
        input$check2
    })
    
    # Select all / Unselect all
    observeEvent(input$all, {
        if (is.null(input$check2)) {
            updateCheckboxGroupInput(
                session = session, inputId = "check2", selected = colnames(data)
            )
        } else {
            updateCheckboxGroupInput(
                session = session, inputId = "check2", selected = ""
            )
        }
    })
    
    
    
}

ui <- fluidPage(
    

    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options ----
        sidebarPanel(
            
            
            # Input: Specification of range within an interval ----
            sliderInput("ABCRange", "Grading ABC Range:",
                        min = 0, max = 1,
                        value = c(0.6,0.9)),
            
            # Input: Select grade by
            radioButtons("skuart", "ABC granularity",
                         choices = c("SKU","Art"),
                         selected = "SKU"),
            
            # Input: Select grade by
            radioButtons("rossold", "ABC Value",
                         choices = c("ROS","SoldQty"),
                         selected = "SoldQty"),
            
            # Input: Select recency by
            radioButtons("wkperiod", "ABC Wk Period",
                         choices = c("LastWeek","Last2Weeks"),
                         selected = "LastWeek"),
            
            sliderInput("STRange", "Buy Sell-Thru Range:",
                        min = 0, max = 1,
                        value = c(0.2,0.3)),
            
            # Input: Select grade by
            radioButtons("ST_skuart", "ST granularity",
                         choices = c("SKU","Art"),
                         selected = "SKU"),
            
            # Input: Select recency by
            radioButtons("ST_wkperiod", "ST Wk Period",
                         choices = c("4_weeks","8_weeks", "13_weeks", "utd"),
                         selected = "4_weeks")
            
        , width = 3),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            
            tabsetPanel(
                
                id = 'dataset',
                
                tabPanel("Overall Data",
                         
                         # Sheet Header
                         titlePanel(paste("UTWK", get_week_number,"Overall Data")),
 
                         fluidRow(
                           
                           column(width = 2,
                                  selectInput("category",
                                              "category:",
                                              c("All",
                                                unique(as.character(data$category))))
                           ),
                           column(width = 2,
                                  selectInput("launch",
                                              "launch:",
                                              c("All",
                                                unique(as.character(data$launch))))
                           ),
                           column(width = 2,
                                  selectInput("rpt",
                                              "repeat:",
                                              c("All",
                                                unique(as.character(data$`repeat`))))
                           ),
                           column(width = 2,
                                  selectInput("article",
                                              "article:",
                                              c("All",
                                                unique(as.character(data$article))))
                           )
                           
                         ),
                         
                         
                         # Display plot
                         fluidRow(
                           plotlyOutput('trendPlot', height = "300px")
                         ),
                         
                         br(),
                         
                         # Create a new Row in the UI for selectInputs
                         fluidRow(
                           column(width = 4,
                                  dropdownButton(
                                    label = "Customise columns to show", status = "default", width = 80,
                                    actionButton(inputId = "baseline", label = "Launch Info", icon = icon("spinner")),
                                    actionButton(inputId = "orig", label = "Add Wk Info", icon = icon("spinner")),
                                    actionButton(inputId = "a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
                                    br(),
                                    
                                    actionButton(inputId = "all", label = "(Un)select all"),
                                    #checkboxGroupInput(inputId = "allcol", label = "Select All", choices = c('Select All')),
                                    checkboxGroupInput(inputId = "check2", label = "Choose", choices = colnames(data))
                                  ))),
                         # Display data table
                         fluidRow(
                           DT::dataTableOutput('table')
                         )
                         #titlePanel(paste("Summary Plots")),
                      
                         ), #close tabPanel (one sheet: Overall Data)
                
                # Tabbed out unless needed to save computing time
                #tabPanel("Weekly Data",
                 #        
                 #        # Sheet Header
                 #        titlePanel("Weekly DataTable"),
                #         
                 #        # Create a new Row in the UI for selectInputs
                #       fluidRow(
                 #            column(4,
                  #                  selectInput("week_category",
                   #                             "category:",
                    #                            c("All",
                     #                             unique(as.character(week_data$category))))
                      #       ),
                       #      column(4,
                        #            selectInput("week_launch",
                        #                      "launch:",
                        #                        c("All",
                        #                          unique(as.character(week_data$launch))))
                        #     ),
                        #     column(4,
                        #            selectInput("week_article",
                        #                        "article:",
                        #                        c("All",
                        #                          unique(as.character(week_data$article))))
                        #     )
                        # ),
                        # 
                        # # Display data table
                        # DT::dataTableOutput("week_table"),
                        #
                        # 
                        # # Output: Table summarizing the values entered
                        # # textOutput("selected_var"),
                        # tableOutput("values")
                        # 
                        #), #close tabPanel (one sheet: Weekly Table)
                
                tabPanel("Proposed Repeats",
                         
                         # Sheet Header
                         titlePanel("Proposed Repeats"),
                         
                         fluidRow(
                           checkboxInput(inputId = "ST_RPT_switch", label = "Consider ST", value = TRUE)
                           ),
                         
                         fluidRow(plotlyOutput('rpt_plot', height = "300px")),

                         # DT::dataTableOutput("downloadData2"),
                         br(),
                         
                         fluidRow(downloadButton("downloadData", "Download")),
                         
                         fluidRow(rpivotTableOutput("rpt_table"))

                    
                ) #close tabPanel (one sheet: Repeat Table)
                
                
                    ) #close tabsetpanel
                ) #close mainpanel
        ) #close sidebar layout
) #close fluidpage

shinyApp(ui, server)