##' Interactive conversion from numeric to factor
##'
##' This function launches a shiny app in a web browser in order to do
##' interactive conversion of a numeric variable into a categorical one.
##'
##' @param dfobject data frame to operate on, as an object or a character string
##' @param oldvar name of the variable to be recoded, as a character string (possibly without quotes)
##' @return
##' The function launches a shiny app in the system web browser. The recoding code is returned in the console
##' when the app is closed with the "Done" button.
##' @author Julien Barnier <julien.barnier@@ens-lyon.fr>
##' @examples
##' \dontrun{data(hdv2003)
##' icut(hdv2003, "age")
##' irec(hdv2003, heures.tv) ## this also works}
##' 
##' @import shiny
##' @export

icut <- function(dfobject, oldvar) {
    ## Check if dfobject is an object or a character string
    if (!is.character(dfobject)) dfobject <- deparse(substitute(dfobject))
    ## Prevents get() conflicts
    if (dfobject=="dfobject") stop(sQuote(paste0(dfobject, ' must not be an object named "dfobject".')))
    ## Check if dfobject is a data frame
    if (!is.data.frame(get(dfobject))) stop(sQuote(paste0(dfobject, ' must be a data frame.')))
    ## If oldvar is not a character string, deparse it
    is_char <- FALSE
    try(if(is.character(oldvar)) is_char <- TRUE, silent=TRUE)
    if (!is_char) oldvar <- deparse(substitute(oldvar))
    ## Check if oldvar is a column of df
    if (!(oldvar %in% names(get(dfobject)))) stop(sQuote(paste0(oldvar, ' must be a column of ', dfobject, '.')))    
    
    ## Global variables
    ## Original data frame name and object
    df_name <- dfobject
    df <- get(df_name)
    if (inherits(df, "tbl_df") || inherits(df, "data.table")) df <- as.data.frame(df)
    ## Variable to be recoded, name and object
    oldvar_name <- oldvar
    oldvar <- df[,oldvar_name]
    ## Formatted source variable name
    src_var <- ifelse(grepl(" ", oldvar_name),
                      sprintf('%s[,"%s"]', df_name, oldvar_name),
                      sprintf('%s$%s', df_name, oldvar_name))

    ## Flag to display the alert on first time launch
    show_alert <- is.null(getOption("questionr_hide_alert"))
    if (show_alert) options(questionr_hide_alert=TRUE)  
    
    ## CSS file
    css.file <- system.file(file.path("shiny", "css", "ifuncs.css"), package = "questionr")
    css.content <- paste(readLines(css.file),collapse="\n")
    

    summary_table <- function(v) {
      out <- "<table class='table table-bordered table-condensed' id='sumtable'>"
      out <- paste0(out, "<thead><tr>")
      out <- paste0(out, "<th>Min</th><th>1st quartile</th><th>Median</th><th>Mean</th><th>3rd quartile</th><th>Max</th><th>NA</th>")
      out <- paste0(out, "</tr></thead><tbody><tr>")
      out <- paste0(out, sprintf("<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>",
                                 min(v, na.rm=TRUE), stats::quantile(v, prob=0.25, na.rm=TRUE),
                                 stats::median(v, na.rm=TRUE), round(mean(v, na.rm=TRUE),3),
                                 stats::quantile(v, prob=0.75, na.rm=TRUE), max(v, na.rm=TRUE), sum(is.na(v))))
      out <- paste0(out, "</tr></tbody></table>")
      out
    }
    
    ## Run shiny app
    shiny::shinyApp(ui=bootstrapPage(
      header=tags$head(
        ## Custom CSS
        tags$style(HTML(css.content))),
 
        ## Page title
        div(class="container",
            div(class="row",
                headerPanel(gettext("Interactive cutting", domain="R-questionr"))),

          ## Display an alert, only on first launch for the current session
            if (show_alert) {
              div(class="row",
                  div(class="col-md-12",
                      div(class="alert alert-warning alert-dismissible",
                          HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
                          HTML(gettext("<strong>Warning :</strong> This interface doesn't do anything by itself. It only generates R code you'll have to copy/paste into your script and execute yourself.", domain="R-questionr"))
                      )))} else "",
            
            ## First panel : new variable name
            div(class="row",
                div(class="col-sm-7",
                    div(class="row",
                        div(class="col-md-12 well",
                            tags$form(textInput("newvarname",
                                                gettext('New variable',domain="R-questionr"), 
                                                paste0(oldvar_name,".rec")),HTML("</td>"))
                            ),
                        div(class="col-md-12 well",
                            tags$form(
                                      HTML(gettextf("<p>Statistics of <tt>%s</tt> :</p>", oldvar_name, domain="R-questionr")),
                                      HTML(summary_table(oldvar)),
                                      selectizeInput("cutMethod", gettext('Cutting method',domain='R-questionr'), choices=c("Manual" = "fixed", "Standard deviation" = "sd", "Equal width" = "equal", "Pretty" = "pretty", "Quantile" = "quantile", "K-means" = "kmeans", "Hierarchical cluster" = "hclust", "Bagged clustering" = "bclust", "Fisher algorithm" = "fisher", "Jenks algorithm" = "jenks")),
                                      uiOutput("ui"),
                                      textInput("breaks", "Breaks"),
                                      checkboxInput("right", HTML(gettext("Right-closed intervals (<tt>right</tt>)", domain="R-questionr")), TRUE),
                                      checkboxInput("inclowest", HTML(gettext("Include extreme (<tt>include.lowest</tt>)", domain="R-questionr")), FALSE),
                                      checkboxInput("addext", gettext("Append extreme values if necessary", domain="R-questionr"), FALSE)
                            )))), 
                
                
                div(class="col-sm-5",
                    wellPanel(plotOutput("histOut")))),
            
            ## Second panel : recoding fields,
            
            ## Main panel with tabs
            div(class="row",
                div(class="col-md-12",
                    tabsetPanel(
                      ## Code tab
                      tabPanel(gettext("Code", domain="R-questionr"), htmlOutput("codeOut")),
                      ## Table check tab
                      tabPanel(gettext("Check", domain="R-questionr"),
                               div(class="span4",
                                   HTML("<p class='header'></p>"),
                                   tableOutput("tableOut")),
                               div(class="span8",
                                   plotOutput("barOut"))
                      )),
                    
                    ## Bottom buttons
                    p(class='bottom-buttons',
                      tags$button(id="donebutton", type="button", class="btn action-button btn-success", 
                                  onclick="javascript:window.close();", 
                                  list(icon=icon("share")), 
                                  gettext("Send code to console and exit", domain="R-questionr"))
                    ),
                    textOutput("done")
                )
                      
        ))),
      
      server=function(input, output, session) {
        
          output$ui <- renderUI({
            if (input$cutMethod == "fixed") return()
            numericInput(inputId="nb_breaks", label=gettext("Breaks number", domain="R-questionr"), value=6, min=2, step=1)
          })
          
          observe(if (input$cutMethod != "fixed") {
            nb_breaks <- reactive({
              if (is.null(input$nb_breaks)) return(2)
              if (is.na(input$nb_breaks)) return(2)
              if (input$nb_breaks < 2) return(2)
              return(input$nb_breaks)
            })
            updateTextInput(session, "breaks", value=classInt::classIntervals(oldvar, n=ifelse(is.null(nb_breaks()), 6, nb_breaks()), style=input$cutMethod)$brks)
          })
      
        get_breaks <- function(b, compute=FALSE) {
          if (b=="") return(NULL)
          b <- gsub(", *$", "",b)
          b <- paste0("c(",b,")")
          breaks <- sort(unique(eval(parse(text=b))))
          ## Code taken directly from `cut` source code
          if (length(breaks) == 1L && compute) {
            if (is.na(breaks) || breaks < 2L) 
              stop("invalid number of intervals")
            nb <- as.integer(breaks + 1)
            dx <- diff(rx <- range(oldvar, na.rm = TRUE))
            if (dx == 0) 
              dx <- abs(rx[1L])
            breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000, 
                              length.out = nb)
          }
          if (length(breaks) > 1 && input$addext) {
            if (min(breaks, na.rm=TRUE) > min(oldvar, na.rm=TRUE)) breaks <- c(min(oldvar, na.rm=TRUE), breaks)
            if (max(breaks, na.rm=TRUE) < max(oldvar, na.rm=TRUE)) breaks <- c(breaks, max(oldvar, na.rm=TRUE))
          }
          breaks
        }
        
        
        ## Call recoding code generation function based on style
        generate_code <- function(check=FALSE) {
          newvar_name <- input$newvarname
          ## if null, create temporary variable for check table
          if (check) dest_var <- ".icut_tmp"
          ## else, format new variable for code
          else
            dest_var <- ifelse(grepl(" ", newvar_name),
                               sprintf('%s[,"%s"]', df_name, newvar_name),
                               sprintf('%s$%s', df_name, newvar_name))
          out <- sprintf(gettextf("## Cutting %s into %s\n", oldvar_name, newvar_name, domain="R-questionr"))
          out <- paste0(out, sprintf("%s <- cut(%s, include.lowest=%s,  right=%s,\n", dest_var, src_var, input$inclowest, input$right))
          breaks <- paste0(utils::capture.output(dput(get_breaks(input$breaks))), collapse="")
          out <- paste0(out, paste0(rep(" ",nchar(dest_var)+8),collapse=""),sprintf("breaks=%s)\n", breaks))
          out
        }
        
        
        output$histOut <- renderPlot({
          graphics::hist(oldvar, col="#bbd8e9", border="white", main=gettext("Original histogram", domain="R-questionr"), xlab=oldvar_name)
          breaks <- get_breaks(input$breaks, compute=TRUE)
          for (b in breaks) graphics::abline(v=b, col="#dd1144", lwd=1, lty=2)
        })
        
        
        ## Generate the code in the interface
        output$codeOut <- renderText({
          ## Generate code
          out <- generate_code()
          ## Generated code syntax highlighting
          out <- paste(highr::hi_html(out), collapse="\n")
          ## Final paste
          out <- paste0("<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
          out
        })
        
        output$done <- renderText({
          ## Generate code
          out <- generate_code()
          ## If "Done" button is pressed, exit and cat generated code in the console
          if (input$donebutton > 0) {
            cat(gettext("\n-------- Start recoding code --------\n\n", domain="R-questionr"))
            cat(out)
            cat(gettext("\n--------- End recoding code ---------\n", domain="R-questionr"))
            shiny::stopApp()
          }
          return("")
        })
        
        ## Generate the check table
        output$tableOut <- renderTable({
          ## Generate the recoding code with a temporary variable
          code <- generate_code(check=TRUE)
          ## Eval generated code
          eval(parse(text=code))
          ## Display table
          tab <- freq(get(".icut_tmp"))
          tab
        })
        
        ## Generate the barplot
        output$barOut <- renderPlot({
          ## Generate the recoding code with a temporary variable
          code <- generate_code(check=TRUE)
          ## Eval generated code
          eval(parse(text=code))
          ## Display table
          graphics::plot(get(".icut_tmp"), col="#bbd8e9", border="white")
        })
    })
    
}
