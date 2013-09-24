library(shiny)

df <- get("*questionr_grec_tmp_df*", inherits=TRUE)
oldvar <- (df[,get("*questionr_grec_tmp_oldvar*", inherits=TRUE)])

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$recodeOut <- renderText({
      out <- NULL
      for (l in levels(oldvar)) {
          out <- paste0(out, l, " -> ", input[[l]],"<br />")
      }
      attr(out, "html") <- TRUE
      out
  })

  output$levelsInput <- renderUI({
      out <- NULL
      for (l in levels(oldvar)) {
          out <- paste0(out,(textInput(l, paste0('"',l,'" devient :'), l)))
      }
      attr(out, "html") <- TRUE
      out
  })
})
