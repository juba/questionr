library(shiny)

df <- get(get("*questionr_grec_df*", .GlobalEnv))
oldvar_name <- get("*questionr_grec_oldvar*", .GlobalEnv)
print(oldvar_name)
oldvar <- df[,oldvar_name]

shinyServer(function(input, output) {

  output$recodeOut <- renderText({
      out <- "<pre class='r'><code class='r' id='codeout'>"
      for (l in levels(oldvar)) {
          out <- paste0(out, 'for (i in c(1:10, TRUE)) ', l, " <- \"", input[[l]],"\"\n")
      }
      out <- paste0(out, "</code></pre>")
      out
  })

  output$tableOut <- renderTable({
      table(oldvar)
  })
  
  output$levelsInput <- renderUI({
      out <- "<table>"
      for (l in levels(oldvar)) {
#          out <- paste0(out,(textInput(l, paste0('"',l,'" devient :'), l)))
          out <- paste0(out,'<tr><td>',l,'&nbsp;<i class="icon-arrow-right"></i>&nbsp;</td><td>',textInput(l,"",l),'</td></tr>')
      }
      attr(out, "html") <- TRUE
      out <- paste0(out, "</table>");
      HTML(out)
  })
})
