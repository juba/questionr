library(shiny)
library(highr)

## Global variables
## Original data frame name and object
df_name <- get(".questionr_iorder_df", .GlobalEnv)
df <- get(df_name)
## Variable to be recoded, name and object
oldvar_name <- get(".questionr_iorder_oldvar", .GlobalEnv)
oldvar <- df[,oldvar_name]
## Formatted source variable name
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))


shinyServer(function(input, output) {

    ## Generate reordering code
    generate_code <- function(newvar_name) {
        ## if null, create temporary variable for check table
        if (is.null(newvar_name)) dest_var <- ".iorder_tmp"
        ## else, format new variable for code
        else
            dest_var <- ifelse(grepl(" ", newvar_name),
                               sprintf('%s[,"%s"]', df_name, newvar_name),
                               sprintf('%s$%s', df_name, newvar_name))
        newlevels <- paste0(capture.output(dput(input$sortable)), collapse="")
	out <- sprintf("## Reordering %s", src_var)
	if (src_var != dest_var) out <- paste0(out, sprintf(" into %s", dest_var))
        out <- paste0(out, sprintf("\n%s <- factor(%s, levels=", dest_var, src_var))
        out <- paste0(out, newlevels, ')')
        out
    }

    ## Generate the code in the interface
    output$codeOut <- renderText({
        ## Header
        header <- HTML(paste0("<p class='header'>Reordering <tt>", oldvar_name, "</tt> from <tt>", df_name, "</tt> of class <tt>", class(oldvar), "</tt>.</p>"))
        ## Generate code
        out <- generate_code(input$newvarname)
        ## If "Done" button is pressed, exit and cat generated code in the console
        if (input$closebutton > 0) {
            cat("\n-------- Start reordering code --------\n\n")
            cat(out)
            cat("\n--------- End reordering code ---------\n")
            shiny::stopApp()
        }
        ## Generated code syntax highlighting
        out <- paste(hi_html(out), collapse="\n")
        ## Final paste
        out <- paste0(header, "<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
        out
    })

    ## Generate the check table
    output$tableOut <- renderTable({
        ## Generate the recoding code with a temporary variable
        code <- generate_code(newvar_name=NULL)
        ## Eval generated code
        eval(parse(text=code))
        ## Display table
        tab <- freq(.iorder_tmp)
        tab
    })

    ## Text fileds for levels, dynamically generated
    output$levelsInput <- renderUI({
        out <- "<ol class='sortable'>"
        ## List of levels
        if (is.factor(oldvar)) levs <- levels(oldvar)
        else levs <- na.omit(unique(oldvar))
        ## Generate fields
        for (l in levs) out <- paste0(out,'<li>',l,'</li>')
        out <- paste0(out, "</ol>")
        HTML(out)
    })
})


