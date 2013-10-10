library(shiny)
library(highr)

df_name <- get("*questionr_irec_df*", .GlobalEnv)
df <- get(df_name)
oldvar_name <- get("*questionr_irec_oldvar*", .GlobalEnv)
oldvar <- df[,oldvar_name]
src_var <- ifelse(grepl(" ", oldvar_name),
                  sprintf('%s[,"%s"]', df_name, oldvar_name),
                  sprintf('%s$%s', df_name, oldvar_name))


shinyServer(function(input, output) {


    get_value_character<- function(val) {
        if (val %in% c("NA", "TRUE", "FALSE")) return(val)
        ## TODO : find a way to escape double quotes
        val <- gsub('"', "", val)
        val <- paste0('"',val,'"')
        val
    }

    get_value_factor<- function(val) {
        if (val=="NA") return(NA)
        if (val=="TRUE") return(TRUE)
        if (val=="FALSE") return(FALSE)
        val
    }
    
    generate_code_factor <- function(newvar_name, dest_var) {
        out <- NULL
        oldfac <- factor(oldvar, exclude=NULL)
        levs <- levels(oldfac)
        labels <- NULL
        for (l in levs) {
            if (is.na(l)) l <- "*irec_NA_id*"
            value <- get_value_factor(input[[l]])
            labels <- c(labels, value)
        }
        out <- paste0(out, sprintf("%s <- factor(%s, exclude=NULL,\n", dest_var, src_var))
        out <- paste0(out, sprintf("           levels=%s,\n", capture.output(dput(levs))))
        out <- paste0(out, sprintf("           labels=%s)\n", capture.output(dput(labels))))
        out
    }

    generate_code_character <- function(newvar_name, dest_var, style) {
        out <- NULL
        if (!is.character(oldvar))
            out <- paste0(out, sprintf("%s <- as.character(%s)\n", dest_var, src_var))
        else
            out <- paste0(out, sprintf("%s <- %s\n", dest_var, src_var))
        if (is.factor(oldvar)) levs <- levels(oldvar)
        else levs <- na.omit(unique(oldvar))
        if (any(is.na(oldvar))) levs <- c(levs, NA)
        for (l in levs) {
            if (is.na(l)) l <- "*irec_NA_id*"
            value <- get_value_character(input[[l]])
            if (style=="min") {
                if (l==input[[l]]) next
                if (l=="*irec_NA_id*" && value=="NA") next
            }
            if (l!="*irec_NA_id*")
                out <- paste0(out, sprintf('%s[%s == "%s"] <- %s\n', dest_var, src_var, l, value))
            else
                out <- paste0(out, sprintf('%s[is.na(%s)] <- %s\n', dest_var, src_var, value))
        }
        out
    }

    generate_code <- function(newvar_name) {
        if (is.null(newvar_name)) dest_var <- "irec_tmp"
        else
            dest_var <- ifelse(grepl(" ", newvar_name),
                               sprintf('%s[,"%s"]', df_name, newvar_name),
                               sprintf('%s$%s', df_name, newvar_name))
        recstyle <- input$recstyle
        if(recstyle=="default") {
            if(is.factor(oldvar)) return(generate_code_factor(newvar_name, dest_var))
            else return(generate_code_character(newvar_name, dest_var, style="comp"))
        }
        if(recstyle=="factor") return(generate_code_factor(newvar_name, dest_var))
        if(recstyle=="charcomp") return(generate_code_character(newvar_name, dest_var, style="comp"))
        if(recstyle=="charmin") return(generate_code_character(newvar_name, dest_var, style="min"))
    }


    
    output$recodeOut <- renderText({
        out <- generate_code(input$newvarname)
        ## for (l in levels(oldvar)) {
        ##     out <- paste0(out, 'for (i in c(1:10, TRUE)) ', l, " <- \"", input[[l]],"\"\n")
        ## }
        out <- paste(hi_html(out), collapse="\n")
        out <- paste0("<pre class='r'><code class='r' id='codeout'>",out,"</code></pre>")
        out
    })

    output$tableOut <- renderTable({
        code <- generate_code(newvar_name=NULL)
        eval(parse(text=code))
        tab <- table(oldvar, irec_tmp, useNA="always")
        rownames(tab)[is.na(rownames(tab))] <- "NA"
        colnames(tab)[is.na(colnames(tab))] <- "NA"
        tab
    })
    
    output$levelsInput <- renderUI({
        out <- "<table>"
        if (is.factor(oldvar)) levs <- levels(oldvar)
        else levs <- na.omit(unique(oldvar))
        if (any(is.na(oldvar))) levs <- c(levs, NA)
        for (l in levs) {
            out <- paste0(out,'<tr><td style="text-align: right;">',l,'</td>')
            out <- paste0(out,'<td>&nbsp;<i class="icon-arrow-right"></i>&nbsp;</td>')
            out <- paste0(out,'<td>',textInput(ifelse(is.na(l), "*irec_NA_id*",l),"", ifelse(is.na(l), "NA",l)),'</td>')
            ## out <- paste0(out,'<td class="checkquote">')
            ## out <- paste0(out, checkboxInput(paste0(l,'-quote')," disable quotes",FALSE),'</td>')
            out <- paste0(out,'</tr>')
        }
        out <- paste0(out, "</table>")
        HTML(out)
    })
})
