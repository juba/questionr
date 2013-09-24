library(shiny)

df <- get("*questionr_grec_tmp_df*")
oldvar <- (df[,`*questionr_grec_tmp_oldvar*`])

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Graphical recoding"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    uiOutput("levelsInput")
   ),

  # Show a plot of the generated distribution
  mainPanel(
    textOutput("recodeOut")
  )
))
