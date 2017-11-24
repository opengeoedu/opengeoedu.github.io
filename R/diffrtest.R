install.packages("diffr")
library(diffr)
?diffr

library(diffr)
library(shiny)
file1 = tempfile()
writeLines("hello, world!\n", con = file1)
file2 = tempfile()
writeLines(paste0(
  "hello world?\nI don't get it\n",
  paste0(sample(letters, 65, replace = TRUE), collapse = "")), con = file2)

file1 <- file("data/portale_geocoded3.csv")
file2 <- file("data/OLD/portale_geocoded2 (Kopie).csv")

ui <- fluidPage(
  h1("A diffr demo"),
  checkboxInput("wordWrap", "Word Wrap",
                value = TRUE),
  diffrOutput("exdiff")
)

server <- function(input, output, session) {
  output$exdiff <- renderDiffr({
    diffr(file1, file2, wordWrap = input$wordWrap,
          before = "f1", after = "f2")
  })
}

shinyApp(ui, server)
