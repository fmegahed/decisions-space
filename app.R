# App logic lives in R/ and is sourced here in a fixed order
# (R/_disable_autoload.R stops Shiny from sourcing the directory a second time)
for (r_file in c("config.R", "spec.R", "samples.R", "data.R", "runs.R")) {
  source(file.path("R", r_file), local = TRUE)
}

# Uploads larger than this are rejected by Shiny
options(shiny.maxRequestSize = MAX_ZIP_MB * 1024^2)

# Placeholder page: the real interface comes in later tasks
ui <- shiny::fluidPage(
  title = "decisions",
  shiny::tags$head(
    shiny::tags$meta(name = "app-version", content = APP_VERSION)
  ),
  shiny::tags$header(shiny::tags$h1("decisions"))
)

server <- function(input, output, session) {}

shiny::shinyApp(ui, server)
