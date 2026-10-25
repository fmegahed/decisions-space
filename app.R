# App logic lives in R/ and is sourced here in a fixed order
# (R/_disable_autoload.R stops Shiny from sourcing the directory a second time)
for (r_file in c("config.R", "spec.R", "samples.R", "data.R", "runs.R", "spending.R", "answers.R", "code.R", "bundle.R", "status.R", "help.R", "render.R", "ui.R")) {
  source(file.path("R", r_file), local = TRUE)
}

# Uploads larger than this are rejected by Shiny
options(shiny.maxRequestSize = MAX_ZIP_MB * 1024^2)

# The page comes from app_ui() (R/ui.R). The server does nothing yet: it
# comes in a later task.
server <- function(input, output, session) {}

shiny::shinyApp(app_ui, server)
