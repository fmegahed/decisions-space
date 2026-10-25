# User interface: the page, its parts, and small helpers that build the
# dynamic inputs and outputs. The look is in www/theme.css, the behaviour in
# the browser (tabs, Help switch, menu, video, Copy) in www/app.js.

# The address of the video ("" for none). The page reads it through this
# function, so that a test can replace it.
video_url <- function() VIDEO_URL

# The embed address of a YouTube video, on youtube-nocookie.com ("" if the
# address holds no video id)
video_embed <- function(url) {
  id <- stringr::str_match(url, "(?:v=|youtu\\.be/|/embed/)([A-Za-z0-9_-]{11})")[, 2]
  if (is.na(id)) "" else paste0("https://www.youtube-nocookie.com/embed/", id)
}

# The data sources of the sidebar
SOURCE_CHOICES <- c(
  "Sample \u00b7 woven textiles (images, labelled)" = "textiles",
  "Sample \u00b7 injury reports (texts, labelled)" = "injuries",
  "Your own data" = "upload"
)

# The file types of the image uploads
IMAGE_ACCEPT <- paste0(".", IMAGE_EXTENSIONS)

# --- Small parts ------------------------------------------------------------

# Text for screen readers only
sr <- function(text) shiny::span(class = "sr-only", text)

# One help line of the sidebar, and one Help note of a card. The Help switch
# hides both (class "help").
help_line <- function(text) shiny::p(class = "help help-line", text)
note_box <- function(...) shiny::div(class = "howto help", shiny::strong("Help."), " ", ...)

# Radio buttons that look like a segmented control
seg_radio <- function(id, label, choices, selected = NULL, class = "seg") {
  shiny::div(
    class = class,
    shiny::radioButtons(id, sr(label), choices, selected = selected, inline = TRUE)
  )
}

# A file input. The read-only box that shows the chosen file gets a name for
# screen readers (Shiny gives it none) and no tab stop (it does nothing).
upload_input <- function(id, label, accept, multiple = FALSE, button = "Browse") {
  input <- shiny::fileInput(id, label, multiple = multiple, accept = accept, buttonLabel = button)
  htmltools::tagQuery(input)$
    find("input.form-control")$
    addAttrs(`aria-label` = "Chosen file", tabindex = "-1")$
    allTags()
}

# Small icons for the buttons of the R code card
icon_svg <- function(path) {
  htmltools::HTML(sprintf(
    '<svg class="ico" aria-hidden="true" focusable="false" viewBox="0 0 16 16" width="14" height="14"><path d="%s" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/></svg>',
    path
  ))
}
ICON_COPY <- "M5 5h8v8H5zM3 11V3h8"
ICON_DOWNLOAD <- "M8 2v8M4.5 6.5 8 10l3.5-3.5M3 13h10"

# The links of the page open in a new tab: the app runs in a frame on
# Hugging Face, and the visitor keeps the session
ext_link <- function(text, href) shiny::a(href = href, target = "_blank", rel = "noopener", text)

# --- Top bar ----------------------------------------------------------------

tab_button <- function(id, label, selected = FALSE) {
  shiny::tags$button(
    type = "button", role = "tab", id = paste0("tab-", id),
    `aria-controls` = paste0("panel-", id), `aria-selected` = tolower(selected),
    tabindex = if (selected) "0" else "-1", label
  )
}

help_switch <- function() {
  shiny::tags$label(
    class = "switch", `for` = "help_on",
    shiny::tags$input(id = "help_on", type = "checkbox", role = "switch", checked = NA),
    shiny::span(class = "track", `aria-hidden` = "true"),
    "Help"
  )
}

samples_menu <- function() {
  shiny::tags$details(
    id = "samples_menu", class = "menu",
    shiny::tags$summary(class = "btn btn-red", "Samples"),
    shiny::div(
      class = "menu-list",
      shiny::actionButton("sample_textiles", "Woven textiles (images)", class = "menu-item"),
      shiny::actionButton("sample_injuries", "Injury reports (texts)", class = "menu-item")
    )
  )
}

top_bar <- function(video) {
  shiny::tags$header(
    class = "topbar",
    shiny::div(
      class = "brand",
      shiny::img(src = "logo.svg", alt = "", class = "logo", width = "28", height = "32"),
      shiny::h1(class = "name", "decisions"),
      shiny::span(class = "tag", "Decision models")
    ),
    shiny::tags$nav(
      class = "tabs", `aria-label` = "Pages",
      shiny::div(
        role = "tablist",
        tab_button("build", "Build", TRUE), tab_button("about", "About"),
        tab_button("record", "Record")
      )
    ),
    shiny::div(
      class = "topbar-right",
      help_switch(),
      shiny::span(
        class = "chip", shiny::span(class = "dot", `aria-hidden` = "true", "\u25cf"), " ",
        shiny::textOutput("status_chip", inline = TRUE)
      ),
      if (nzchar(video)) {
        shiny::tags$button(
          id = "open_video", type = "button", class = "btn btn-quiet",
          `aria-haspopup` = "dialog", "Video"
        )
      },
      shiny::div(
        class = "import-btn",
        upload_input("import_file", NULL, c(".zip", ".json"), button = "Import")
      ),
      samples_menu()
    )
  )
}

# --- Build tab: sidebar -----------------------------------------------------

# A numbered group of the sidebar, such as "01 - Data"
group <- function(num, title, ...) {
  shiny::div(class = "group", shiny::h2(class = "eyebrow", shiny::tags$b(num), " \u00b7 ", title), ...)
}

sidebar <- function() {
  textiles <- sample_question("textiles")
  shiny::tags$aside(
    class = "card sidebar", `aria-label` = "Setup",
    group(
      "01", "Data",
      seg_radio("mode", "Kind of data", c(Images = "image", Text = "text")),
      shiny::selectInput("source", sr("Source"), SOURCE_CHOICES, selectize = FALSE),
      shiny::conditionalPanel(
        "input.source == 'upload' && input.mode == 'image'",
        upload_input("upload_images", "Images or zip file", c(IMAGE_ACCEPT, ".zip"), multiple = TRUE)
      ),
      shiny::conditionalPanel(
        "input.source == 'upload' && input.mode == 'text'",
        shiny::textAreaInput("paste_text", "Texts", rows = 4),
        upload_input("upload_csv", "CSV file", ".csv"),
        shiny::selectInput("csv_text_col", "Text column", character(), selectize = FALSE),
        shiny::selectInput("csv_label_col", "Label column", character(), selectize = FALSE)
      ),
      help_line(help_text("data"))
    ),
    group(
      "02", "Question",
      seg_radio(
        "qtype", "Type of question",
        c("Yes / no" = "noul", Choice = "choice", Score = "score"), selected = textiles$type
      ),
      shiny::textAreaInput("instructions", "Instructions", textiles$instructions, rows = 2),
      shiny::uiOutput("answers_ui"),
      help_line(help_text("question"))
    ),
    shiny::conditionalPanel(
      "input.mode == 'image'",
      group(
        "03", "References",
        shiny::conditionalPanel("input.qtype != 'score'", help_line(help_text("references"))),
        shiny::conditionalPanel("input.qtype == 'score'", help_line(help_text("references_score"))),
        shiny::uiOutput("references_ui")
      )
    ),
    shiny::div(
      class = "run-row",
      shiny::actionButton("try5", "Try on 5", class = "btn-ghost"),
      shiny::actionButton(
        "run_all", shiny::tagList("Run all ", shiny::textOutput("run_estimate", inline = TRUE)),
        class = "btn-red"
      )
    ),
    shiny::div(class = "stop-row", shiny::actionButton("stop", "Stop", class = "btn-ghost")),
    htmltools::tagAppendChild(shiny::uiOutput("spend_meter"), spend_meter_ui(0)),
    shiny::p(class = "privacy", help_text("privacy")),
    shiny::div(class = "limits", role = "status", shiny::textOutput("limits_text", inline = TRUE))
  )
}

# --- Build tab: cards -------------------------------------------------------

intro_card <- function() {
  step <- \(n, name, text) {
    shiny::tags$li(
      shiny::span(class = "num", `aria-hidden` = "true", n),
      shiny::span(shiny::strong(name), " \u00b7 ", text)
    )
  }
  shiny::tags$section(
    id = "intro", class = "card intro", `aria-label` = "Intro",
    shiny::div(
      shiny::p(class = "intro-lead", "Ask a model a question about each image or text."),
      shiny::p(class = "intro-help", help_text("intro"), " ", shiny::actionLink("intro_hide", "Hide"))
    ),
    shiny::tags$ol(
      class = "steps",
      step("1", "Question", "a question, its answers and a few examples"),
      step(
        "2", "Answers",
        "an answer and a probability for each input; with labels, how often Jev is right"
      ),
      step("3", "R code", "the same run in R, to copy or download")
    )
  )
}

# A region that scrolls sideways; it takes keyboard focus, so that the arrow
# keys can scroll it
scroll_region <- function(label, ...) {
  shiny::div(class = "table-wrap", tabindex = "0", role = "region", `aria-label` = label, ...)
}

answers_card <- function() {
  shiny::tags$section(
    class = "card answers-card", `aria-labelledby` = "answers_title",
    shiny::div(
      class = "hd",
      shiny::h2(
        id = "answers_title", class = "card-title", tabindex = "-1",
        shiny::span(class = "eyebrow-num", `aria-hidden` = "true", "1"), "Answers"
      ),
      shiny::div(
        class = "hd-tools",
        shiny::selectInput("target", "Target", character(), selectize = FALSE, width = "auto"),
        seg_radio("view", "View", c(Gallery = "gallery", Table = "table"), class = "seg vtabs")
      )
    ),
    shiny::div(
      class = "bd",
      shiny::uiOutput("stats"),
      note_box(shiny::span(id = "help_answers", class = "shiny-text-output", help_text("answers"))),
      shiny::conditionalPanel("input.view == 'gallery'", shiny::uiOutput("gallery")),
      shiny::conditionalPanel(
        "input.view == 'table'",
        scroll_region("Answers table", shiny::tableOutput("table"))
      ),
      shiny::div(
        class = "card-foot",
        shiny::downloadButton("download_answers", "Answers .csv", class = "btn-quiet sm", icon = NULL)
      )
    )
  )
}

# Server: shiny::outputOptions(output, id, suspendWhenHidden = FALSE) for "code", "bundle_files", "code_summary"
code_card <- function() {
  code <- htmltools::tagAppendAttributes(
    shiny::verbatimTextOutput("code", placeholder = FALSE),
    class = "code", tabindex = "0", role = "region", `aria-label` = "R code of this run"
  )
  shiny::tags$section(
    class = "card code-card", `aria-label` = "R code",
    # The summary holds only the summary line; the buttons sit outside it
    shiny::tags$details(
      class = "code-details",
      shiny::tags$summary(
        class = "code-summary",
        shiny::span(class = "eyebrow-num", `aria-hidden` = "true", "2"),
        shiny::span(id = "code_summary", class = "shiny-text-output", "R code")
      ),
      shiny::div(class = "bd", note_box(help_text("code")), code, shiny::uiOutput("bundle_files"))
    ),
    shiny::div(
      class = "code-actions",
      shiny::tags$button(
        type = "button", class = "btn btn-quiet sm copy-btn", `data-target` = "code",
        icon_svg(ICON_COPY), shiny::span(class = "copy-label", "Copy")
      ),
      shiny::downloadButton(
        "download_code", shiny::tagList(icon_svg(ICON_DOWNLOAD), "Download .R"),
        class = "btn-quiet sm", icon = NULL
      ),
      shiny::downloadButton(
        "download_bundle", shiny::tagList(icon_svg(ICON_DOWNLOAD), "Bundle .zip"),
        class = "btn-red sm", icon = NULL
      )
    ),
    shiny::div(id = "copy_status", class = "sr-only", role = "status")
  )
}

build_panel <- function() {
  shiny::div(
    class = "build", sidebar(),
    shiny::div(class = "cards", intro_card(), answers_card(), code_card())
  )
}

# --- About and Record tabs --------------------------------------------------

about_panel <- function() {
  # The About text is Markdown; its links open in a new tab
  html <- shiny::markdown(help_text("about")) |>
    as.character() |>
    stringr::str_replace_all("<a href=", '<a target="_blank" rel="noopener" href=')
  shiny::div(class = "page card", shiny::div(class = "bd prose", htmltools::HTML(html)))
}

record_panel <- function() {
  shiny::div(
    class = "page card",
    shiny::div(
      class = "bd prose",
      shiny::h2("Record"),
      shiny::p(help_text("record")),
      scroll_region("Record table", shiny::tableOutput("record_table")),
      shiny::p(
        ext_link("Status file", LINKS$status), " \u00b7 ",
        ext_link("Package record", LINKS$package_record), " \u00b7 ",
        ext_link("Project record", LINKS$project_record)
      )
    )
  )
}

tab_panel <- function(id, content, selected = FALSE) {
  shiny::tags$section(
    id = paste0("panel-", id), role = "tabpanel", `aria-labelledby` = paste0("tab-", id),
    hidden = if (!selected) NA, content
  )
}

# --- Footer and video ---------------------------------------------------------

footer_bar <- function() {
  shiny::tags$footer(
    class = "foot",
    shiny::span(sprintf(
      "decisions-space %s \u00b7 decisions %s \u00b7 %s", APP_VERSION, decisions_version(), MODEL
    )),
    shiny::span(
      AUTHORS, " \u00b7 ", ext_link("Source", LINKS$source), " \u00b7 ",
      ext_link("MIT", LINKS$license)
    )
  )
}

# The video dialog (only when there is a video). The iframe gets its address
# from data-src when the dialog opens, so nothing loads before a click.
video_dialog <- function(url) {
  shiny::tags$dialog(
    id = "video_dialog", class = "video-dialog", `aria-labelledby` = "video_title",
    shiny::div(
      class = "video-head",
      shiny::h2(id = "video_title", "Video"),
      shiny::tags$button(id = "close_video", type = "button", class = "btn btn-quiet sm", "Close")
    ),
    shiny::tags$iframe(
      id = "video_frame", class = "video-frame", title = "Video of the decisions app",
      `data-src` = video_embed(url), allow = "autoplay; encrypted-media; picture-in-picture; fullscreen",
      referrerpolicy = "strict-origin-when-cross-origin"
    )
  )
}

# --- The page -----------------------------------------------------------------

# The page. Shiny passes the HTTP request; the page does not use it.
app_ui <- function(request) {
  video <- video_url()
  shiny::bootstrapPage(
    title = "decisions", lang = "en",
    shiny::tags$head(
      shiny::tags$meta(name = "app-version", content = APP_VERSION),
      shiny::tags$link(rel = "icon", type = "image/png", href = "logo.png"),
      shiny::tags$link(rel = "stylesheet", href = "theme.css")
    ),
    top_bar(video),
    shiny::tags$main(
      id = "main",
      tab_panel("build", build_panel(), selected = TRUE),
      tab_panel("about", about_panel()),
      tab_panel("record", record_panel())
    ),
    footer_bar(),
    if (nzchar(video)) video_dialog(video),
    shiny::tags$script(src = "app.js")
  )
}
