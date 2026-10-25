# Samples: the two runs of the vignettes of decisions, with their saved answers.
# The inputs and the answers come from the installed package, so the app and
# the vignettes cannot drift apart.

# Numbers of the saved runs and the sentences that the help text shows
SAMPLE_FACTS <- list(
  textiles = c(auc = 0.75, accuracy = 0.80),
  injuries = c(accuracy = 0.96)
)
SAMPLE_FACTS_TEXT <- list(
  textiles = c(
    sprintf(
      "The saved run has an AUC of %.2f and an accuracy of %.2f.",
      SAMPLE_FACTS$textiles[["auc"]], SAMPLE_FACTS$textiles[["accuracy"]]
    ),
    "Jev puts all 14 local flaws (one broken spot) at the top.",
    "The 9 global flaws, where the whole pattern is distorted, score like regular weaves (0.14 to 0.26).",
    "A sharper description of \"defective\" or another reference can help there."
  ),
  injuries = sprintf(
    "The saved run has an accuracy of %.2f.", SAMPLE_FACTS$injuries[["accuracy"]]
  )
)

# The questions, copied from the vignettes images.Rmd and choosing-a-model.Rmd
sample_question <- function(id) {
  switch(id,
    textiles = list(
      type = "choice",
      instructions = "Does the woven pattern in `inspection_image` have a flaw?",
      answers = c(
        nominal = "The weave is regular. The pattern repeats with no break.",
        defective = "The weave has a flaw. A part of the pattern is broken, or the whole pattern is distorted."
      )
    ),
    injuries = list(
      type = "choice",
      instructions = "Which injury does this report of a work injury describe? Choose the main injury.",
      answers = c(
        amputation = "A body part is cut or torn off, in whole or in part.",
        fracture = "A bone is broken or cracked.",
        `crushing injury` = "A body part is squeezed hard between objects, and the tissue is damaged.",
        `thermal burn` = "Heat, flame, hot liquid or a hot surface burns the skin.",
        concussion = "A blow or a shake to the head hurts the brain."
      )
    )
  )
}

# Folder of the textile images in the installed package
textiles_folder <- function() {
  system.file("extdata", "textiles", package = "decisions")
}

# The spec of a sample, id "textiles" or "injuries"
sample_spec <- function(id) {
  id <- rlang::arg_match(id, c("textiles", "injuries"))
  if (id == "textiles") {
    files <- decisions::textiles
    inputs <- files |>
      dplyr::filter(role == "input") |>
      dplyr::transmute(
        id = file, path = file.path(textiles_folder(), file),
        text = NA_character_, file = file, label = as.character(class)
      )
    refs <- files |>
      dplyr::filter(role == "reference") |>
      dplyr::arrange(class, reference_rank) |>
      dplyr::mutate(path = file.path(textiles_folder(), file))
    references <- list(
      nominal = refs$path[refs$class == "nominal"],
      defective = refs$path[refs$class == "defective"]
    )
    mode <- "image"
  } else {
    saved <- readRDS(system.file("extdata", "choosing-a-model.rds", package = "decisions"))
    inputs <- saved$injuries |>
      dplyr::transmute(
        id = id, path = NA_character_, text = final_narrative, file = id,
        label = as.character(collapsed_nature_title)
      )
    references <- NULL
    mode <- "text"
  }
  list(
    mode = mode, question = sample_question(id), references = references,
    inputs = inputs, source = id, name = id
  )
}

# The saved output of decide() for Jev, with the id of each input
sample_answers <- function(id) {
  id <- rlang::arg_match(id, c("textiles", "injuries"))
  saved <- readRDS(system.file(
    "extdata", if (id == "textiles") "images.rds" else "choosing-a-model.rds",
    package = "decisions"
  ))
  stopifnot(nrow(saved$answers$jev) == nrow(sample_spec(id)$inputs))
  saved$answers$jev |>
    dplyr::mutate(id = sample_spec(id)$inputs$id, .before = 1)
}

# The two samples, read once for each R process
SAMPLE_CACHE <- new.env(parent = emptyenv())
sample_data <- function(id) {
  SAMPLE_CACHE[[id]] <- SAMPLE_CACHE[[id]] %||%
    list(spec = sample_spec(id), answers = sample_answers(id))
  SAMPLE_CACHE[[id]]
}
