# Configuration: the single place for the model id, the limits and the links.
# Everything a user reads about the model or the limits comes from these
# values (see app_config() at the end), so the text cannot drift from the
# behaviour.

# Version of this app
APP_VERSION <- "1.0.0"
APP_DATE <- "24 October 2026"

# Model used for every call. This is the ONLY place a model id may be written
# (tests/testthat/test-config.R enforces this).
MODEL <- "jev-1.14.0"

# Commit of the decisions package that the app uses
DECISIONS_SHA <- "b25fc9ffa7908e9a6a2d9833f080de1456c9d50a"

# Price in USD for each input token (USD 0.042 for each million tokens)
PRICE_PER_TOKEN <- 0.042 / 1e6

# Spending limits in USD (all days and months are in UTC)
LIMIT_SESSION <- 0.25
LIMIT_DAY <- 2
LIMIT_MONTH <- 20

# Token guesses for the cost check before the first call
TOKENS_GUESS <- c(image = 16000, text = 1000)

# Input limits
MAX_FILE_MB <- 5
MAX_ZIP_MB <- 50
MAX_IMAGES <- 100
MAX_TEXTS <- 200
MAX_TEXT_CHARS <- 4000
MAX_INSTRUCTIONS <- 500
MAX_DESCRIPTION <- 300

# Number of answers for a choice question and of levels for a score question
ANSWER_RANGE <- list(choice = c(2, 6), score = c(2, 5))

# Reference images for each answer, and inputs for each call to Jev
MAX_REFERENCES <- 2
CHUNK_SIZE <- 10

# Image settings that decisions applies before it sends an image
IMAGE_SETTINGS <- list(max_size = 768, upscale = TRUE, crop = "none")

# Names of the environment variables (the values are never in the code)
KEY_VAR <- "HF_JEV_API"
USAGE_TOKEN_VAR <- "HF_USAGE_TOKEN"

# Private dataset that stores the usage, and the public status file
USAGE_REPO <- "fmegahed/decisions-space-usage"
STATUS_URL <- "https://raw.githubusercontent.com/fmegahed/decisions-space/main/status/status.csv"

# Address of the Space, and the video (empty until the video exists)
SPACE_HOST <- "https://fmegahed-decisions.hf.space"
VIDEO_URL <- ""

# The values that the help text uses, in one named list
app_config <- function() {
  list(
    version = APP_VERSION, date = APP_DATE, model = MODEL,
    price_per_million = PRICE_PER_TOKEN * 1e6,
    limit_session = LIMIT_SESSION, limit_day = LIMIT_DAY,
    limit_month = LIMIT_MONTH, tokens_guess = TOKENS_GUESS,
    max_file_mb = MAX_FILE_MB, max_zip_mb = MAX_ZIP_MB,
    max_images = MAX_IMAGES, max_texts = MAX_TEXTS,
    max_text_chars = MAX_TEXT_CHARS, max_instructions = MAX_INSTRUCTIONS,
    max_description = MAX_DESCRIPTION, answer_range = ANSWER_RANGE,
    max_references = MAX_REFERENCES, chunk_size = CHUNK_SIZE,
    image_settings = IMAGE_SETTINGS, space_host = SPACE_HOST,
    video_url = VIDEO_URL
  )
}
