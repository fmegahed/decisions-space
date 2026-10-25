FROM rocker/shiny:4.6.1

# Install system dependencies for R packages
# - libcurl4-openssl-dev, libssl-dev: for httr2 and curl
# - libxml2-dev: for xml2
# - libmagick++-dev: for magick (decisions resizes images with it)
# - libfontconfig1-dev, libfreetype6-dev: for magick text rendering
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libmagick++-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /code

# Install pak, then decisions at a fixed commit and the other packages at fixed
# versions. The last line fails the build if any of them cannot be loaded.
RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')" \
    && R -e "pak::pak('url::https://github.com/fmegahed/decisions/archive/b25fc9ffa7908e9a6a2d9833f080de1456c9d50a.tar.gz')" \
    && R -e "pak::pak(c('shiny@1.14.0', 'mirai@2.7.1', 'dplyr@1.2.1', 'purrr@1.2.2', 'stringr@1.6.0', 'tibble@3.3.1', 'readr@2.2.0', 'jsonlite@2.0.0', 'httr2@1.2.3', 'yardstick@1.4.0', 'magick@2.9.0', 'zip@3.0.0', 'janitor@2.2.1', 'openssl@2.4.2'))" \
    && R -e "for (p in c('shiny', 'decisions', 'mirai', 'dplyr', 'purrr', 'stringr', 'tibble', 'readr', 'jsonlite', 'httr2', 'yardstick', 'magick', 'zip', 'janitor', 'openssl')) library(p, character.only = TRUE)"

# Copy only what the app needs (tests, .env and .git are excluded by .dockerignore)
COPY app.R ./
COPY R/ ./R/
COPY www/ ./www/

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=7860)"]
