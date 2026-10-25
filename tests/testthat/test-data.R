# Helpers: tiny JPEGs made with magick, zip files made with zip::zip
make_jpg <- function(path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  magick::image_blank(8, 8, "red") |> magick::image_write(path, format = "jpeg")
  invisible(path)
}

# Make a zip file with the entries `rel` (relative paths). A `.jpg` gets a
# real image, any other entry gets a few bytes. Returns the zip path.
make_zip <- function(rel, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  for (r in rel) {
    p <- file.path(root, r)
    dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
    if (grepl("\\.jpe?g$", r) && !grepl("^__MACOSX", r)) make_jpg(p) else writeBin(as.raw(1:5), p)
  }
  zf <- withr::local_tempfile(fileext = ".zip", .local_envir = env)
  zip::zip(zf, files = rel, root = root, mode = "mirror")
  zf
}

as_files <- function(paths, names = basename(paths)) {
  data.frame(name = names, datapath = paths)
}

test_that("a labelled zip gives labelled inputs", {
  zf <- make_zip(c("nominal/a.jpg", "defective/b.jpg"))
  out <- read_images(as_files(zf, "set.zip"))
  expect_identical(nrow(out$inputs), 2L)
  expect_setequal(out$inputs$label, c("nominal", "defective"))
  expect_identical(out$problems, character())
  expect_true(all(file.exists(out$inputs$path)))
  expect_true(all(is.na(out$inputs$text)))
  expect_named(out$inputs, c("id", "path", "text", "file", "label"))
})

test_that("a single top folder around the label folders is unwrapped", {
  zf <- make_zip(c("parts/nominal/a.jpg", "parts/defective/b.jpg"))
  out <- read_images(as_files(zf, "set.zip"))
  expect_setequal(out$inputs$label, c("nominal", "defective"))
})

test_that("a single label folder is not unwrapped", {
  zf <- make_zip(c("nominal/a.jpg", "nominal/b.jpg"))
  out <- read_images(as_files(zf, "set.zip"))
  expect_identical(unique(out$inputs$label), "nominal")
})

test_that("a zip with no folders and loose files are unlabelled", {
  zf <- make_zip(c("a.jpg", "b.jpg"))
  out <- read_images(as_files(zf, "set.zip"))
  expect_identical(nrow(out$inputs), 2L)
  expect_true(all(is.na(out$inputs$label)))
  j <- make_jpg(withr::local_tempfile(fileext = ".jpg"))
  loose <- read_images(as_files(j, "photo.jpg"))
  expect_identical(loose$inputs$file, "photo.jpg")
  expect_true(is.na(loose$inputs$label))
})

test_that("junk entries are skipped and not counted", {
  zf <- make_zip(c(
    "nominal/a.jpg", "__MACOSX/nominal/._a.jpg", "nominal/.DS_Store",
    "nominal/Thumbs.db", ".hidden.jpg"
  ))
  out <- read_images(as_files(zf, "set.zip"))
  expect_identical(nrow(out$inputs), 1L)
  expect_identical(out$problems, character())
})

test_that("the same name in two folders gives distinct ids", {
  zf <- make_zip(c("nominal/a.jpg", "defective/a.jpg"))
  out <- read_images(as_files(zf, "set.zip"))
  expect_identical(nrow(out$inputs), 2L)
  expect_identical(anyDuplicated(out$inputs$id), 0L)
  expect_identical(out$inputs$file, c("a.jpg", "a.jpg"))
})

test_that("two uploaded files with the same name give distinct ids", {
  j1 <- make_jpg(withr::local_tempfile(fileext = ".jpg"))
  j2 <- make_jpg(withr::local_tempfile(fileext = ".jpg"))
  out <- read_images(as_files(c(j1, j2), c("a.jpg", "a.jpg")))
  expect_identical(anyDuplicated(out$inputs$id), 0L)
})

test_that("a name with a space and a non-ASCII letter works", {
  zf <- make_zip(c("D\u00e9faut/d\u00e9faut 1.jpg", "nominal/ok 2.jpg"))
  out <- read_images(as_files(zf, "set.zip"))
  expect_identical(nrow(out$inputs), 2L)
  expect_true("d\u00e9faut 1.jpg" %in% out$inputs$file)
  expect_true(all(file.exists(out$inputs$path)))
  expect_false(any(grepl(" ", out$inputs$id)))
  expect_identical(anyDuplicated(out$inputs$id), 0L)
})

test_that("more than the maximum of images loads nothing", {
  d <- withr::local_tempdir()
  rel <- sprintf("img%03d.jpg", seq_len(MAX_IMAGES + 1))
  make_jpg(file.path(d, rel[1]))
  for (r in rel[-1]) file.copy(file.path(d, rel[1]), file.path(d, r))
  zf <- withr::local_tempfile(fileext = ".zip")
  zip::zip(zf, files = rel, root = d, mode = "mirror")
  out <- read_images(as_files(zf, "set.zip"))
  expect_identical(nrow(out$inputs), 0L)
  expect_match(out$problems, "at most 100 images")
})

test_that("junk does not count toward the maximum of images", {
  d <- withr::local_tempdir()
  make_jpg(file.path(d, "img.jpg"))
  rel <- c("img.jpg", sprintf("junk%03d/.DS_Store", seq_len(MAX_IMAGES)))
  for (r in rel[-1]) {
    dir.create(file.path(d, dirname(r)), showWarnings = FALSE)
    writeBin(as.raw(1), file.path(d, r))
  }
  zf <- withr::local_tempfile(fileext = ".zip")
  zip::zip(zf, files = rel, root = d, mode = "mirror")
  expect_identical(nrow(read_images(as_files(zf, "set.zip"))$inputs), 1L)
})

test_that("a file over the size limit is skipped and the rest loads", {
  d <- withr::local_tempdir()
  make_jpg(file.path(d, "small.jpg"))
  writeBin(raw((MAX_FILE_MB + 1) * 1024^2), file.path(d, "big.jpg"))
  out <- read_images(as_files(file.path(d, c("small.jpg", "big.jpg")), c("small.jpg", "big.jpg")))
  expect_identical(out$inputs$file, "small.jpg")
  expect_match(out$problems, "at most 5 MB")
  zf <- withr::local_tempfile(fileext = ".zip")
  zip::zip(zf, files = c("small.jpg", "big.jpg"), root = d, mode = "mirror")
  out_zip <- read_images(as_files(zf, "set.zip"))
  expect_identical(out_zip$inputs$file, "small.jpg")
  expect_match(out_zip$problems, "at most 5 MB")
})

test_that("a zip over the size limit is skipped", {
  zf <- withr::local_tempfile(fileext = ".zip")
  writeBin(raw((MAX_ZIP_MB + 1) * 1024^2), zf)
  out <- read_images(as_files(zf, "big.zip"))
  expect_identical(nrow(out$inputs), 0L)
  expect_match(out$problems, "at most 50 MB")
})

test_that("a file that is not an image is skipped with a problem", {
  zf <- make_zip(c("nominal/a.jpg", "nominal/notes.pdf"))
  out <- read_images(as_files(zf, "set.zip"))
  expect_identical(nrow(out$inputs), 1L)
  expect_match(out$problems, "1 file is not an image")
  expect_match(
    read_images(as_files(make_zip(c("a.jpg", "b.pdf", "c.txt")), "s.zip"))$problems,
    "2 files are not images"
  )
})

test_that("a broken zip gives a problem and no crash", {
  bad <- withr::local_tempfile(fileext = ".zip")
  writeLines("not a zip", bad)
  out <- read_images(as_files(bad, "bad.zip"))
  expect_identical(nrow(out$inputs), 0L)
  expect_match(out$problems, "bad.zip")
})

test_that("unsafe zip entries are ignored", {
  expect_true(safe_entry("nominal/a.jpg"))
  expect_false(safe_entry("../evil.jpg"))
  expect_false(safe_entry("a/../../evil.jpg"))
  expect_false(safe_entry("/etc/evil.jpg"))
  expect_false(safe_entry("C:/evil.jpg"))
})

test_that("labels match the answers after squish and lower case", {
  zf <- make_zip(c("DEFECTIVE/a.jpg", "Nominal/b.jpg", "scratch/c.jpg"))
  inputs <- read_images(as_files(zf, "set.zip"))$inputs
  out <- match_labels(inputs, c("nominal", "defective"))
  expect_setequal(out$inputs$label[!is.na(out$inputs$label)], c("nominal", "defective"))
  expect_identical(sum(is.na(out$inputs$label)), 1L)
  expect_identical(nrow(out$inputs), 3L)
  expect_identical(out$problems, "scratch is not an answer")
})

test_that("match_labels lists each unmatched label once and keeps NA", {
  inputs <- tibble::tibble(
    id = as.character(1:4), path = NA_character_, text = letters[1:4],
    file = as.character(1:4), label = c("x", "x", NA, " Nominal  ")
  )
  out <- match_labels(inputs, c(nominal = "A part.", defective = "B part."))
  expect_identical(out$inputs$label, c(NA, NA, NA, "nominal"))
  expect_identical(out$problems, "x is not an answer")
})

test_that("pasted text gives one input for each paragraph", {
  out <- read_text_paste("First one.\n\n\n  \nSecond one,\nsame paragraph.\r\n\r\nThird.\n\n")
  expect_identical(nrow(out$inputs), 3L)
  expect_identical(out$inputs$text[2], "Second one,\nsame paragraph.")
  expect_true(all(is.na(out$inputs$path)) && all(is.na(out$inputs$label)))
  expect_identical(anyDuplicated(out$inputs$id), 0L)
  expect_identical(out$problems, character())
  expect_identical(nrow(read_text_paste("")$inputs), 0L)
})

test_that("a text over the limit is skipped and the rest loads", {
  long <- strrep("a", MAX_TEXT_CHARS + 1)
  out <- read_text_paste(paste("short", long, "other", sep = "\n\n"))
  expect_identical(nrow(out$inputs), 2L)
  expect_match(out$problems, "at most 4,000 characters")
})

test_that("more than the maximum of texts loads nothing", {
  out <- read_text_paste(paste(seq_len(MAX_TEXTS + 1), collapse = "\n\n"))
  expect_identical(nrow(out$inputs), 0L)
  expect_match(out$problems, "at most 200 texts")
})

test_that("a CSV with BOM, an empty text and a blank label reads", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(
    as.raw(c(0xEF, 0xBB, 0xBF)),
    charToRaw("text,label\nfirst,nominal\nsecond,\n,defective\n\n")
  ), f)
  expect_identical(csv_columns(f), c("text", "label"))
  out <- read_text_csv(f, "text", "label")
  expect_identical(nrow(out$inputs), 2L)
  expect_identical(out$inputs$text, c("first", "second"))
  expect_identical(out$inputs$label, c("nominal", NA))
  expect_identical(out$problems, "1 empty text was dropped")
  expect_identical(nrow(read_text_csv(f, "text")$inputs), 2L)
  expect_true(all(is.na(read_text_csv(f, "text")$inputs$label)))
})

test_that("a Latin-1 CSV reads with the right letters", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeBin(iconv("text,label\nR\u00e9sum\u00e9 de la pi\u00e8ce,nominal\n", "UTF-8", "latin1", toRaw = TRUE)[[1]], f)
  out <- read_text_csv(f, "text", "label")
  expect_identical(out$inputs$text, "R\u00e9sum\u00e9 de la pi\u00e8ce")
  expect_identical(csv_columns(f), c("text", "label"))
  g <- withr::local_tempfile(fileext = ".csv")
  writeBin(iconv("text\nR\u00e9sum\u00e9\n", "UTF-8", "latin1", toRaw = TRUE)[[1]], g)
  expect_identical(read_text_csv(g, "text")$inputs$text, "R\u00e9sum\u00e9")
})

test_that("a UTF-8 CSV with non-ASCII letters reads", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeBin(charToRaw("text\nR\u00e9sum\u00e9\n"), f)
  expect_identical(read_text_csv(f, "text")$inputs$text, "R\u00e9sum\u00e9")
})

test_that("a wrong column and a long CSV text give problems", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("text", strrep("b", MAX_TEXT_CHARS + 1), "ok"), f)
  out <- read_text_csv(f, "text")
  expect_identical(out$inputs$text, "ok")
  expect_match(out$problems, "at most 4,000 characters")
  bad <- read_text_csv(f, "nope")
  expect_identical(nrow(bad$inputs), 0L)
  expect_match(bad$problems, "nope")
})

test_that("read_references keeps at most the maximum", {
  js <- c(make_jpg(withr::local_tempfile(fileext = ".jpg")), make_jpg(withr::local_tempfile(fileext = ".jpg")), make_jpg(withr::local_tempfile(fileext = ".jpg")))
  out <- read_references(as_files(js, c("a.jpg", "b.jpg", "c.jpg")))
  expect_length(out$paths, MAX_REFERENCES)
  expect_match(out$problems, "at most 2 references")
  pdf <- withr::local_tempfile(fileext = ".pdf")
  writeLines("x", pdf)
  out2 <- read_references(as_files(c(js[1], pdf), c("a.jpg", "b.pdf")))
  expect_length(out2$paths, 1)
  expect_match(out2$problems, "1 file is not an image")
  expect_identical(read_references(NULL), list(paths = character(), problems = character()))
})
