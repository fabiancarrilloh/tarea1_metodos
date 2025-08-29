options(stringsAsFactors = FALSE)
set.seed(123)

root <- normalizePath(".", winslash = "/", mustWork = TRUE)
scripts_dir <- file.path(root, "scripts")
outputs_dir <- file.path(root, "outputs")

stopifnot(dir.exists(scripts_dir))
if (!dir.exists(outputs_dir)) {
  dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)
}

files <- list.files(scripts_dir, pattern = "\\.R$", full.names = TRUE)
base  <- basename(files)
nums  <- as.integer(sub("^q([0-9]+).*", "\\1", base))
suf   <- sub("^q[0-9]+([a-z]?).*", "\\1", base)
ord   <- order(nums, suf, base)
files <- files[ord]

message("Ejecutando en este orden:")
print(basename(files))

run_one <- function(f) {
  message(">>> ", basename(f))
  sys.source(f, envir = new.env(parent = globalenv()))
  invisible(TRUE)
}

invisible(lapply(files, run_one))

si <- utils::capture.output(sessionInfo())
writeLines(si, file.path(outputs_dir, "sessionInfo.txt"))

message("Listo.")
