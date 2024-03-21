files <- list.files('data-raw', pattern='.csv')
library(usethis)
for (fl in files) {
  dat <- read.csv(file.path('data-raw', fl))
  name <- tools::file_path_sans_ext(fl)
  assign(name, dat)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
}
