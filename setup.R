# Loads objects from CDC-code-generated .RData files, assuming these are
# collected in the same directory (specified by RDA_PATH)

RDA_PATH = "PATH"

rda_files <- c(list.files(RDA_PATH))
for (f in rda_files) {
  load(file = paste(RDA_PATH, f, sep = '/'))
}