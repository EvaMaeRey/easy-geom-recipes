# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script styles R code before rendering with quarto
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. The files being rendered (we don't necessarily want to re-style the whole)
# project if we're only rendering one file
files <- strsplit(Sys.getenv("QUARTO_PROJECT_INPUT_FILES"), "\n")[[1]]

# 2. Make sure we only try to style supported filetypes
files <- files[grep("\\.(R|Rmd|Rmarkdown|qmd|Rnw)$", files, ignore.case = TRUE)]

# 3. Apply the styling
styler::style_file(files)
