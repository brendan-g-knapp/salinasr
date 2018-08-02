# install.packages("roxygen2")
pkgdown::clean_site()
# devtools::as.package(".", create = TRUE)
devtools::document(".")
devtools::install(".")
rmarkdown::render("README.Rmd", output_format = "github_document")
# pkgdown::clean_site()
pkgdown::build_site(seed = 1234)

