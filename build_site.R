library(rmarkdown)

build = function(src) rmarkdown::render(input=sprintf('%s.Rmd', src),output_dir='www')

build('index')
build('01-rstudio')
build('02-R_intro')
build('links')