# Run down of code used to install and test package

urlchecker::url_check()

devtools::build()

devtools::install(build_vignettes = TRUE)

devtools::document()

devtools::check(remote = TRUE, manual = TRUE)

#revdepcheck::revdep_reset()
#revdepcheck::revdep_check(num_workers = 4)

devtools::check_win_devel()

# If that fails, you can try:
# devtools::check(cran = TRUE)



