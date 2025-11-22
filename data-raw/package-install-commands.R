# Run down of code used to install and test package

devtools::build()

devtools::install()

devtools::document()

devtools::check(remote = TRUE, manual = TRUE)

#revdepcheck::revdep_reset()
#revdepcheck::revdep_check(num_workers = 4)

devtools::check_win_devel()

# urlchecker::url_check()

