devtools::document()  # To generate/update the documentation again if needed
devtools::build()     # To build the package
devtools::install()   # To install the package

devtools::build_vignettes()
devtools::document()


library(identifyASE)
?slice_bcx_data   # Replace with the function name you want to check
?search_by_seqnum
?add_window_day
?add_aod_daily
?qualifying_abx_duration
?define_sepsis
?apply_all_transformations
?define_ase_disch
?define_ase
