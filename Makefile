.PHONY: all test restore

# Restore packages, then rebuild data, model, tables, and charts.
all: restore
	Rscript run_all.R

restore:
	Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::restore()"

test:
	Rscript -e "testthat::test_dir('tests/testthat')"
