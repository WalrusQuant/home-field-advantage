.PHONY: all test restore

# Restore packages, then rebuild data, model, tables, and charts.
all: restore
	Rscript run_all.R

restore:
	Rscript --vanilla renv/restore.R

test:
	Rscript -e "testthat::test_dir('tests/testthat')"
