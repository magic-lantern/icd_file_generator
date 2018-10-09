all: acquire-and-build-ICD-data.html

acquire-and-build-ICD-data.html: acquire-and-build-ICD-data.Rmd
	R --vanilla -e "rmarkdown::render('$<')"

