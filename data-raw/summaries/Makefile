HTML_FILES := $(patsubst %.R, %.html ,$(wildcard *.R))

all: clean html

html: $(HTML_FILES)

%.html: %.R
	R --slave -e "set.seed(0);rmarkdown::render('$<')"

.PHONY: clean

clean:
	$(RM) $(HTML_FILES)
