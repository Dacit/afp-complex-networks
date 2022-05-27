RTEX_FILES := $(wildcard figures/*.Rtex) data/data.Rtex main.Rtex
TEX_FILES := $(patsubst %.Rtex,%.tex,$(RTEX_FILES))

all : $(TEX_FILES)

clean :
	rm -f $(TEX_FILES) && rm -r figure

%.tex : %.Rtex
	R -e 'library(knitr);knit("$<")' && mv $(@F) $@
