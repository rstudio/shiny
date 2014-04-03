purl = Rscript --no-restore --no-save ../tools/purl.R "$(1)" "$(2)"

rfiles := $(patsubst %.Rmd,%.R,$(wildcard *.Rmd))
rmdfiles := $(wildcard *.Rmd)

all: $(rfiles)

%.R: %.Rmd
	$(call purl,$^,$@)

clean:
	rm $(rfiles)
