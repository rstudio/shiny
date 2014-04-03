purl = Rscript --no-restore --no-save tools/purl.R "$(1)" "$(2)"

rfiles := $(patsubst Rmd/%.Rmd,R/%.R,$(wildcard Rmd/*.Rmd))
rhtmlfiles := $(patsubst Rmd/%.Rmd,Rmd/%.html,$(wildcard Rmd/*.Rmd))
rmdfiles := $(wildcard Rmd/*.Rmd)

all: $(rfiles)

R/%.R: Rmd/%.Rmd
	$(call purl,$^,$@)

clean:
	rm -f $(rfiles) $(rhtmlfiles)

.PHONY: all clean
