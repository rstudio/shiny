UGLIFY = ./node_modules/.bin/uglifyjs 

# Directory of bootstrap datepicker
DATEPICKER = ../inst/www/shared/datepicker/js/

all: $(DATEPICKER)/bootstrap-datepicker.min.js

clean:
	rm -f $(DATEPICKER)/bootstrap-datepicker.min.js


$(DATEPICKER)/bootstrap-datepicker.min.js: $(DATEPICKER)/bootstrap-datepicker.js \
	                    $(wildcard $(DATEPICKER)/locales/bootstrap-datepicker.*.js)
	$(UGLIFY) \
	  $(DATEPICKER)/bootstrap-datepicker.js \
	  $(DATEPICKER)/locales/bootstrap-datepicker.*.js \
	  --output $(DATEPICKER)/bootstrap-datepicker.min.js
