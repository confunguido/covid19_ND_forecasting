##===========================================================================#
## Author: Guido España
## Gather all the data and generate figures & tables automatically
all: presentation
.PHONY: cleanJunk 

## Overall variables

CURRENT_DIR := $(shell pwd)
PRES_DIR = .

##==============================================================================#
## 1. Make the figures
##==============================================================================#
FIGS_DIR = ./figures
REPORT_DIR = ../figures/
PRES_FIGS := $(wildcard $(FIGS_DIR)/*.jpeg)\
	$(wildcard $(REPORT_DIR)/*.jpeg)

IMG_DIR := $(PRES_DIR)/images
IMG_SRC := $(wildcard $(PRES_DIR)/images/FRED*.svg)
IMG_INK := $(IMG_SRC:.svg=.png)

figs: $(IMG_INK)

$(IMG_DIR)/%.png: $(IMG_DIR)/%.svg
	(cd $(IMG_DIR); inkscape --export-filename=$(CURRENT_DIR)/$@  $(CURRENT_DIR)/$<)

##==============================================================================#
## 2. Presentation
##==============================================================================#
PRES_TEX = presentation_cdc_forecasting_meeting_20200804
BIB_FILE := presentation_cdc_forecasting.bib

junk_files_pres = *.log *.out *.aux \
	*.snm *.nav *.toc _region_* auto *.bbl *.dvi *.bcf *.blg *.xml

presentation: $(PRES_DIR)/$(PRES_TEX).pdf

$(PRES_DIR)/$(PRES_TEX).pdf: $(PRES_DIR)/$(PRES_TEX).tex $(PRES_DIR)/$(BIB_FILE) $(IMG_INK) $(PRES_FIGS)
	(cd $(PRES_DIR); pdflatex $(PRES_TEX);\
	bibtex $(PRES_TEX);\
	rm -rf `biber --cache`;\
	biber $(PRES_TEX);\
	pdflatex $(PRES_TEX);\
	pdflatex $(PRES_TEX);\
	rm -rfv $(junk_files_pres))


##==============================================================================#
## Clean stuff
##==============================================================================#
cleanJunk:
	(cd $(PRES_DIR); rm -rfv $(junk_files_pres))
