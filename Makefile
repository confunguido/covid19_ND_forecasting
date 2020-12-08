##=========================================================#
## Author: Guido España
## Create report for CDC forecasting challenge
## https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/syncing-a-fork
## Restore git files:
## https://stackoverflow.com/questions/953481/find-and-restore-a-deleted-file-in-a-git-repository?rq=1
##=========================================================#
all: report forecasts
.PHONY: clean cleanJunk 

CURRENT_DIR := $(shell pwd)
PANDOC_NUM_DIR := ~/pandoc_tex_nos

forecast_max_date = 2021-01-04
forecast_date = 2020-12-07

SRC_DIR = ./scripts
DATA_DIR = ../experiments/Midwest_simulations/output/SHORT_FORECAST
FIGS_DIR = ./figures
PDF_DIR = ./report
REPORT_DIR = ./report
OUT_DIR = ./output

TEX_MASTER := report_cdc_forecasting_covid19_NotreDame-FRED
PDF_FILES := $(PDF_DIR)/$(TEX_MASTER).pdf
BIB_FILE := $(TEX_MASTER).bib
DOC_REF := template_report_word.docx

EQ_FILTER := $(PANDOC_NUM_DIR)/pandoc_eqnos_tex.py
TBL_FILTER := $(PANDOC_NUM_DIR)/pandoc_tablenos_tex.py
FIG_FILTER := $(PANDOC_NUM_DIR)/pandoc_fignos_tex.py
CLEAN_FILTER := $(PANDOC_NUM_DIR)/pandoc_clean_tex.py

##=========================================================#
## Data  ---------------------
##=========================================================#
FRED_FILES := $(wildcard $(DATA_DIR)/FRED*short_forecast_asymp_out/*.csv) 
DATA_FILES := $(wildcard ../data/US_states_covid*.csv)
SRC_FILES := $(SRC_DIR)/download_covid_data.R
DATA_OUT := $(SRC_FILES:.R=.Rout)
date_file = $(SRC_DIR)/last_updated.txt
update_flag := $(shell $(SRC_DIR)/update_date.sh $(date_file))

data: $(DATA_OUT)

$(SRC_DIR)/download_covid_data.Rout: $(SRC_DIR)/download_covid_data.R $(date_file)
	(cd $(SRC_DIR); R CMD BATCH --no-save $(<F))

##========================================================#
## Figures ---------------------
##========================================================#
FIG_FILES := $(FIGS_DIR)/report_figure_shelter_patterns.jpeg \
	$(FIGS_DIR)/report_figure_deaths_forecast.jpeg\
	$(FIGS_DIR)/report_figure_deaths_cumulative.jpeg\
	$(FIGS_DIR)/report_figure_calibrated_parameters.jpeg\
	$(FIGS_DIR)/report_figure_reproduction_number.jpeg\
	$(FIGS_DIR)/report_figure_parameter_periods.jpeg\
	$(FIGS_DIR)/report_figure_age_CFR.jpeg\
	$(FIGS_DIR)/report_figure_age_symptoms.jpeg

figs: $(FIG_FILES) 

$(FIGS_DIR)/report_figure_%.jpeg: $(SRC_DIR)/report_figure_%.R  $(FRED_FILES) $(DATA_OUT)
	(cd $(SRC_DIR); Rscript $(<F) ${forecast_max_date})

##========================================================#
## Report ---------------------
##========================================================#
report: $(PDF_FILES) $(PDF_DIR)/$(TEX_MASTER).docx

PANDOC_FLAGS :=  --csl ~/Dropbox/Literature/style_files/plos-computational-biology.csl	--reference-doc $(DOC_REF) --mathml --bibliography=$(BIB_FILE) --filter $(CLEAN_FILTER) --filter $(EQ_FILTER) --filter $(TBL_FILTER) --filter $(FIG_FILTER) -M reference-section-title=References

junk_files_report = *.log *.out *.blg *.aux *.toc auto *.bbl *.bcf *.run.xml
$(PDF_DIR)/$(TEX_MASTER).pdf: $(PDF_DIR)/$(TEX_MASTER).tex $(PDF_DIR)/$(BIB_FILE) $(FIG_FILES)
	(cd $(PDF_DIR); pdflatex $(TEX_MASTER);\
	bibtex $(TEX_MASTER);\
	rm -rf `biber --cache`;\
	biber $(TEX_MASTER);\
	pdflatex $(TEX_MASTER);\
	pdflatex $(TEX_MASTER);\
	rm -rfv $(junk_files_report); rm -rfv *.bbl)

$(PDF_DIR)/$(TEX_MASTER).docx: $(PDF_DIR)/$(TEX_MASTER).tex $(PDF_DIR)/$(TEX_MASTER).pdf $(PDF_DIR)/$(DOC_REF)
	(cd $(PDF_DIR); pandoc $(<F) -o $(@F) $(PANDOC_FLAGS))

##========================================================#
## Forecasts----------------
##========================================================#
forecasts: $(OUT_DIR)/$(forecast_date)-NotreDame-FRED.csv

$(OUT_DIR)/$(forecast_date)-NotreDame-FRED.csv: $(SRC_DIR)/format_outputs_for_cdc.R $(FRED_FILES)
	(cd $(SRC_DIR); Rscript $(<F) $(forecast_date))

##========================================================#
## Clean stuff-----------------------
##========================================================#
clean:
	rm -fv $(DATA_OUT); rm -fv $(FIG_FILES); rm -fv $(PDF_FILES)

cleanJunk:
	(cd $(PDF_DIR); rm -rfv $(junk_files_report);)
