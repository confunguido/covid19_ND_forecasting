##=========================================================#
## Author: Guido España
## Create report for CDC forecasting challenge
## https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/syncing-a-fork
##=========================================================#
all: report forecasts
.PHONY: clean cleanJunk 

CURRENT_DIR := $(shell pwd)

forecast_max_date = 2020-07-21
forecast_date = 2020-06-01

SRC_DIR = ./scripts
DATA_DIR = ../experiments/Midwest_simulations/output/SHORT_FORECAST
FIGS_DIR = ./figures
PDF_DIR = ./report
REPORT_DIR = ./report
OUT_DIR = ./output

TEX_MASTER := report_cdc_forecasting_covid19_NotreDame-FRED
PDF_FILES := $(PDF_DIR)/$(TEX_MASTER).pdf
BIB_FILE := $(TEX_MASTER).bib

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
report: $(PDF_FILES) 

junk_files_report = *.log *.out *.blg *.aux *.toc auto *.bbl *.bcf *.run.xml
$(PDF_DIR)/$(TEX_MASTER).pdf: $(PDF_DIR)/$(TEX_MASTER).tex $(PDF_DIR)/$(BIB_FILE) $(FIG_FILES)
	(cd $(PDF_DIR); pdflatex $(TEX_MASTER);\
	bibtex $(TEX_MASTER);\
	rm -rf `biber --cache`;\
	biber $(TEX_MASTER);\
	pdflatex $(TEX_MASTER);\
	pdflatex $(TEX_MASTER);\
	rm -rfv $(junk_files_report); rm -rfv *.bbl)

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
