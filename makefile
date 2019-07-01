# setting RECIPEPREFIX to plus sign:
.RECIPEPREFIX = +

# Folders
SRC = src
DATA = data

# Folders that need to be created
PHONY = tmp
INTER = intermediate
EA = EA
DPLACE = DPLACE

DIRS = $(PHONY) $(INTER) $(EA) $(EA)/data $(DPLACE) $(DPLACE)/data

all: prepare_packages prepare_ea prepare_dplace make_ea make_dplace

# PHONY designation
.PHONY: prepare_packages prepare_dplace prepare_ea make_dplace make_ea clean

$(DIRS):
+ mkdir -p $@

prepare_packages:
+ Rscript $(SRC)/check_packages.r -i \
    "argparser" "magrittr" "import" "homals" "Rcpp" "ape" "entropy" "jsonlite" "phytools"



# Prepare data for R's version of Ethnographic Atlas
# -- set up directories
# -- format and filter EA.Rdata into EA.rds
# -- prepares residence data
# -- prepares makefile
prepare_ea: $(INTER)/EA.rds $(EA)/data/EA.rds $(EA)/data/residences.txt $(EA)/makefile

$(INTER)/EA.rds: | $(INTER)
+ Rscript $(SRC)/prepare_ea.r $(DATA)/EA.Rdata $@

$(EA)/data/residences.txt: data/EA.Rdata | $(EA)/data
+ Rscript $(SRC)/prepare_ea.r --residence $< $@

$(EA)/data/EA.rds: $(INTER)/EA.rds | $(EA)/data
+ cp $< $@

$(EA)/makefile: | $(EA)
+ cp $(SRC)/makefile $@


# Prepare data for d-place version of Ethnographic Atlas
# -- set up directories
# -- clone dplace data repository
# -- install pydplace package to pyton's virtual environment
# -- prepares residence data
# -- prepares makefile
prepare_dplace: venv/bin/activate $(PHONY)/dplace_git $(INTER)/dplace.rds
prepare_dplace: $(DPLACE)/data/residences.txt $(DPLACE)/makefile $(DPLACE)/data/EA.rds

venv/bin/activate: data/requirements.txt
+ test -f /venv/bin/activate || python3 -m venv venv; \
    . venv/bin/activate; \
    pip install -Ur data/requirements.txt; \
    touch venv/bin/activate

$(PHONY)/dplace_git: | $(INTER) $(PHONY)
+ git clone https://github.com/D-PLACE/dplace-data $(INTER)/dplace
+ touch $@

# pydplace needs to depend on previous version of pyglottolog
$(INTER)/dplace.csv: $(PHONY)/dplace_git venv/bin/activate
+ . venv/bin/activate; \
    dplace --repos $(INTER)/dplace extract --dataset EA $@

$(INTER)/dplace.rds: $(INTER)/dplace.csv
+ Rscript $(SRC)/prepare_dplace.r $< $@

$(DPLACE)/data/residences.txt: $(INTER)/dplace.csv | $(DPLACE)/data 
+ Rscript $(SRC)/prepare_dplace.r --residence $< $@

$(DPLACE)/makefile: | $(DPLACE)
+ cp $(SRC)/makefile $@

$(DPLACE)/data/EA.rds: $(INTER)/dplace.rds | $(DPLACE)/data
+ cp $< $@

# run make in EA folder
# this performs analysis on R's version of Ethnographic Atlas
make_ea: prepare_ea | EA
+ cd $(EA) && $(MAKE)


# run make in DPLACE folder
# this performs analysis on d-place's version of Ethnographic Atlas
make_dplace: prepare_dplace | $(DPLACE)
+ cd DPLACE && $(MAKE)


clean:
+ rm -rf $(DIRS)
+ rm -rf venv



remake:
+ rm $(EA)/makefile
+ rm $(DPLACE)/makefile
+ $(MAKE)
