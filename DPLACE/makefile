# setting RECIPEPREFIX to plus sign:
.RECIPEPREFIX = +

# Variables:

# Folders:
DATA = data
SRC = ../src

# Folders to create:
PHONY = tmp
INTER = intermediate
EXPLORE = results/explore
CLUSTER = results/cluster
PURITY = results/purity
OPTIMAL = results/optimal
MCA = results/mca
CMCA = results/cluster_mca
CPURITY = results/purity_mca
COPTIMAL = results/optimal_mca

DIRS = $(PHONY) $(INTER) $(RESULT) $(EXPLORE) \
    $(CLUSTER) $(PURITY) $(OPTIMAL) \
    $(MCA) $(CMCA) $(CPURITY) $(COPTIMAL)

# targets:
all: explore cluster mca evaluate evaluate_mca compare compare_mca

.PHONY: explore cluster mca clean evaluate compare compare_mca evaluate_mca

$(DIRS):
+ mkdir -p $@


# Explore Ethnographic Atlas variables 
# -- explore:
#   -- gini-simpson diversity
#   -- frequency of the most represented/least represented category
#   -- frequency of unknown values (NA)
# -- Filter variables with bad performance for MCA
explore: $(INTER)/fEA.rds $(EXPLORE)/summary.txt $(PHONY)/figures

$(INTER)/fEA.rds: $(DATA)/EA.rds | $(INTER)
+ Rscript $(SRC)/explore.r $< --output $@

$(EXPLORE)/summary.txt: $(DATA)/EA.rds | $(EXPLORE)
+ Rscript $(SRC)/explore.r $< --summary $@

$(PHONY)/figures: $(DATA)/EA.rds | $(EXPLORE) $(PHONY)
+ Rscript $(SRC)/explore.r $< --figures $(EXPLORE)
+ touch $@


cluster: $(INTER)/clustered.rds $(INTER)/purity.rds

$(INTER)/clustered.rds: $(INTER)/fEA.rds $(DATA)/residences.txt | $(CLUSTER)
+ Rscript $(SRC)/cluster.r $(INTER)/fEA.rds --methods all --residence $(DATA)/residences.txt \
    --output $@ --figures $(CLUSTER)


$(INTER)/purity.rds: $(INTER)/clustered.rds $(DATA)/residences.txt| $(PURITY)
+ Rscript $(SRC)/calculate_purity.r $< $@ --folder $(PURITY) --residences $(DATA)/residences.txt


mca: $(INTER)/mca.rds $(INTER)/clustered_mca.rds

$(INTER)/mca.rds: $(INTER)/fEA.rds | $(MCA)
+ Rscript $(SRC)/mca.r $< --output $@ --extract 98 --explore 5 --figures $(MCA)

$(INTER)/clustered_mca.rds: $(INTER)/mca.rds $(INTER)/fEA.rds $(DATA)/residences.txt | $(CMCA)
+ Rscript $(SRC)/cluster.r $(INTER)/fEA.rds --methods all --residence $(DATA)/residences.txt \
    --output $@ --figures $(CMCA) --mca $(INTER)/mca.rds

$(INTER)/purity_mca.rds: $(INTER)/clustered_mca.rds $(DATA)/residences.txt| $(CPURITY)
+ Rscript $(SRC)/calculate_purity.r $< $@ --folder $(CPURITY) --residences $(DATA)/residences.txt


compare: $(PURITY)/max_purity_table.tex $(INTER)/optimal_clusters.json

$(PURITY)/max_purity_table.tex: $(INTER)/purity.rds | $(PURITY)
+ Rscript $(SRC)/max_purity.r $< $@

$(INTER)/optimal_clusters.json: $(INTER)/purity.rds $(INTER)/clustered.rds \
    $(DATA)/residences.txt| $(OPTIMAL)
+ Rscript $(SRC)/optimal_clusters.r $(INTER)/purity.rds $(INTER)/clustered.rds $@ \
    --tables $(OPTIMAL)/tables --figures $(OPTIMAL)/figures --residences $(DATA)/residences.txt


compare_mca: $(CPURITY)/max_purity_table_mca.tex $(INTER)/optimal_clusters_mca.json

$(CPURITY)/max_purity_table_mca.tex: $(INTER)/purity_mca.rds | $(CPURITY)
+ Rscript $(SRC)/max_purity.r $< $@

$(INTER)/optimal_clusters_mca.json: $(INTER)/purity_mca.rds $(INTER)/clustered_mca.rds \
    $(DATA)/residences.txt | $(COPTIMAL)
+ Rscript $(SRC)/optimal_clusters.r $(INTER)/purity_mca.rds $(INTER)/clustered_mca.rds $@ \
    --tables $(COPTIMAL)/tables --figures $(COPTIMAL)/figures --residences $(DATA)/residences.txt

clean:
+ rm -rf $(DIRS)
