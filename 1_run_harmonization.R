## code to run the diatom harmonization function
## Xavier Benito (xavier.benito@irta.cat)/ website: https://xbenitogranell.github.io 
## November 2026

# set up the folder structure
scriptpath <- dirname(rstudioapi::getSourceEditorContext()$path) # path of current script file
resultpath <- file.path(scriptpath,paste0("results"))

# Create a result folder to store the harmonization look up table output
if (!dir.exists(resultpath)){
  dir.create(resultpath)
} else {
  print("resultpath already exists!")
}

# Set the paths of folders where the harmonization functions are, and load them
functionspath <- file.path(scriptpath,paste0("functions"))
source(paste0(functionspath,"/trunc_author_v1.R"))
source(paste0(functionspath,"/diatom_harmonization_v1.R"))

# Set the paths to the folder where the internal taxonomic lists are
taxonomic_lists_path <- file.path(scriptpath, "taxonomic_lists")

# read external diatom lists
setwd(taxonomic_lists_path)
# Read in Biodata complete taxonomy
BioData <- read.csv("BioData_taxonlist.csv", sep = ";")
# read in Neotoma diatom master list (Simon Goring May 2024)
Neotoma <- read.csv("neotoma_taxonlist_22102026.csv", sep = ";", row.names = 1)
# read in EDDI diatom master list
eddi_diatoms <- read.csv("eddi_taxonlist.csv", sep = ";")


# Apply the diatom_dictionary harmonization function
my_diatom_dictionary <- diatom_dictionary_v1(diatnames="user_list", 
                                             biodata="BioData",
                                             eddi_taxon_table="eddi_diatoms",
                                             neotoma_master_list="Neotoma",
                                             output_file = paste0(resultpath,"/my_diatom_dictionary.xlsx"))

# save the conversion table
#openxlsx::write.xlsx(my_diatom_dictionary, "my_diatom_dictionary.xlsx")

