## code to prepare input long list of diatoms names
## Xavier Benito (xavier.benito@irta.cat)/ website: https://xbenitogranell.github.io 
## November 2026

#Activate the project-specific package library safely
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
} else {
  stop(
    "renv project not detected.\n",
    "Please initialize renv first using: renv::init()"
  )
}

#Verify that package versions are locked
if (!file.exists("renv.lock")) {
  stop(
    "renv.lock not found.\n",
    "Package versions cannot be reproduced without a lockfile."
  )
}

#Install the required packages
required_packages <- c(
  "tidyverse",
  "stringr",
  "qdapTools",
  "worrms",
  "openxlsx",
  "stringdist",
  "diathor",
  "taxize",
  "zen4R",
  "pangaear",
  "mapview",
  "sf",
  "sp",
  "rstudioapi",
  "readxl"
)

# Check for missing packages and optionally auto-restore
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  message("The following required packages are missing from the active library:\n - ", 
          paste(missing_packages, collapse = "\n - "))
  
  # Option to auto-restore if running interactively, or stop with instructions
  answer <- readline(prompt = "Would you like to run renv::restore() now? (y/n): ")
  if (tolower(answer) == "y") {
    renv::restore(prompt = FALSE)
  } else {
    stop("Execution halted. Run `renv::restore()` manually to sync the environment.")
  }
}

# Load all packages invisibly
invisible(lapply(required_packages, library, character.only = TRUE))

# set up the folder structure
scriptpath <- dirname(rstudioapi::getSourceEditorContext()$path) # path of current script file
resultpath <- file.path(scriptpath,paste0("results"))

# Create a result folder to store the harmonization look up table output
if (!dir.exists(resultpath)){
  dir.create(resultpath)
} else {
  print("resultpath already exists!")
}

# Dataset #1
# Milan, M. et al 2022.  Limnol. Oceanogr. 67, 2088–2100 (https://aslopubs.onlinelibrary.wiley.com/doi/10.1002/lno.12190) 
# Set the settings of the diatom dataset

Id <- "1" #id diatom dataset 
Author <- "Milan" #first author of the published dataset
Site <- "Constance" #name of the study site

# search diatom dataset in Pangea using the author's publication name and filter by the known DOI of the dataset
search_results <- pg_search(query = "citation:author:Milan AND diatoms", count = 100) %>%
  dplyr::filter(grepl("10.1594/PANGAEA.958773",doi))

# Download the dataset and store the metadata in a list
pangaea_list <- list()
for (i in seq_along(search_results$doi)) {
  pangaea_list[[i]] <- pg_data(search_results$doi[i])
}

# Flatten the list to make it easier to iterate through all potential 'item' objects
all_items <- unlist(pangaea_list, recursive = FALSE)

# extract metadata
my_metadata <- list()
for (i in seq_along(all_items)) {
  my_metadata[[i]] <- as.data.frame(all_items[[i]]$metadata$coverage)
}

# all items (datasets) within the lake record have the same metadata, so get the first one
my_metadata <- my_metadata[[1]]

#Extract all numbers (including decimals) using regex
nums <- as.numeric(str_extract_all(my_metadata, "\\d+\\.\\d+")[[1]])

# Create the data frame from the extracted values
metadata <- data.frame(
  LATITUDE = nums[1],
  LONGITUDE = nums[2],
  min_depth = nums[3],
  max_depth = nums[4],
  substrate = "sediment/rock")

# Make a shapefile of the site and visualize it on a map
my_shp <- st_as_sf(metadata,
                   coords = c("LONGITUDE", "LATITUDE"),
                   crs = 4326) 
mapview(my_shp, map.types = c("OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"))

# get diatom counts
my_data <- list()
for (i in seq_along(all_items)) {
  my_data$data[[i]] <- as.data.frame(all_items[[i]][['data']][-c(1:3)])
  my_data$meta[[i]] <- as.data.frame(all_items[[i]][['data']][c(1:3)])
  colnames(my_data$meta[[i]]) <- c("ID","depth_sed_m", "age_ad_ce")
  my_data$core[[i]] <- cbind(my_data$meta[[i]],my_data$data[[i]]) 
  my_data$data[[i]] <- NULL
  my_data$meta[[i]] <- all_items[[i]][['metadata']][["parameters"]] #where full diatom taxa names are
}

# extract the list of unique diatom names
spp_list_1 <-  my_data$core[[1]] %>%
  select(c(Species)) %>%
  rename(taxa=Species) %>%
  distinct(taxa) %>%
  mutate(dataset=paste0(Id,"_", Author,"_",Site),
         diatom_id=row_number()) %>% #add name of the dataset (source locality or collection unit)
  as.data.frame() 


# Dataset #2
# Benito, X., Trobajo, R., and Ibáñez, C. (2015). J. Paleolimnol. 54, 171–188. (https://link.springer.com/article/10.1007/s10933-015-9845-3)
Id <- "2" #id diatom dataset 
Author <- "Benito" #first author of the published dataset
Site <- "EbroDelta" #name of the study site

zenodo <- ZenodoManager$new() #initialize the interface

# download Ebro Delta diatom dataset from Zenodo
EbroDelta <- zenodo$getRecordByDOI("https://doi.org/10.5281/zenodo.15113793")
EbroDelta$downloadFiles(path = resultpath)

# create function to read Excel files
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  sapply(sheets, function(f) (as.data.frame(readxl::read_excel(filename, sheet = f, col_names = TRUE))), 
         simplify = FALSE)
}

## Read in data
files <- list.files(path=resultpath, full.names = TRUE, pattern = ".xlsx")
print(files)

# execute function for all excel files in "files"
data_all <- lapply(files, read_excel_allsheets)[[1]]

## Extract files from the list. First where site locations are
sites <- data.frame(data_all$sites) 

# Make a shapefile of the site and visualize it on a map
my_shp <- st_as_sf(as.data.frame(sites),
                   coords = c("UTMX", "UTMY"),
                   crs = 32631) 
mapview(my_shp, map.types = c("OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"))

# Read in the diatom data
diat <- data_all$diatoms
diat <- data.frame(taxa=colnames(diat))

# extract the list of unique diatom names
spp_list_2 <- diat %>% 
  dplyr::filter(!taxa %in% c("code", "...426")) %>%
  distinct(taxa) %>%
  mutate(dataset=paste0(Id,"_", Author,"_",Site),
         diatom_id=row_number()) %>% #add name of the dataset (source locality or collection unit)
  as.data.frame() 

# combine the two diatom taxon longlists
user_list <- bind_rows(spp_list_1,spp_list_2)


