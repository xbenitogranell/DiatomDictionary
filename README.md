# DiatomDictionary
Enabling diatom nomenclatural harmonization through a reproducible workflow

The script aims at guiding decisions of nomenclatural harmonization of complex diatom longlists across datasets. 
The rationale of the **Diatom Dictionary** is to standardize and clean diatom names, and to gather the taxonomic metadata of the taxa before new taxon uploads to [Neotoma Paleoecology Database](https://www.neotomadb.org).
Although the workflow is closely related to diatom data for Neotoma, it can be adjusted to other types of aquatic proxies stored in different Earth and environmental databases such as [Pangaea](https://www.pangaea.de/)

---

## Features
The workflow identifies and documents:
- spelling errors and misspellings;
- nomenclatural inconsistencies;
- abbreviations and formatting variants;
- taxonomic qualifiers such as cf., aff., sp. and spp.;
- potential taxonomic ambiguities;
- possible synonyms and updated nomenclature;
- discrepancies between contributor-provided names and the Neotoma diatom master list; and
- taxonomic information available from multiple external resources.

## Internal and external taxonomic sources
This rationale of the harmonization workflow reflects the absence of a single centralized, current and universally accepted diatom taxonomic list.\
Different taxonomic resources may provide different names, authorities or synonymy relationships. Consequently, automated selection of one supposedly correct name could introduce changes that are not justified by the original data.\
The **Diatom Dictionary** therefore prioritizes the informed choice of the data steward over fully automated taxonomic curation.

## Sources
- `diaThor R` package internal species list [Nicolosi Gelis et al., 2022](10.1016/j.ecolmodel.2021.109859). European diatom taxon longlist from monitoring data
- `USGS BioData` [MacCoy 2011](https://doi.org/10.3133/fs20113112)\
- `European Diatom Database taxon list` (Battarbee et al., 2001, ECRC Research Report No 81, University College London, London, p. 94)
- `Neotoma diatom master list` Diatom taxonomy table downloaded from Neotoma on 22/10/2026

The four diatom longlists combined have **12209** unique taxon names. The list can be expanded with future additions.

---
  
## Running the script
1. If you have a [GitHub account](https://github.com/) and can use [git](https://git-scm.com/), you can [clone the repo](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
2. Download the full repository in your local machine: Code > Download ZIP. Unzip the file and open `DiatomDictionary.Rproj`
3. The workflow uses the `renv` package to ensure reproducible package versions. `renv` ensures that the Diatom Dictionary Harmonization Workflow can always be run with the same package versions, specially those that call external taxonomic API that may change over time, including `taxize`, `diathor`, `worrms`, `pangear`, and `zen4R`.
4. The exact package versions used to develop and test this workflow are recorded in the `renv.lock` file.
4. Restore the package environment first with:

```r
install.packages("renv")
renv::restore()
```
5. Open `0_prepare_data.R` and `1_run_harmonization` in run them a sequential order. Both scripts are self-explanatory containing comments to guide through the different steps.

## Input data options
The workflow uses two diatom datasets as examples to extract the longlist of names. However 

---

## Contributing
For questions, feedback, or contributions to the Diatom Dictionary, please contact:\
- [Xavier Benito](mailto:xavier.benito@irta.cat), https://xbenitogranell.github.io *Marine and Continental Waters Programme, Institute for Agrifood Research and Technology (IRTA), Spain*\
- [Anna Tichá](mailto:anna.ticha@natur.cuni.cz), *Charles University, Prague, Czechia*

---
  
## How to cite the Diatom Dictionary?
The project has received support from the [EU COST Action PalaeOpen](https://www.cost.eu/actions/CA23116/)

---

## Other online resources
1.  [Diatom Dictionary Zenodo repository](https://doi.org/10.5281/zenodo.23737817)
2.  [Shiny app European Diatom Inventory for Palaeoecology and Ecology (EDIPE)](https://edipe.shinyapps.io/shiny/)
3.  [PalaeOpen project website](https://palaeopen.github.io/)

