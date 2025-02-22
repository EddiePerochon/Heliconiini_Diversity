
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Research Article

This repository contains the code and data used to carry out analyses
for this research article:

**Pérochon et al., 2025 - Müllerian mimicry in Neotropical butterflies:
One mimicry ring to bring them all, and in the jungle bind them. in
prep.**

Müllerian mimicry is a remarkable example of convergent evolution driven
by natural selection where coexisting prey species converge in their
warning signal advertising their defense abilities to predators.
Heliconiine and ithomiine butterflies found throughout Neotropical
rainforests were instrumental in Fritz Müller’s historical model
providing the mechanism for such resemblance. Leveraging decades of
fieldwork observations, we show that phenotypically similar species
present striking broadscale associations in spatial distributions both
within and between tribes. Such co-occurrence appears reinforced by the
convergence of climatic niche among look-alike species . Our findings
emphasize the influential role of mutualistic interactions in shaping
large-scale distribution patterns and driving convergence in species
niches, spanning across phylogenetically distant clades.


All content (including, maps, models, outputs, environmental variables rasters and stacks) is available [here](https://doi.org/10.5281/zenodo.14765685).



## Contents

 - [:file\_folder: **controls**](controls/) directory contains plots used to control the quality of SDM outputs.
 
 - [:file\_folder: **figures**](figures/) directory contains graphs and figures produced for the research article.

 - [:file\_folder: **functions**](functions/) directory contains homemade functions used in the analyses.
 
 - [:file\_folder: **input_data**](input_data/) directory contains the raw and curated data used in the analyses, including occurrence databases, environmental layers, phylogenies, and phenotypic classification. (./Species_data/Env_stacks/ ; ./envData/bioclim_elev/ ;  ./envData/bioclim_var/ ;  ./envData/nasa_forestcover/ are available on [Zenodo](https://doi.org/10.5281/zenodo.14765685))
 
 - [:file\_folder: **Ithomiini**](input_data/) directory contains all data related to ithomiine butterflies used to caryy out analyses between the two tribes. Data were extracted from Doré et al., 2022 [![DOI](https://zenodo.org/badge/DOI/10.5281/10.1111/ddi.13455.svg)](https://doi.org/10.1111/ddi.13455) and Doré et al., 2023  [![DOI](https://zenodo.org/badge/DOI/10.5281/10.1111/ele.14198.svg)](https://doi.org/10.1111/ele.14198)

 - [:file\_folder: **maps**](maps/) directory contains distribution maps and biodiversity maps generated during the study. (available on [Zenodo](https://doi.org/10.5281/zenodo.14765685))

 - [:file\_folder: **models**](models/) directory contains outputs of Species Distribution Models (SDMs). (available on [Zenodo](https://doi.org/10.5281/zenodo.14765685))

 - [:file\_folder: **outputs**](outputs/) directory contains all files generated by the scripts that are not maps, figures, tables, or SDM outputs. (available on [Zenodo](https://doi.org/10.5281/zenodo.14765685))

 - [:file\_folder: **scripts**](scripts/) directory contains the scripts used to run the analyses. Scripts are ordered such as one can generate the study outputs by running scripts in the provided order. 
 
 - [:file\_folder: **tables**](tables/) directory contains the summary tables generated during the study.
  

## How to run it

This research has been developed using the statistical programming
language R. To run the analyses, you will need
installed on your computer the [R software](https://cloud.r-project.org/)
itself and optionally [RStudio Desktop](https://rstudio.com/products/rstudio/download/).

You can download the entire project as a `.zip` from [this URL](https://github.com/EddiePerochon/Heliconiini_Diversity/zipball/master/). After unzipping:
  
  - Open the `Heliconiini_Diversity.Rproj` file, found at the root of the project, in RStudio

  - Run sequentially the scripts found in the [:file\_folder: **scripts**](scripts/) folder. It will rebuild the model outputs, maps, graphs and figures, including the final ones presented in the main text of the article.


## How to cite

Please cite this research article as:
  
> Pérochon, E., Rosser, N., Kozak, K., McMillan, W.O., Huertas, B., Mallet, J., Ready, J., Willmott, K., Elias, M. & Doré, M. (2025). Müllerian mimicry in Neotropical butterflies: One mimicry ring to bring them all, and in the jungle bind them, in prep.


## Associated online archives

The occurrence dataset used to run the analyses is made publicly available at [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10906853.svg)](https://doi.org/10.5281/zenodo.10906853).

The phenotypic classification of heliconiine subspecies defined for this study is made publicly available at [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10903197.svg)](https://doi.org/10.5281/zenodo.10903197).

The distribution maps generated throughout the analyses are made publicly available at [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10903661.svg)](https://doi.org/10.5281/zenodo.10903661)

The folders with all files generated/used throughout the analyses are publicly available at 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14765685.svg)](https://doi.org/10.5281/zenodo.14765685)

The R scripts and input data used throughout the analyses are publicly available at 
[github.com/EddiePerochon/Heliconiini_Diversity](https://github.com/EddiePerochon/Heliconiini_Diversity)
