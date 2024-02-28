# Autonomous Vehicle Explanations

This is a repo for working analysis. Background on the raw data can be found at
https://datashare.ed.ac.uk/handle/10283/8714

## Authors

- Balint Gyevnar (balint.gyevnar@ed.ac.uk)
- Stephanie Droop (stephanie.droop@ed.ac.uk)
- Tadeg Quillien (tadeg.quillien@ed.ac.uk)

## Packages needed for analysis in R

- tidyverse
- jsonlite
- lme4
- ggbeeswarm
- ggcorrplot
- colorspace
- effectsize
- ggpubr

## License

2024 openly licensed via CC BY 4.0 https://creativecommons.org/licenses/by/4.0/

## R Files

- `analysis.R` - main analysis file with regressions and testing contribution of
each predictor
- `HighlightAV_check.R` - supplementary file to run same analyses as
`analysis.R` split by the AV/car manipulation.
- `preprocessing.R` - if one wants to re-create the file `preProcessedData.csv` 
from the raw data.

