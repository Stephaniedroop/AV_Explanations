# Autonomous Vehicle Explanations

This is a repo for working analysis. For background on the dataset and annotations please contact Balint Gyevnar.

## Authors

- Balint Gyevnar
- Stephanie Droop (stephanie.droop@ed.ac.uk)
- Tadeg Quillien

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

## Files

- `preprocessing.R` - creates file `preProcessedData.csv` from video stimuli and participant data (not stored here)
- `analysis.R` - main analysis file with regressions and testing contribution of each predictor
- `HighlightAV_check.R` - supplementary file to run same analyses as `analysis.R` split by the AV/car manipulation.
