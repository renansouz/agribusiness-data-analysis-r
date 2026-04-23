# Agribusiness Data Exploratory Analysis 🌱📊

Exploratory data analysis in R on a structured agribusiness dataset, focusing on statistical profiling and data visualization.

Built with Talles.

## Dataset

The dataset includes 30 observations and 4 variables:

- `num_propriedades` — quantitative discrete
- `producao_ton` — quantitative continuous
- `tipo_cultura` — qualitative nominal
- `nivel_tecnologico` — qualitative ordinal

## Workflow

- import Excel dataset
- check structure and data types
- compute descriptive statistics for one quantitative variable
- generate graphical analysis for one quantitative variable
- generate graphical analysis for one qualitative variable

## Statistics Covered

- mean
- median
- mode
- variance
- standard deviation
- coefficient of variation
- range
- quartiles
- deciles
- percentiles

## Visualizations

- histogram
- boxplot
- density plot
- Q-Q plot
- bar chart
- proportional chart

## Tech Stack

- R
- Excel
- ggplot2
- dplyr
- scales
- gridExtra
- readxl

## Notes

- `nivel_tecnologico` is explicitly modeled as an ordered factor.
- Mode for the continuous variable is estimated via grouped values.
- Dataset size is intentionally small, focused on EDA techniques.

## Authors

- Renan de Souza
- Talles
