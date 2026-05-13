# Agribusiness Data Exploratory Analysis - Exploratory data analysis in R for agribusiness datasets

<p align="left">
  <img src="https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/Excel-217346?style=for-the-badge&logo=microsoftexcel&logoColor=white" />
  <img src="https://img.shields.io/badge/ggplot2-1F77B4?style=for-the-badge&logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/dplyr-00A7E1?style=for-the-badge&logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/readxl-4E4E4E?style=for-the-badge&logo=r&logoColor=white" />
</p>

Exploratory data analysis project in R focused on agribusiness statistics, descriptive analysis, and data visualization

![Project Preview](https://i.imgur.com/uz85DwG.png)

## Introduction

Agribusiness Data Exploratory Analysis is an exploratory data analysis project built in R using a structured agribusiness dataset. The project focuses on statistical profiling, quantitative and qualitative variable analysis, and graphical visualization using ggplot2.

The workflow demonstrates common exploratory data analysis techniques used in agricultural datasets, business intelligence studies, and academic statistics projects. It is useful for users searching for R data analysis examples, agribusiness EDA projects, Excel data analysis in R, or statistical visualization with ggplot2.

## Technical Overview

The dataset contains 30 observations and 4 variables representing production metrics and categorical classifications related to agribusiness operations.

The analysis pipeline includes:
- Excel dataset import using `readxl`
- data structure validation
- factor conversion for ordinal variables
- descriptive statistical analysis
- quantitative distribution analysis
- qualitative frequency analysis
- chart generation with ggplot2

The project uses `dplyr` for data manipulation and `ggplot2` for statistical visualization. The variable `nivel_tecnologico` is explicitly modeled as an ordered factor to preserve ordinal relationships during analysis.

## User Instructions

### Dataset structure

- `num_propriedades` - quantitative discrete
- `producao_ton` - quantitative continuous
- `tipo_cultura` - qualitative nominal
- `nivel_tecnologico` - qualitative ordinal

### Workflow

1. Import the Excel dataset
2. Validate structure and data types
3. Compute descriptive statistics
4. Generate quantitative visualizations
5. Generate qualitative visualizations

### Statistics covered

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

### Visualizations

- histogram
- boxplot
- density plot
- Q-Q plot
- bar chart
- proportional chart

## Developer Instructions

### Local setup

```bash
git clone https://github.com/renansouz/<repository-name>.git
cd <repository-name>
```

### Install dependencies

```bash
Rscript -e "install.packages(c('ggplot2', 'dplyr', 'scales', 'gridExtra', 'readxl'))"
```

### Run the analysis

1. Open the R script or R Markdown file
2. Load the dataset
3. Execute the descriptive statistics workflow
4. Generate the visualizations
5. Export charts or summaries if needed

### Contribution guidelines

- Create a feature branch from `main`
- Keep pull requests focused on one change
- Use descriptive commit messages
- Document any new variables or statistical methods
- Verify all charts render correctly before opening a PR

Suggested branch names:

```bash
feature/eda-charts
fix/data-cleaning
chore/readme-update
```

## Support

[![Buy Me a Coffee](https://img.shields.io/badge/Buy_Me_a_Coffee-Support_Project-FFDD00?style=for-the-badge&logo=buymeacoffee&logoColor=black)]()
