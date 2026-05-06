# Tennessee Reservoir Nutrient Analysis

A Python-based analysis pipeline for examining nutrient dynamics and water quality patterns across Tennessee reservoirs. This repository contains data preprocessing utilities, exploratory analyses, and statistical workflows supporting ongoing applied research on reservoir water quality.

## Project Overview

Reservoirs are critical components of regional water resources, and their nutrient dynamics (nitrogen, phosphorus, and related water quality indicators) influence downstream ecosystem health, drinking water quality, and recreational use. This project assembles long-term monitoring data into a reproducible analytical pipeline that supports:

- Cleaning and harmonization of multi-source water quality records
- Exploratory data analysis of seasonal, spatial, and inter-reservoir patterns
- Statistical assessment of relationships among nutrients and physicochemical variables
- Reproducible outputs intended for both technical and non-technical audiences

## Repository Structure

```
Reservoir_Nutrients_Analysis/
├── tnwater/              # Python package for data preprocessing and helpers
├── notebooks/            # Jupyter notebooks for EDA and statistical analysis
├── data/                 # Raw and processed data (see Data section)
├── outputs/              # Figures, tables, and exported results
├── environment.yml       # Conda environment specification
└── README.md
```

## Data

Data are drawn from the Tennessee Nutrient Database hosted by the Tennessee Department of Environment and Conservation long-term reservoir nutrient monitoring records (https://tdec.tn.gov/nutrient/). Raw and processed data are organized under `data/` and loaded into pandas DataFrames through utilities in the `tnwater` package.

Variables include nutrient concentrations (total nitrogen, total phosphorus, nitrate, nitrite, ammonia, orthophosphate), chlorophyll-a, dissolved oxygen, temperature, pH, and related physicochemical measurements collected across multiple reservoirs and sampling stations.


## Methods

The analysis emphasizes transparent, reproducible statistical workflows:

- **Preprocessing**: pandas-based cleaning, harmonization across sources, and quality control filters implemented in the `tnwater` package
- **Exploratory analysis**: distributional summaries, time series visualization, and spatial comparisons using pandas, seaborn, and matplotlib
- **Correlation analysis**: Pearson correlations alongside Bayes Factor computation via `pingouin` for evidence-weighted assessment of nutrient relationships
- **Reporting**: figures and summary tables exported for inclusion in technical reports and stakeholder deliverables

## Tech Stack

- **Language**: Python 3.x
- **Core libraries**: pandas, NumPy, seaborn, matplotlib, pingouin
- **Environment**: conda
- **IDE**: Positron

## Installation

Clone the repository and create the conda environment:

```bash
git clone https://github.com/SpencerWomble/Reservoir_Nutrients_Analysis.git
cd Reservoir_Nutrients_Analysis
conda env create -f environment.yml
conda activate reservoir-nutrients
```

Install the local `tnwater` package in editable mode:

```bash
pip install -e .
```

## Usage

A typical workflow:

1. Place raw data files in `data/raw/` (or follow the loading utilities in `tnwater`).
2. Run preprocessing to clean and harmonize the data:
   ```bash
   python -m tnwater.preprocess
   ```
3. Open the EDA and analysis notebooks in `notebooks/` to reproduce figures and statistical summaries.

Example loading processed data:

```python
import pandas as pd
df = load_water_quality("../data/wq_data_for_tennessee.csv")

print(df.head())
```

## Status

This project is under active development. Current priorities include expanding the EDA notebooks, formalizing the statistical analysis, and producing a self-contained dashboard summarizing results for a non-technical audience.

## Author

**Spencer Womble**
Quantitative ecologist and applied statistician
[www.linkedin.com/in/spencer-womble] | [swomble@tntech.edu]

## License

[MIT, CC BY 4.0, or other. Add LICENSE file accordingly.]

## Acknowledgments

Funding was provided by the Tennessee Department of Environment and Conservation (TDEC)
