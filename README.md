Stakeholder Perception Gaps in Forest Governance

Overview
This repository contains the R code used for the statistical analysis of stakeholder perception gaps regarding forest governance conflicts in the Arasbaran forests, northwestern Iran.

The study examines differences in perceptions among four stakeholder groups across 16 forest governance conflict issues.

Study Population and Data
The study includes 482 respondents from four stakeholder groups:

- Local residents: 387
- Government experts: 30
- NGO members: 32
- Academics and researchers: 33

Responses were collected using a five-point Likert scale across 16 conflict issues (C1–C16).

Statistical Methods
The analyses include:

- Kruskal–Wallis tests to compare the four stakeholder groups across the 16 conflict issues.
- Pairwise Mann–Whitney U tests for group comparisons.
- Bonferroni correction for six pairwise comparisons within each conflict issue (adjusted significance threshold: 0.0083).
- Holm adjustment as a sensitivity analysis.
- Effect size calculation (r).
- Mean-rank summaries to examine the direction of differences.
- Heatmap visualisation of stakeholder perception patterns.

Repository Structure
The main R analysis script is located in the root directory of this repository.

Software Requirements
The analysis was conducted using R version 4.5.2.

Required packages:

- readxl
- ggplot2
- viridis

Install the required packages using:

```r
install.packages(c("readxl", "ggplot2", "viridis"))
```

How to Run
1. Download or clone this repository.
2. Open the main R script in RStudio.
3. Ensure that the required packages are installed.
4. Make the input dataset available at the file location specified in the script.
5. Run the script.

Data Availability
The respondent-level dataset is not publicly available due to ethical and consent-related restrictions. Data access may be considered upon reasonable request to the corresponding author, subject to applicable ethical and institutional conditions.

Reproducibility
The analysis code and statistical procedures are provided to support transparency and reproducibility of the reported results.

Manuscript Status
Manuscript in preparation.

Citation
A full citation will be added after publication.

Contact
For questions about the code or data availability, please contact the corresponding author.
