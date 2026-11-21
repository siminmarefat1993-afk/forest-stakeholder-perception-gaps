Stakeholder Perception Gaps in Forest Governance

Research Article – Supplementary R Code

1. Project Overview

This repository contains the R scripts developed for the statistical analyses and graphical presentation of results reported in the research manuscript:

Stakeholder Perception Gaps in Forest Governance: Who Disagrees, About Which Conflicts, and by How Much?

This study examines differences in stakeholder perceptions of 16 conflict issues related to forest governance in the Arasbaran forests of northwestern Iran.

The main objective is to identify which stakeholder groups differ in their assessments, the specific conflict issues associated with these differences, the direction of perceptual divergence, and the magnitude of the observed gaps.

This repository is provided to promote transparency, facilitate independent evaluation, and support the reproducibility of the statistical analyses reported in the manuscript.

2. Study Population and Data

The study is based on questionnaire data collected from 482 respondents representing four stakeholder groups:

- Local residents (n = 387)
- Government experts (n = 30)
- Non-governmental organisation (NGO) members (n = 32)
- Academics and researchers (n = 33)

Sixteen conflict issues were assessed using a five-point Likert scale, ranging from 1 (strongly disagree) to 5 (strongly agree).

The conflict variables are coded C1–C16 in the analytical scripts.

3. Statistical Methods

All statistical analyses were conducted using R (version 4.5.2).

The analytical procedures include:

3.1. Kruskal–Wallis Test

The Kruskal–Wallis test was used to examine overall differences among the four stakeholder groups for each of the 16 conflict issues.

3.2. Pairwise Mann–Whitney U Tests

Two-sided Mann–Whitney U tests were conducted to identify differences between stakeholder pairs, resulting in 96 pairwise comparisons across the 16 conflict issues.

3.3. Multiple-Comparison Adjustments

Bonferroni correction was applied to control the family-wise error rate across the six pairwise comparisons within each conflict issue, using a significance threshold of p < 0.0083.

The Holm procedure was additionally applied as a sensitivity analysis to assess the robustness of the findings.

3.4. Effect Size Estimation

Effect size (r) was calculated to quantify the magnitude of differences between stakeholder groups.

3.5. Direction of Perception Gaps

Mean ranks were compared to determine which stakeholder group reported higher ratings for each conflict issue.

3.6. Heat Map Visualisation

A heat map was developed to illustrate the distribution, direction, and magnitude of statistically significant perception gaps across the 16 conflict issues and six stakeholder pairs.

4. Repository Structure

The repository is organised as follows:

- `R/01_data_validation.R`: Importing and validating the respondent-level dataset.
- `R/02_kruskal_wallis.R`: Conducting the omnibus Kruskal–Wallis tests.
- `R/03_pairwise_mann_whitney.R`: Performing pairwise Mann–Whitney U tests and calculating effect sizes.
- `R/04_multiple_testing_and_summaries.R`: Applying Bonferroni and Holm adjustments and generating summary tables.
- `R/05_heatmap.R`: Generating the heat map.
- `data/`: Directory for the input dataset.
- `results/tables/`: Directory for generated statistical tables.
- `results/figures/`: Directory for generated figures.

5. Software Requirements

- R version: 4.5.2
- RStudio: Recommended for project management and script execution.

The required R packages are:

- `readxl`
- `ggplot2`
- `viridis`

Install the required packages using:

```r
install.packages(c("readxl", "ggplot2", "viridis"))
```

6. How to Run the Analysis

1. Install R (version 4.5.2) and RStudio.
2. Download or clone this repository.
3. Place the authorised input Excel dataset in the `data/` directory.
4. Open the RStudio project file.
5. Execute the scripts sequentially, following their numerical order in the `R/` directory.
6. Generated statistical tables and figures will be saved in the corresponding subdirectories of `results/`.

The input dataset should contain a `Groups` variable and 16 conflict variables coded C1–C16.

7. Data Availability

The respondent-level dataset is not included in this public repository.

Access to the underlying data is subject to applicable research ethics, participant consent, and data-sharing conditions. Requests concerning data access may be directed to the corresponding author, subject to these conditions.

8. Reproducibility and Transparency

The R scripts are provided to document the statistical workflow and facilitate independent examination of the analytical procedures reported in the manuscript.

The repository supports the examination of data validation, statistical testing, multiple-comparison adjustments, effect-size estimation, and graphical presentation.

9. Citation

Please cite the associated research article when using or referring to these analytical scripts:

Marefat, S. et al. Stakeholder Perception Gaps in Forest Governance: Who Disagrees, About Which Conflicts, and by How Much?

Manuscript under review.

10. Contact

For questions regarding the analytical procedures or supplementary code, please contact the corresponding author.
