
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A two-stage Subgrouping Analysis with Application to the Lung Dataset

Correct indentification of subgroups of a heterogeneous population is an
important step in developing precision medicine. In this report, we
develop a two-stage data-driven efficient method to detect the subgroups
existing in the lung cancer population under the \(p>>n\) situation. We
assume the heterogeneity mainly comes from the unobserved latent
factors. After adjusting for the effects of a subset of the covariates
\(\x_i = (X_{i1},\ldots,X_{ip})\) using the Lasso procudure, we apply
the k-means algorithm to divide the heterogenous population into
subgroups using dynamic programming based the elbow rule with the
modified BIC criterion. Finally we verify the proposed method by
applying our method to the publicly available lung cancer genomic data.

## Data

### Abstract

Publicly available lung cancer genomic data from the Chemores Cohort
Study. This data is part of an integrated study of mRNA, miRNA and
clinical variables to characterize the molecular distinctions between
squamous cell carcinoma (SCC) and adenocarcinoma (AC) in Non Small Cell
Lung Cancer (NSCLC) aside large cell lung carcinoma (LCC). Tissue
samples were analysed from a cohort of 123 patients, who underwent
complete surgical resection at the Institut Mutualiste Montsouris
(Paris, France) between 30 January 2002 and 26 June 2006. All the
patients belong to NSCLC. The studied outcome was the “Disease-Free
Survival Time”. Patients were followed until the first relapse occurred
or administrative censoring. In this genomic dataset, the expression
levels of Agilent miRNA probes (p=939) were included from the n=123
cohort samples. The miRNA data contains normalized expression levels.
See below the paper by Lazar et al. (2013) and Array Express data
repository for complete description of the samples, tissue preparation,
Agilent array technology, and data normalization. In addition to the
genomic data, five clinical variables, also evaluated on the cohort
samples, are included as continuous variable (‘Age’) and nominal
variables (‘Type’,‘KRAS.status’,‘EGFR.status’,‘P53.status’).

### Availability

Data is available here
<http://fafner.meb.ki.se/personal/yudpaw/?page_id=11>.

## Code

### Abstract

The authors have released all the relevant data in
<https://github.com/chencanyi1997/subgroup_AAS>. All the data for
processing data and analysisi for this report were done in R. The
corresponding code is provided to take exploratory data analysis on the
raw data; conduct various preprocessing steps; fit a two-stage subgroup
analysis; and generate plots used in this report.

  - computeBIC.R: compute the proposal BIC.
  - Reproduce\_Subgroup.R: reproduce all the simulations and figures.

### Optional Information

R version 3.6.3 (2020-02-29, “Holding the Windsock”) we used for the
analysis in this report. The necessary R packages are

  - tidyverse, version 1.2.1
    (<https://cran.r-project.org/web/packages/tidyverse/index.html>)
  - glmnet, version 3.0.1
    (<https://cran.r-project.org/web/packages/glmnet/index.html>)
  - Amelia, version 1.7.6
    (<https://cran.r-project.org/web/packages/Amelia/index.html>)
  - Ckmeans.1d.dp, version 4.3.2
    (<https://cran.r-project.org/web/packages/Ckmeans.1d.dp/index.html>)
  - PRIMsrc, version 0.8.2
    (<https://cran.r-project.org/web/packages/PRIMsrc/index.html>)

## Instructions for Use

### Reproducibility

All data preprocessing and simulations are reproduced. The workflow
information is contained in the Reproduce\_Subgroup.R script. The
general steps are:

1.  Take exploratory data analysis on the raw data.
2.  Conduct data processing/preparation for the analyses.
3.  Fit the two-stage subgroup analysis to the preprocessed data.
4.  Generate all plots in the report.

## R Shiny

We deploy our simulations with R Shiny in
<https://chency1997.shinyapps.io/shiny/>.
