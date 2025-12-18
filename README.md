````markdown
# Mobility-Network–Informed Bayesian Disease Mapping (Montreal & Toronto)

## Overview
This repository implements a reproducible workflow for **areal disease mapping** that augments standard spatial neighborhood structure with **mobility-derived connectivity**. The pipeline (i) aggregates origin–destination (OD) mobility “stop counts”, (ii) constructs **mobility interaction graphs** and **contiguity adjacency** matrices, (iii) prepares borough-level analytic datasets (including expected counts), and (iv) fits **Bayesian hierarchical models in Stan** (Poisson likelihood with offsets) to estimate spatial risk and compare neighborhood definitions.

Primary pilot: **Montreal boroughs (34 areas)**.  
Extension: **Toronto/Ontario** (work-in-progress).

---

## Repository Structure
```text
Mtl_borough_disease_mapping/
  Mtl_M/
    MTL_Boroughs_2021_m1_to_m...    # Montreal borough boundaries / mapping resource
    data_mtl_boroughs.csv           # (Optional) processed borough-level dataset
    aggregate.R                     # OD stop-count aggregation utilities
    get_routing_pop_stop_map.R      # OD routing / population / stop-count mapping
    get_cov.Rmd                     # covariate extraction / preprocessing notebook
    mtl_data.R                      # Montreal analytic dataset construction
    New_E.R                         # expected count (E_i) computation utilities
    mtl_data_with_new_E.R           # analytic dataset including E_i / offsets
    mtl_new_E_weight_data.R         # mobility-derived weights / matrices
    mtl_graph_data.R                # mobility graph construction (interaction network)
    mtl_graph_Contiguity.R          # contiguity neighborhood construction
    map_diease_count.R              # choropleths / mapping utilities
    table.R                         # summary tables (descriptives / model outputs)

Scott_data_toy_example/
  ...                               # toy example for development & debugging

Toronto/
  Model_Simple/
    ON_data.R                       # Ontario/Toronto data preparation (initial)
    ON_newE.R                       # expected count computation for ON data
    Cov.R                           # covariate processing for ON data
    Scatter.Rmd                     # exploratory plots / diagnostics
````

---

## Data Notes

### Mobility (OD stop counts)

* Monthly OD mobility (“stop counts”) are assumed to be stored in a database or local extracts.
* **Credentials / raw mobility data are not included** in this repository.

### Geographic units

* Montreal pilot uses **boroughs** (publicly available outcomes at this level).
* Toronto/Ontario scripts are preliminary and intended to generalize the workflow.

---

## Methods

### 1) Mobility network construction

We represent mobility as a directed, weighted graph:

* Nodes: boroughs
* Edges: mobility flows between boroughs
* Edge weights: population-normalized stop counts

Let $G(N, E)$ denote the mobility graph and let $I$ be a **mobility-based interaction matrix** (derived from OD data). For comparison, we also construct a **contiguity adjacency matrix** $C$ based on shared borders.

> Implementation: `mtl_graph_data.R`, `mtl_new_E_weight_data.R`, `mtl_graph_Contiguity.R`

---

### 2) Disease mapping models in Stan

Let $y_i$ be observed case counts in area $i$ and $E_i$ the expected counts. We model:
$$
y_i \sim \text{Poisson}(\mu_i), \qquad \log(\mu_i) = \log(E_i) + \eta_i
$$
where $\log(E_i)$ is an **offset** and $\eta_i$ is a linear predictor.

Expected counts are computed using age-stratified rates and area populations (schematically):
$$
E_i = \sum_{a} n_{i,a}, r_a
$$
where $n_{i,a}$ is the population of area $i$ in age group $a$ and $r_a$ is the reference rate for age group $a$.

A typical spatial model specification is:
$$
\eta_i = \alpha + \mathbf{x}_i^\top \boldsymbol{\beta} + u_i + v_i
$$

* $\alpha$: intercept
* $\mathbf{x}_i$: covariates (e.g., socio-demographics)
* $v_i$: i.i.d. (non-spatial) heterogeneity
* $u_i$: structured spatial effect (ICAR/BYM) using an adjacency structure

We compare two choices of adjacency:

* **Contiguity-based** adjacency ($C$)
* **Mobility-interaction** adjacency (from $I$)

---

## Models Implemented (Montreal Pilot)

* **M1:** Poisson GLM with covariates only (no random effects)
* **M2:** Adds i.i.d. random effects (non-spatial heterogeneity)
* **M3:** BYM model with ICAR spatial effects using **contiguity** adjacency
* **M4:** BYM model using **mobility interaction** adjacency

  * includes an additional within-borough mobility (“loop/stop count”) covariate

Stan sampling (pilot):

* NUTS / HMC, multiple chains, warmup + sampling iterations, with convergence checks via trace/density diagnostics.

---

## Recommended Workflow

### Montreal (borough-level pilot)

1. **Aggregate OD mobility data**

   * `Mtl_M/aggregate.R`
2. **Build mobility interaction weights / matrices**

   * `Mtl_M/mtl_new_E_weight_data.R`
   * `Mtl_M/mtl_graph_data.R`
3. **Build contiguity neighborhood matrix**

   * `Mtl_M/mtl_graph_Contiguity.R`
4. **Prepare analytic dataset**

   * `Mtl_M/get_cov.Rmd`
   * `Mtl_M/New_E.R`
   * `Mtl_M/mtl_data.R`
   * `Mtl_M/mtl_data_with_new_E.R`
5. **Visualization and reporting outputs**

   * `Mtl_M/map_diease_count.R`
   * `Mtl_M/table.R`

### Toronto / Ontario (work-in-progress)

* `Toronto/Model_Simple/` contains initial scripts to generalize the pipeline to smaller-area ON/Toronto datasets.

---

## Outputs

Typical outputs include:

* Mobility network visualizations (**interaction vs contiguity**)
* Borough-level choropleths of observed counts and (optionally) estimated risk
* Interaction/adjacency matrices used for modeling
* Stan model summaries and diagnostics (trace/density plots)
* Regression tables and model comparison metrics (e.g., WAIC)

---

## Reproducibility Notes

* Some scripts assume local file paths and/or database connections. Configure:

  * input data locations
  * database credentials (if applicable)
  * boundary/shapefile paths
* For Bayesian models, record:

  * random seed
  * number of chains / iterations
  * priors and scaling/standardization choices

---

## Software

This project is implemented primarily in **R** and **Stan**. Typical R dependencies may include:

* `tidyverse`, `data.table`
* `sf`, `tmap`/`ggplot2` (mapping)
* `igraph` (network construction)
* `rstan` or `cmdstanr` (Bayesian inference)

> Exact package versions may differ across environments.

---

## Report

A detailed description of the Montreal pilot analysis, covariate screening, model specifications (M1–M4), and preliminary findings is provided in:

* `Summer2024_report-revised.pdf`

---

## Acknowledgements

* Supervisor: Prof. Hiroshi Mamiya
* Funding: CIHR / CPHO Catalyst Grant (Future of Public Health)

---

## Contact

**Chi Zhang**
chi.zhang12@mail.mcgill.ca

```


