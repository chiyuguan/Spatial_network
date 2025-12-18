# Mobility-Network–Informed Bayesian Disease Mapping (Montreal & Toronto)

## Overview
This repository provides an end-to-end workflow for **areal disease mapping** that augments standard spatial neighborhood structure with **mobility-derived connectivity**. The pipeline:

1. Aggregates origin–destination (OD) mobility “stop counts”
2. Constructs **mobility interaction graphs** and **contiguity adjacency** matrices
3. Builds borough-level analytic datasets (including expected counts)
4. Fits **Bayesian hierarchical disease mapping models in Stan** to estimate risk and compare neighborhood definitions

Primary pilot: **Montreal boroughs (34 areas)**.  
Extension: **Toronto / Ontario** (work-in-progress).

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


