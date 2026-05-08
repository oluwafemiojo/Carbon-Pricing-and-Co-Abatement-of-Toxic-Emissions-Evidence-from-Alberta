\# CLAUDE.md — Pham \& Roach (2024) Replication



\## Paper

"Spillover Benefits of Carbon Dioxide Cap and Trade:

Evidence from the Toxics Release Inventory"

Pham \& Roach, Economic Inquiry, Vol. 62(1), 2024

DOI: 10.1111/ecin.13162



\## Replication package

data/raw/pham\_roach\_pkg/



\## Language

\- All analysis code in R only

\- tidyverse style (dplyr, tidyr, ggplot2)

\- fixest for all panel regressions

\- did package (Callaway \& Sant'Anna) for staggered DiD

\- here::here() for all file paths — no absolute paths

\- modelsummary for output tables



\## Conventions

\- Scripts numbered: 01\_, 02\_, 03\_

\- Never modify anything in data/raw/

\- All outputs → output/tables/ or output/figures/

\- set.seed(42) before any bootstrap

\- Every script prints a completion timestamp



\## My workflow (do these in order)

1\. Read and inventory the replication package

2\. Understand the dataset structure and key variables

3\. Reproduce Table 1 exactly in R

4\. Reproduce the event study figure

5\. Then extend to Canadian NPRI / Alberta TIER data



\## Never do without asking me first

\- Change sample restrictions from the original paper

\- Switch estimators without explaining why

\- Drop or add control variables not in the original

