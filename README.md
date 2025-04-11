
# Greener on the Other Side? Mapping China’s Overseas Co-financing and Financial Innovation

Welcome! This repository accompanies our research paper, [“Greener on
the Other Side? Mapping China’s Overseas Co-financing and Financial
Innovation”](https://odi.org/en/publications/greener-on-the-other-side-mapping-chinas-overseas-co-financing-and-financial-innovation/),
and includes materials from our Methodological Annex. Here, you can
explore our analytical workflow, replicate select findings, and
(hopefully) learn from our experiences applying large language models
(LLMs) and social network analysis (SNA) to policy research on Chinese
overseas lending.

We want to emphasize that this project was a modest, exploratory effort.
We had a limited budget and spent a considerable amount of time
iterating on our approach. While it was a lot of work, it was also a lot
of fun—we ended up innovating on how to use LLMs for policy-relevant
classification tasks and how to integrate SNA for mapping co-financing
networks. Our hope is that by sharing our methods, data workflows, and
code, others can build upon what we’ve started.

[![Report: Greener on the other
side?](report_cover.png)](https://odi.org/en/publications/greener-on-the-other-side-mapping-chinas-overseas-co-financing-and-financial-innovation/)

## Key Takeaways from Our Methodology

1.  **Using LLMs for Text Classification**
    - We leveraged four different LLMs on a validation set of 300
      project descriptions to classify “green,” “brown,” “grey,” and
      “neutral” projects in Chinese overseas lending.
    - We compared model outputs to human evaluations, achieving
      encouraging—but not perfect—agreement.
    - We discuss how we worried about issues like “hallucination,” how
      we tried to mitigate these, and how we interpret confidence scores
      in classification.
2.  **Social Network Analysis (SNA) of Co-financing Relationships**
    - We created a transaction-level dataset of co-financed lending,
      identified unique transactions, and mapped them as nodes and edges
      to see which lenders tend to collaborate.
    - Our approach reveals structural patterns: which banks or
      institutions serve as “bridges,” how “green” financing networks
      differ from overall networks, etc.
    - Insights from SNA can highlight potential ways to encourage
      collaborative, green finance.
3.  **Pragmatic Approach to Reproducibility**
    - We used GitHub for version control and renv (in R) for package
      management, so you can see exactly which versions of each package
      we used.
    - Reproducibility is a ladder. We’ve taken a few steps—version
      control, documented code, pinned packages—but we acknowledge
      there’s room for improvement, like containerization or fully
      scripted data extractions, which we could not fully implement due
      to time and budget limits.
    - We encourage anyone interested in replicating or extending this
      analysis to reach out, ask questions, and submit pull requests or
      issues.
4.  **Policy Relevance & Transparency**
    - Research on Chinese overseas lending can be politically sensitive,
      so we aim for transparent methods and assumptions.
    - Our classification rules (e.g., what counts as “green” vs. “grey”
      or “brown”) may not be universal. Different scholars might define
      “green” differently. We encourage adaptation and testing
      alternative definitions in our code.

## Repository Overview

Below is a quick map of key folders and files:

    ├── R
    │   ├── defining_transactions
    │   │   └── transaction_definition_exploration.qmd
    │   ├── key_figures_and_charts
    │   │   ├── charts_and_figures.qmd
    │   │   └── social_network_analysis.R
    │   ├── llm_classification
    │   │   ├── classification-prompt.md
    │   │   ├── llm_functions.R
    │   │   ├── llm_run_full_dataset.qmd
    │   │   └── llm_validation.qmd
    │   ├── llm_validation_testing
    │   │   └── llm_validation_testing.qmd
    │   └── name_standardization
    │       ├── funder_name_standardization.qmd
    │       └── name_standardization_functions.R
    └── renv
        ├── activate.R
        ├── settings.json
        └── ...

### 1. LLM Scripts

- Path: `R/llm_classification/`
- Key Files:
  - `classification-prompt.md` – The prompt template we used with each
    LLM.
  - `llm_run_full_dataset.qmd` – Runs classification across the full
    dataset.
  - `llm_validation.qmd` – Contains code for our sample validation
    approach, including how we tested four different models and compared
    their outputs.

Use these to replicate or refine the LLM approach. For those interested
in re-running the classification, you’ll need API keys or local model
setups. Check the code comments for more details.

### 2. Social Network Analysis (SNA)

- Path: `R/key_figures_and_charts/social_network_analysis.R`
- We define how we constructed the co-financing network (nodes =
  lenders, edges = number of co-financed transactions).
- Steps to replicate:
  1.  Ensure you have the merged dataset with standardized names.
  2.  Run the code to create network objects using tidygraph or igraph.
  3.  Generate the figures in charts_and_figures.qmd or by sourcing the
      script directly.

### 3. Name Standardization

- Path: `R/name_standardization/`
- Key Files:
  - `funder_name_standardization.qmd` – Our manual or semi-automated
    approach to unifying variations of lender names.
  - `name_standardization_functions.R` – Helper functions for fuzzy
    matching and pattern replacement.

We suggest you review this before working with our data. Spelling
inconsistencies or alternative naming conventions can create major
headaches in linking transactions together.

### 4. Reproducing Key Figures from the Paper

- Path: `R/key_figures_and_charts/charts_and_figures.qmd`
- Contains R code that runs all the data wrangling and plotting steps
  for the main visuals in our paper.
- To replicate:
  1.  Install or activate our renv environment.
  2.  Run charts_and_figures.qmd end to end (e.g., in RStudio’s “Render”
      or from the command line).
  3.  Outputs will appear in the output/figures/ directory.

## Installation & Setup

1.  Clone this repository

``` bash
git clone https://github.com/YourOrg/YourRepoName.git
```

2.  Activate the R environment

    - Make sure you have the renv package installed.
    - In R, run:

    ``` r
    renv::activate()
    renv::restore()
    ```

    This will install the required package versions.

3.  Check for data availability

    - Some data files may be too large to store in the repo. Look for
      references to external data links, or contact us if you need
      further guidance.

4.  Run analyses

    - Most .qmd or .R scripts contain a “Setup” section that loads
      libraries and data.
    - Adjust file paths if needed.

## Limitations & Future Directions

- We spent considerable time on this project despite a modest budget and
  a lot of labor intensity. This means we didn’t have the resources to
  build fully containerized or cross-platform reproducibility solutions.
  We hope that what we’ve done—version control, pinned packages, and
  open-sourcing everything—still moves the needle on reproducible
  research.
- Classifying “green” vs. “grey” vs. “brown” projects is inherently
  subjective and reliant on the text we had. Other researchers might
  define or interpret “green” differently (e.g., whether nuclear or
  large-hydro counts). We try to make our assumptions explicit so you
  can adapt the definitions.
- LLM-based classification is still experimental—and these models can
  “hallucinate.” We tested models against a ~300-observation sample and
  cross-checked them with human coders. Use caution and replicate our
  checks if you plan to rely on these outputs.

## Contact & Feedback

- If you have questions or feedback, feel free to open an Issue or reach
  out via email. We welcome comments, suggestions, and even pull
  requests if you spot areas for improvement.
- Because Chinese overseas lending is politically sensitive, we
  encourage robust debate, alternative definitions, and thoughtful
  examination of the data.

------------------------------------------------------------------------

Thank you for your interest. We hope you find this repository helpful
for exploring new frontiers of applying large language models and social
network analysis to policy-relevant challenges, and we look forward to
seeing how you might build on our work.

Please cite this work if it helps you in your own research (citation
details are in our main paper). Good luck, and don’t hesitate to reach
out with any questions!

------------------------------------------------------------------------

Last updated: 2025-04-11
