*Note: This is a work in progress.*

# Conversational News: Analyzing News Provider Diversity in ChatGPT's Daily News Responses

This repository hosts the analysis code and reproduction package for the paper. To replicate the report, clone the repository, restore the environment from `renv.lock` and render `Diversity.qmd`. 

Run these commands (after installing the `renv` package) to reproduce the report.

```
git clone https://github.com/Kudusch/chatgpt_news/
cd chatgpt_news
R --no-echo -e 'renv::init()'
R --no-echo -e 'renv::restore()'
R --no-echo -e 'quarto::quarto_render("docs/Diversity.qmd")'
```
