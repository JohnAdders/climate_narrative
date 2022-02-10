tab_intro_ui <- function() {
  out <- list(
    p("This online climate scenario analysis tool is designed to support firms
      in assessing their climate-related risks and opportunities."),
    h3("Target audience"),
    p("The CFRF Online Scenario Analysis tool content has been written by a cross-industry
      working group under the auspices of the Climate Financial Risk Forum (CFRF).
      It is aimed at banks, asset managers and insurers of all sizes, and may be of interest
      to other institutions, such as pension schemes."),
    p("The tool summarises the relevant climate-related risks and opportunities for banks,
    insurers and asset managers based on the business activities, products, or risks
    of the firm and the materiality of different lending exposure types, underwriting classes,
    asset classes, and economic sectors for the firm. These descriptions are based on
    the NGFS scenarios which were updated in June 2021."),
    h3("How can the tool be used?"),
    p("Firms are required to select the relevant NGFS scenario (i.e. Orderly, Disorderly,
    or Hot House World) and input some basic information regarding their business activities, products, and materiality of exposure to an asset class, sector, or liability class, as follows: 
    - High: One of your top 5 exposures or more than 10% of total assets
    - Medium: 5% - 10% of total assets
    - Low: below 5% of total assets
    The tool would then output an executive summary and a detailed long form report. Based on materiality and the NGFS scenario selected, the tool will output different levels of detail in the reports. Where low or medium materiality are selected, the tool will output a summary narrative. Where high materiality is selected, the tool will output a more detailed narrative.
    The executive summary covers the following:
    -	A summary of the key assumptions and parameters used in the selected NGFS scenario
    -	For each sector, underwriting and asset class as well as lending exposure type (ordered from high to low materiality):
    -	Tables summarising the selected exposure type and highlighting the materiality
    -	A summary for high materiality exposures 
    -	A table of one-liners for medium and low materiality exposures
    The long form report covers the following:
    -	A description of key assumptions and parameters used in the selected NGFS scenario
    -	For each sector, underwriting and asset class as well as lending exposure type (ordered from high to low materiality):
    -	A description of each lending exposure type, underwriting class, asset class and sector selected
    -	Detail narratives for high materiality exposures 
    -	A summary for medium and low materiality
    ")
  )
  out
}