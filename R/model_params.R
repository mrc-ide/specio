get_model_params <- function() {
  list(
    ## Number of sexes (male, female)
    NG = 2,
    ## Number of age groups (5 year groups from 0-4, ..., 75-79, 80+)
    AG = 17,
    ## Number of disease stages (CD4 count stages: >500, 350-499, 250-349,
    ## 200-249, 100-199, 50-99, <5)
    DS = 7,
    ## Number of treatment stages (<6 months, 6-12 months, >1 year)
    TS = 3,
    ## Number of age groups for fertility (15-19, 20-24, ..., 45-49)
    fAG = 7,
    ## Number of disease stages for paediatric HIV infection
    PAED_DS = 6
  )
}
