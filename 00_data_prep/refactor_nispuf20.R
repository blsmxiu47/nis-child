# Prepares raw data downloaded from CDC (NISPUF20) for analysis by re-factoring
# variables, handling missing values

# CONSTANTS
IN_PATH = "FILEPATH"
OUT_PATH = "FILEPATH"


# Read in data, if necessary
# NISPUF20 <- read.csv(IN_PATH, "IN_FILE")


# Social and Demographic Fields
## Age Groups (AGEGRP)
NISPUF20$AGEGRP <- factor(
  NISPUF20$AGEGRP, 
  levels = c(1, 2, 3), 
  labels = c(
    "19-23 months", 
    "24-29 months", 
    "30-35 months"
  )
)

## Number of people in household (C1R)
NISPUF20$C1R <- factor(
  NISPUF20$C1R, 
  levels = c(2, 3, 4, 5, 6, 7, 8), 
  labels = c(
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8+"
  )
)

## Relationship of Respondent to Child (C5R)
NISPUF20$C5R <- factor(
  NISPUF20$C5R,
  levels = c(1, 2, 3, 4, 77, 99),
  labels = c(
    "mother or female guardian",
    "father or male guardian",
    "grandparent",
    "other family/friend",
    "don't know",
    "refused"
  )
)

## (US) Region
NISPUF20$CEN_REG <- replace(NISPUF20$CEN_REG, is.na(NISPUF20$CEN_REG), 99)
NISPUF20$CEN_REG <- factor(
  NISPUF20$CEN_REG,
  levels = c(1, 2, 3, 4, 99),
  labels = c(
    "northeast",
    "midwest",
    "south",
    "west",
    "territory or missing"
  )
)

## Children under 18 in HH
NISPUF20$CHILDNM <- factor(
  NISPUF20$CHILDNM,
  levels = c(1, 2, 3),
  labels = c(
    "one",
    "two or three",
    "four or more"
  )
)

## Child received WIC benefits?
NISPUF20$CWIC_01 <- factor(
  NISPUF20$CWIC_01,
  levels = c(1, 2, 3, 77, 99),
  labels = c(
    "yes",
    "no",
    "never heard of wic",
    "don't know",
    "refused"
  )
)

## Education of mother categories
NISPUF20$EDUC1 <- factor(
  NISPUF20$EDUC1,
  levels = c(1, 2, 3, 4),
  labels = c(
    "< 12 years",
    "12 years",
    "> 12 years, non-college grad",
    "college grad"
  )
)

## Firstborn?
NISPUF20$FRSTBRN <- factor(
  NISPUF20$FRSTBRN,
  levels = c(1, 2),
  labels = c(
    "no",
    "yes"
  )
)
## Hispanic?
NISPUF20$I_HISP_K <- factor(
  NISPUF20$I_HISP_K,
  levels = c(1, 2),
  labels = c(
    "hispanic",
    "non-hispanic"
  )
)

## Poverty Status
NISPUF20$INCPOV1 <- factor(
  NISPUF20$INCPOV1,
  levels = c(1, 2, 3, 4),
  labels = c(
    "above poverty, >$75k",
    "above poverty, <=$75k",
    "below poverty",
    "unknown"
  )
)

## Income bucket
NISPUF20$INCQ298A <- factor(
  NISPUF20$INCQ298A,
  levels = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 77, 99),
  labels = c(
    "$0 - $7500",
    "$7501 - $10000",
    "$10001 - $17500",
    "$17501 - $20000",
    "$20001 - $25000",
    "$25001 - $30000",
    "$30001 - $35000",
    "$35001 - $40000",
    "$40001 - $50000",
    "$50001 - $60000",
    "$60001 - $75000",
    "$75001+",
    "don't know",
    "refused"
  )
)

## Language of interview (Eng or Esp or Other)
NISPUF20$LANGUAGE <- factor(
  NISPUF20$LANGUAGE,
  levels = c(1, 2, 3),
  labels = c(
    "english",
    "spanish",
    "other"
  )
)

## Age of mother (<30yr or >=30yr)
NISPUF20$M_AGEGRP2 <- factor(
  NISPUF20$M_AGEGRP2,
  levels = c(1, 2),
  labels = c(
    "<=29 years",
    ">=30 years"
  )
)

## Marital status of mother
NISPUF20$MARITAL2 <- factor(
  NISPUF20$MARITAL2,
  levels = c(1, 2),
  labels = c(
    "married",
    "not married at time of interview"
  )
)

## Geographic mobility status (interview date vs at birth)
NISPUF20$MOBIL_I <- factor(
  NISPUF20$MOBIL_I,
  levels = c(1, 2),
  labels = c(
    "moved from different state",
    "did not move from different state"
  )
)

## Race of child (Black or White)
NISPUF20$RACE_K <- factor(
  NISPUF20$RACE_K,
  levels = c(1, 2, 3),
  labels = c(
    "white only",
    "black only",
    "other + multiple race"
  )
)

## Race/Ethnicity of child (Hispanic?)
NISPUF20$RACEETHK <- factor(
  NISPUF20$RACEETHK,
  levels = c(1, 2, 3, 4),
  labels = c(
    "hispanic",
    "non-hispanic white only",
    "non-hispanic black only",
    "non-hispanic other + multiple race"
  )
)

## Home being rented or owned
NISPUF20$RENT_OWN <- factor(
  NISPUF20$RENT_OWN,
  levels = c(1, 2, 3, 77, 99),
  labels = c(
    "owned or being bought",
    "rented",
    "other arrangement",
    "don't know",
    "refused"
  )
)

## Sex of child (M or F)
NISPUF20$SEX <- factor(
  NISPUF20$SEX,
  levels = c(1, 2),
  labels = c(
    "male",
    "female"
  )
)

# Geographic variables
## State of residence (FIPS code)
postal_codes <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID",
  "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", 
  "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", 
  "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "PR"
)
fips <- c(
  1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
  26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45,
  46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 72
)
states <- data.frame(
  postal_code = postal_codes,
  fips = fips
)
NISPUF20$state <- factor(
  NISPUF20$STATE,
  levels = fips,
  labels = postal_codes
)

## Number of providers identified by respondent
NISPUF20$D6R <- replace(NISPUF20$D6R, is.na(NISPUF20$D6R), 99)
NISPUF20$D6R <- factor(
  NISPUF20$D6R,
  levels = c(0, 1, 2, 3, 99),
  labels = c(
    "0",
    "1",
    "2",
    "3+",
    "missing"
  )
)

## Number of providers responding with vaccination data for child
NISPUF20$N_PRVR <- factor(
  NISPUF20$N_PRVR,
  levels = c(0, 1, 2, 3),
  labels = c(
    "0",
    "1",
    "2",
    "3+"
  )
)

## Provider facility types (public, private, mixed, etc.)
NISPUF20$PROV_FAC <- replace(NISPUF20$PROV_FAC, is.na(NISPUF20$PROV_FAC), 99)
NISPUF20$PROV_FAC <- factor(
  NISPUF20$PROV_FAC,
  levels = c(1, 2, 3, 4, 5, 99),
  labels = c(
    "all public",
    "all hospital",
    "all private",
    "all military/other",
    "mixed",
    "missing"
  )
)

## Whether all or some or none of providers reported vaccinations to 
## immunization registry
NISPUF20$REGISTRY <- replace(NISPUF20$REGISTRY, is.na(NISPUF20$REGISTRY), 99)
NISPUF20$REGISTRY <- factor(
  NISPUF20$REGISTRY,
  levels = c(1, 2, 3, 4, 99),
  labels = c(
    "all providers",
    "some providers but probably not all",
    "no providers",
    "unknown",
    "missing"
  )
)

## Whether all or some or none of providers ordered vaccines from state/local
## health dept.
NISPUF20$VFC_ORDER <- replace(NISPUF20$VFC_ORDER, is.na(NISPUF20$VFC_ORDER), 99)
NISPUF20$VFC_ORDER <- factor(
  NISPUF20$VFC_ORDER,
  levels = c(1, 2, 3, 4, 99),
  labels = c(
    "all providers",
    "some providers but probably not all",
    "no providers",
    "unknown",
    "missing"
  )
)


# Write prepared data object to file
write.csv(NISPUF20, paste0(OUT_PATH, "nispuf20_refactored.csv"))
