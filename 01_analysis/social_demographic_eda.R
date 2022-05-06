# Imports
library(dplyr)
library(forcats)
library(ggplot2)
library(reshape2)
library(rlang)
library(stringr)


# CONSTANTS
DATA_PATH <- "data/"


# Columns of interest
response_cols <- c(
  "SEQNUMC",
  "PDAT",
  "YEAR"
)
demographic_cols <- c(
  "AGEGRP",
  "C1R",
  "C5R",
  "CEN_REG",
  "CHILDNM",
  "CWIC_01",
  "EDUC1",
  "FRSTBRN",
  "I_HISP_K",
  "INCPORAR",
  "INCPORAR_I",
  "INCPOV1",
  "INCQ298A",
  "LANGUAGE",
  "M_AGEGRP2",
  "MARITAL2",
  "MOBIL_I",
  "RACE_K",
  "RACEETHK",
  "SEX",
  "ESTIAP20",
  "EST_GRANT",
  "STATE",
  "N_PRVR",
  "D7",
  "PROV_FAC",
  "REGISTRY",
  "VFC_ORDER"
)


# Util Functions
plot_value_counts <- function (df, x, xlab = {{x}}, xt_rot = 0) {
  ggplot(df, aes({{x}})) +
    geom_bar() +
    xlab(xlab) +
    ylab("Count") +
    ggtitle(str_glue("Count of {xlab}")) +
    theme(
      axis.text.x = element_text(
        angle = xt_rot
      )
    )
}

normalize <- function (x, X, minimum = 0, maximum = 1) {
  return ((x - min(X)) / (max(X) - min(X)) * (maximum - minimum) + minimum)
}


# Overall Sample EDA
## Social and Demographic Fields
### Age Groups (AGEGRP)
NISPUF20$AGEGRP <- factor(
  NISPUF20$AGEGRP, 
  levels = c(1, 2, 3), 
  labels = c(
    "19-23 months", 
    "24-29 months", 
    "30-35 months"
  )
)
plot_value_counts(NISPUF20, AGEGRP, "Age Group")

### Number of people in household (C1R)
NISPUF20$C1R <- factor(
  NISPUF20$C1R, 
  levels = c(2, 3, 4, 5, 6, 7, 8), 
  labels = c(
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8+"
  )
)
plot_value_counts(NISPUF20, C1R, "Number in HH")

### Relationship of Respondent to Child (C5R)
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
plot_value_counts(
  NISPUF20, 
  C5R, 
  "Relationship of Respondent to Child", 
  xt_rot = 20
)

### (US) Region
#### label missing values for CEN_REG: missing (or territory)
tmp <- NISPUF20 %>%
  mutate(CEN_REG = replace(CEN_REG, is.na(CEN_REG), 99))
tmp$CEN_REG <- factor(
  tmp$CEN_REG,
  levels = c(1, 2, 3, 4, 99),
  labels = c(
    "northeast",
    "midwest",
    "south",
    "west",
    "territory or missing"
  )
)

plot_value_counts(
  tmp, 
  CEN_REG, 
  "Census Region Based on True State of Residence", 
  xt_rot = 0
)

### Children under 18 in HH
tmp$CHILDNM <- factor(
  tmp$CHILDNM,
  levels = c(1, 2, 3),
  labels = c(
    "one",
    "two or three",
    "four or more"
  )
)

plot_value_counts(
  tmp, 
  CHILDNM, 
  "Children under 18 in HH", 
  xt_rot = 0
)


### Child received WIC benefits?
tmp$CWIC_01 <- factor(
  tmp$CWIC_01,
  levels = c(1, 2, 3, 77, 99),
  labels = c(
    "yes",
    "no",
    "never heard of wic",
    "don't know",
    "refused"
  )
)

plot_value_counts(
  tmp, 
  CWIC_01, 
  "Child received WIC benefits?", 
  xt_rot = 0
)

### Education of mother categories
tmp$EDUC1 <- factor(
  tmp$EDUC1,
  levels = c(1, 2, 3, 4),
  labels = c(
    "< 12 years",
    "12 years",
    "> 12 years, non-college grad",
    "college grad"
  )
)

plot_value_counts(
  tmp, 
  EDUC1, 
  "Education level of mother", 
  xt_rot = 0
)

### Firstborn?
tmp$FRSTBRN <- factor(
  tmp$FRSTBRN,
  levels = c(1, 2),
  labels = c(
    "no",
    "yes"
  )
)

plot_value_counts(
  tmp, 
  FRSTBRN, 
  "Firstborn status of child", 
  xt_rot = 0
)

### Hispanic?
tmp$I_HISP_K <- factor(
  tmp$I_HISP_K,
  levels = c(1, 2),
  labels = c(
    "hispanic",
    "non-hispanic"
  )
)

plot_value_counts(
  tmp, 
  I_HISP_K, 
  "Hispanic origin of child (imputed)", 
  xt_rot = 0
)

### Income to Poverty Ratio
#### Note 2988 missing rows, left skew
summary(tmp$INCPORAR)
tmp %>%
  ggplot(aes("", INCPORAR)) +
  geom_boxplot()
#### Imputed version
summary(tmp$INCPORAR_I)
tmp %>%
  ggplot(aes("", INCPORAR_I)) +
  geom_boxplot()

### Poverty Status
tmp$INCPOV1 <- factor(
  tmp$INCPOV1,
  levels = c(1, 2, 3, 4),
  labels = c(
    "above poverty, >$75k",
    "above poverty, <=$75k",
    "below poverty",
    "unknown"
  )
)

plot_value_counts(
  tmp, 
  INCPOV1, 
  "Poverty status (based on 2019 Census thresholds)", 
  xt_rot = 0
)

### Income bucket
tmp$INCQ298A <- factor(
  tmp$INCQ298A,
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

plot_value_counts(
  tmp, 
  INCQ298A, 
  "Family Income Bucket", 
  xt_rot = 0
)

### Language of interview (Eng or Esp or Other)
tmp$LANGUAGE <- factor(
  tmp$LANGUAGE,
  levels = c(1, 2, 3),
  labels = c(
    "english",
    "spanish",
    "other"
  )
)

plot_value_counts(
  tmp, 
  LANGUAGE, 
  "Language in which interview was conducted", 
  xt_rot = 0
)

### Age of mother (<30yr or >=30yr)
tmp$M_AGEGRP2 <- factor(
  tmp$M_AGEGRP2,
  levels = c(1, 2),
  labels = c(
    "<=29 years",
    ">=30 years"
  )
)

plot_value_counts(
  tmp, 
  M_AGEGRP2, 
  "Age of mother categories (imputed)", 
  xt_rot = 0
)

### Marital status of mother
tmp$MARITAL2 <- factor(
  tmp$MARITAL2,
  levels = c(1, 2),
  labels = c(
    "married",
    "not married at time of interview"
  )
)

plot_value_counts(
  tmp, 
  MARITAL2, 
  "Marital status of mother (imputed)", 
  xt_rot = 0
)

### Geographic mobility status (interview date vs at birth)
tmp$MOBIL_I <- factor(
  tmp$MOBIL_I,
  levels = c(1, 2),
  labels = c(
    "moved from different state",
    "did not move from different state"
  )
)

plot_value_counts(
  tmp, 
  MOBIL_I, 
  "Geographic mobility status (imputed)", 
  xt_rot = 0
)

### Race of child (Black or White)
tmp$RACE_K <- factor(
  tmp$RACE_K,
  levels = c(1, 2, 3),
  labels = c(
    "white only",
    "black only",
    "other + multiple race"
  )
)

plot_value_counts(
  tmp, 
  RACE_K, 
  "Race of child (imputed)", 
  xt_rot = 0
)

### Race/Ethnicity of child (Hispanic?)
tmp$RACEETHK <- factor(
  tmp$RACEETHK,
  levels = c(1, 2, 3, 4),
  labels = c(
    "hispanic",
    "non-hispanic white only",
    "non-hispanic black only",
    "non-hispanic other + multiple race"
  )
)

plot_value_counts(
  tmp, 
  RACEETHK, 
  "Race/ethnicity of child (imputed)", 
  xt_rot = 0
)

### Home being rented or owned
tmp$RENT_OWN <- factor(
  tmp$RENT_OWN,
  levels = c(1, 2, 3, 77, 99),
  labels = c(
    "owned or being bought",
    "rented",
    "other arrangement",
    "don't know",
    "refused"
  )
)

plot_value_counts(
  tmp, 
  RENT_OWN, 
  "Home owned/being bought, rented, or other arrangement", 
  xt_rot = 0
)

### Sex of child (M or F)
tmp$SEX <- factor(
  tmp$SEX,
  levels = c(1, 2),
  labels = c(
    "male",
    "female"
  )
)

plot_value_counts(
  tmp, 
  SEX, 
  "Sex of child (imputed)", 
  xt_rot = 0
)


## Geographic variables
### State of residence (FIPS code)
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

tmp$state <- factor(
  tmp$STATE,
  levels = fips,
  labels = postal_codes
)
plot_value_counts(
  tmp, 
  state, 
  "State of residence (FIPS code)", 
  xt_rot = 90
)


tmp %>%
  ggplot(aes(forcats::fct_infreq(state))) +
  geom_bar() +
  xlab("State of residence (FIPS code)") +
  ylab("Count") +
  ggtitle("Count of State of residence (FIPS code)") +
  theme(
    axis.text.x = element_text(
      angle = 90
    )
  )

#### 2020 population estimates from USCB: 
#### https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-total.html
states <- read.csv(paste0(DATA_PATH, "state_pop_estimates_2020.csv"))
states %>%
  ggplot(aes(state, pop_2020)) +
  geom_bar(stat="identity") +
  xlab("State") +
  ylab("Population") +
  ggtitle("USCB 2020 state total population estimates") +
  theme(
    axis.text.x = element_text(
      angle = 90
    )
  )

state_responses <- tmp %>% 
  group_by(state) %>%
  count(name = "response_count") %>%
  inner_join(states, by = "state") %>%
  mutate(
    responses_to_pop_ratio = response_count / pop_2020
  ) %>%
  mutate(
    pop_2020_norm = normalize(
      pop_2020,
      state_responses$pop_2020
    ),
    responses_norm = normalize(
      response_count,
      state_responses$response_count
    ),
    rtp_ratio_norm = normalize(
      responses_to_pop_ratio, 
      state_responses$responses_to_pop_ratio
    )
  )

#### Side-by-side
state_responses %>%
  melt(
    id.vars = "state", 
    measure.vars = c("pop_2020_norm", "responses_norm")
  ) %>%
  ggplot(aes(x=state, y=value, fill=variable)) +
    geom_bar(stat="identity", position="dodge") +
    scale_fill_discrete(
      name = "Variable",
      labels = c("Population", "Responses")
    ) +
    xlab("State") +
    ylab("Normalized Value") +
    ggtitle("Normalized Population vs. Normalized Responses by State") +
    theme(
      axis.text.x = element_text(
        angle = 90
      )
    )

#### Responses / Population
state_responses %>%
  ggplot(aes(state, responses_to_pop_ratio)) +
  geom_bar(stat = "identity") +
  xlab("State") +
  ylab("Responses / Population") +
  ggtitle("States Responses to Population Ratio") +
  theme(
    axis.text.x = element_text(
      angle = 90
    )
  )
#### Ordered descending
state_responses %>%
  ggplot(aes(reorder(state, -responses_to_pop_ratio), responses_to_pop_ratio)) +
  geom_bar(stat = "identity") +
  xlab("State") +
  ylab("Responses / Population") +
  ggtitle("States Responses to Population Ratio") +
  theme(
    axis.text.x = element_text(
      angle = 90
    )
  )

### Number of providers identified by respondent
tmp$D6R <- replace(tmp$D6R, is.na(tmp$D6R), 4)

tmp$D6R <- factor(
  tmp$D6R,
  levels = c(0, 1, 2, 3, 4),
  labels = c(
    "0",
    "1",
    "2",
    "3+",
    "missing"
  )
)

plot_value_counts(
  tmp, 
  D6R, 
  "Number of vaccination providers identified by respondent", 
  xt_rot = 0
)

### Number of providers responding with vaccination data for child
tmp$N_PRVR <- factor(
  tmp$N_PRVR,
  levels = c(0, 1, 2, 3),
  labels = c(
    "0",
    "1",
    "2",
    "3+"
  )
)

plot_value_counts(
  tmp, 
  N_PRVR, 
  "Number of vaccination providers responding with vaccination data for child", 
  xt_rot = 0
)

### Provider facility types (public, private, mixed, etc.)
tmp$PROV_FAC <- replace(tmp$PROV_FAC, is.na(tmp$PROV_FAC), 6)

tmp$PROV_FAC <- factor(
  tmp$PROV_FAC,
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c(
    "all public",
    "all hospital",
    "all private",
    "all military/other",
    "mixed",
    "missing"
  )
)

plot_value_counts(
  tmp, 
  PROV_FAC, 
  "Provider facility types (imputed)", 
  xt_rot = 0
)

### Whether all or some or none of providers reported vaccinations to 
### immunization registry
tmp$REGISTRY <- replace(tmp$REGISTRY, is.na(tmp$REGISTRY), 5)

tmp$REGISTRY <- factor(
  tmp$REGISTRY,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "all providers",
    "some providers but probably not all",
    "no providers",
    "unknown",
    "missing"
  )
)

plot_value_counts(
  tmp, 
  REGISTRY, 
  "Provider(s) reported vaccincations to immunization registry?", 
  xt_rot = 0
)

### Whether all or some or none of providers ordered vaccines from state/local
### health dept.
tmp$VFC_ORDER <- replace(tmp$VFC_ORDER, is.na(tmp$VFC_ORDER), 5)

tmp$VFC_ORDER <- factor(
  tmp$VFC_ORDER,
  levels = c(1, 2, 3, 4, 5),
  labels = c(
    "all providers",
    "some providers but probably not all",
    "no providers",
    "unknown",
    "missing"
  )
)

plot_value_counts(
  tmp, 
  VFC_ORDER, 
  "Provider(s) ordered vaccines from state/local health dept?", 
  xt_rot = 0
)
