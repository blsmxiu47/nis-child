# Imports
library(dplyr)
library(forcats)
library(ggplot2)
library(reshape2)
library(rlang)
library(stringr)


# CONSTANTS
IN_PATH <- "FILEPATH"

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
## Read in data, if necessary
# NISPUF20 <- read.csv(IN_PATH, "IN_FILE")

## Social and Demographic Fields
### Age Groups (AGEGRP)
plot_value_counts(NISPUF20, AGEGRP, "Age Group")

### Number of people in household (C1R)
plot_value_counts(NISPUF20, C1R, "Number in HH")

### Relationship of Respondent to Child (C5R)
plot_value_counts(
  NISPUF20, 
  C5R, 
  "Relationship of Respondent to Child", 
  xt_rot = 20
)

### (US) Region
plot_value_counts(
  NISPUF20, 
  CEN_REG, 
  "Census Region Based on True State of Residence", 
  xt_rot = 0
)

### Children under 18 in HH
plot_value_counts(
  NISPUF20, 
  CHILDNM, 
  "Children under 18 in HH", 
  xt_rot = 0
)

### Child received WIC benefits?
plot_value_counts(
  NISPUF20, 
  CWIC_01, 
  "Child received WIC benefits?", 
  xt_rot = 0
)

### Education of mother categories
plot_value_counts(
  NISPUF20, 
  EDUC1, 
  "Education level of mother", 
  xt_rot = 0
)

### Firstborn?
plot_value_counts(
  NISPUF20, 
  FRSTBRN, 
  "Firstborn status of child", 
  xt_rot = 0
)

### Hispanic?
plot_value_counts(
  NISPUF20, 
  I_HISP_K, 
  "Hispanic origin of child (imputed)", 
  xt_rot = 0
)

### Income to Poverty Ratio
#### Note 2988 missing rows, left skew
summary(NISPUF20$INCPORAR)
NISPUF20 %>%
  ggplot(aes("", INCPORAR)) +
  geom_boxplot()
#### Imputed version
summary(NISPUF20$INCPORAR_I)
NISPUF20 %>%
  ggplot(aes("", INCPORAR_I)) +
  geom_boxplot()

### Poverty Status
plot_value_counts(
  NISPUF20, 
  INCPOV1, 
  "Poverty status (based on 2019 Census thresholds)", 
  xt_rot = 0
)

### Income bucket
plot_value_counts(
  NISPUF20, 
  INCQ298A, 
  "Family Income Bucket", 
  xt_rot = 45
)

### Language of interview (Eng or Esp or Other)
plot_value_counts(
  NISPUF20, 
  LANGUAGE, 
  "Language in which interview was conducted", 
  xt_rot = 0
)

### Age of mother (<30yr or >=30yr)
plot_value_counts(
  NISPUF20, 
  M_AGEGRP2, 
  "Age of mother categories (imputed)", 
  xt_rot = 0
)

### Marital status of mother
plot_value_counts(
  NISPUF20, 
  MARITAL2, 
  "Marital status of mother (imputed)", 
  xt_rot = 0
)

### Geographic mobility status (interview date vs at birth)
plot_value_counts(
  NISPUF20, 
  MOBIL_I, 
  "Geographic mobility status (imputed)", 
  xt_rot = 0
)

### Race of child (Black or White)
plot_value_counts(
  NISPUF20, 
  RACE_K, 
  "Race of child (imputed)", 
  xt_rot = 0
)

### Race/Ethnicity of child (Hispanic?)
plot_value_counts(
  NISPUF20, 
  RACEETHK, 
  "Race/ethnicity of child (imputed)", 
  xt_rot = 0
)

### Home being rented or owned
plot_value_counts(
  NISPUF20, 
  RENT_OWN, 
  "Home owned/being bought, rented, or other arrangement", 
  xt_rot = 0
)

### Sex of child (M or F)
plot_value_counts(
  NISPUF20, 
  SEX, 
  "Sex of child (imputed)", 
  xt_rot = 0
)


## Geographic variables
### State of residence (FIPS code)
plot_value_counts(
  NISPUF20, 
  state, 
  "State of residence (FIPS code)", 
  xt_rot = 90
)
NISPUF20 %>%
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
state_responses <- NISPUF20 %>% 
  group_by(state) %>%
  count(name = "response_count") %>%
  inner_join(states, by = "state")
state_responses <- state_responses %>%
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
plot_value_counts(
  NISPUF20, 
  D6R, 
  "Number of vaccination providers identified by respondent", 
  xt_rot = 0
)

### Number of providers responding with vaccination data for child
plot_value_counts(
  NISPUF20, 
  N_PRVR, 
  "Number of vaccination providers responding with vaccination data for child", 
  xt_rot = 0
)

### Provider facility types (public, private, mixed, etc.)
plot_value_counts(
  NISPUF20, 
  PROV_FAC, 
  "Provider facility types (imputed)", 
  xt_rot = 0
)

### Whether all or some or none of providers reported vaccinations to 
### immunization registry
plot_value_counts(
  NISPUF20, 
  REGISTRY, 
  "Provider(s) reported vaccincations to immunization registry?", 
  xt_rot = 0
)

### Whether all or some or none of providers ordered vaccines from state/local
### health dept.
plot_value_counts(
  NISPUF20, 
  VFC_ORDER, 
  "Provider(s) ordered vaccines from state/local health dept?", 
  xt_rot = 0
)

