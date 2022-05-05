# imports
library(dplyr)
library(ggplot2)
library(stringr)
library(rlang)


response_cols <- c(
  "SEQNUMC",
  "PDAT", # 1 if adequate data
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

# Utils (tb moved)
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

# Overall Sample
## Social and Demographic Fields
### Age Groups (AGEGRP)
NISPUF20$AGEGRP <- factor(
  NISPUF20$AGEGRP, 
  levels = c(1, 2, 3), 
  labels = c("19-23 months", "24-29 months", "30-35 months")
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
    "unknown",
    "refused"
  )
)
plot_value_counts(
  NISPUF20, 
  C5R, 
  "Relationship of Respondent to Child", 
  xt_rot = 20
)

# (US) Region
# label missing values for CEN_REG: missing (or territory)
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

# Children under 18 in HH
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


# Child received WIC benefits?
tmp$CWIC_01 <- factor(
  tmp$CWIC_01,
  levels = c(1, 2, 3, 77, 99),
  labels = c(
    "yes",
    "no",
    "never heard of wic",
    "unknown",
    "refused"
  )
)

plot_value_counts(
  tmp, 
  CWIC_01, 
  "Child received WIC benefits?", 
  xt_rot = 0
)

# Education of mother categories
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

# Firstborn?
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

# Hispanic?


# Income to Poverty Ratio


# Poverty Status


# Income bucket


# Language of interview (Eng or Esp or Other)


# Age of mother (<30yr or >=30yr)


# Marital status of mother


# Geographic mobility status (interview date vs at birth)


# Race of child (Black or White)


# Race/Ethnicity of child (Hispanic?)


# Home being rented or owned


# Sex of child (M or F)


# State of residence (kind of)


# Number of providers responding with vaccination data for child


# Provider facility types (public, private, mixed, etc.)


# Whether all or some or none of providers reported vaccinaztions to 
# immunization registry


# Whether all or some or none of providers ordered vaccines from state/local
# health dept.


