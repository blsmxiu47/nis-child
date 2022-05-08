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
  #' Generates a bar plot from data.frame df and categorical column x with 
  #' x categories on the x-axis and value counts for each category as the height
  #' for the bars
  #' 
  #' @param df A data.frame object
  #' @param x A data frame variable
  #' @param xlab Optional. A string specifying x-axis label
  #' @param xt_rot Optional. An integer specifying x-tick rotation
  #' 
  #' @examples
  #' plot_value_counts(NISPUF20, AGEGRP, "Age Group")
  #' 
  #' plot_value_counts(NISPUF20, state, "State of residence", xt_rot = 90)
  ggplot(df, aes({{x}})) +
    geom_bar() +
    ggtitle(str_glue("Count of {xlab}")) +
    xlab(xlab) +
    ylab("Count") +
    theme(
      axis.text.x = element_text(
        angle = xt_rot
      )
    )
}

normalize <- function (x, X, minimum = 0, maximum = 1) {
  #' Returns the value of a given numeric x normalized proportional to the
  #' mapping of the vector X to a range bounded by the values of the arguments
  #' minimum and maximum
  #' 
  #' @param x A numeric value
  #' @param X A numeric vector
  #' @param minimum A numeric value. The lower bound of the normalized range
  #' @param maximum A numeric value. The upper bound of the normalized range
  #' 
  #' @return The normalized value of x
  #' 
  #' @examples
  #' X <- c(0:100)
  #' normalize(50, X)
  #' 
  #' X <- c(1:42)
  #' normalize(40, X)
  return ((x - min(X)) / (max(X) - min(X)) * (maximum - minimum) + minimum)
}

plot_crosstab <- function (
    crosstab, 
    title = "", 
    xlab = "", 
    ylab = "", 
    xt_rot = 0,
    color = "green"
  ) {
  #' Generates a "crosstab plot" to visualize the frequencies or proportions
  #' from a given contingency table-like object of class "table"
  #' 
  #' @param crosstab A table object
  #' @param title Optional. A string specifying the title of the plot
  #' @param xlab Optional. A string specifying the x-axis label
  #' @param ylab Optional. A string specifying the y-axis label
  #' @param xt_rot Optional. An integer specifying x-tick rotation
  #' @param color Optional. A string specifying dot fill color
  #' 
  #' @examples 
  #' t <- table(NISPUF20$CHILDNM, NISPUF20$INCPOV1)
  #' plot_crosstab(t)
  df <- melt(crosstab)
  df$value <- round(df$value, 3)
  ggplot(df, aes(Var1, Var2)) +
    geom_point(aes(size = value), color = color) +
    theme_bw() +
    scale_size_continuous(range=c(10,30)) +
    geom_text(aes(label = value)) +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    theme(
      axis.text.x = element_text(
        angle = xt_rot
      )
    )
}

contingency <- function (
    data, 
    x, 
    y, 
    style = c("freq", "prop", "margin"), 
    sum_over = 1, 
    chi2 = TRUE,
    rescale.p = FALSE,
    simulate.p.value = FALSE,
    show_plot = FALSE,
    xt_rot = 0,
    color = "turquoise"
  ) {
  #' Generates and prints out a contingency table for given categorical variables 
  #' `x` and `y` from the given data frame object. 
  #' Optionally may also run a Chi-squared test of independence, and/or
  #' generate a crosstab plot.
  #' 
  #' @param data A data frame object
  #' @param x A string specifying a single categorical data frame variable from 
  #' `data`. The rows of the resultant contingency table
  #' @param y A string specifying a single categorical data frame variable from 
  #' `data`. The columns of the resultant contingency table
  #' @param style A string specifying the contingency table style to use.
  #' Default: "freq"
  #' @param sum_over The index to generate margin for. 1 for rows, 2 for 
  #' columns. Default: 1 (rows)
  #' @param chi2 Logical. Whether or not to perform and print results for a 
  #' chi-squared test of independence
  #' @param rescale.p Logical. Whether or not to rescale p-values (if necessary) 
  #' to sum to 1. See `chisq.test`
  #' @param simulate.p.value Logical. Whether or not to compute p-values via 
  #' Monte Carlo simulation. See `chisq.test`
  #' 
  #' @examples 
  #' contingency(NISPUF20, "CEN_REG", "RACE_K", "prop", 1)
  #' 
  #' contingency(NISPUF20, "CEN_REG", "RACE_K", "freq", show_plot = T)
  style <- match.arg(style)
  sum_over <- match.arg(as.character(sum_over), choices = 1:2)
  tab <- table(data[[x]], data[[y]])
  
  print(str_glue("Contingency table for x = {x}, y = {y}"))
  if (style == "freq") {
    print("Frequencies:")
  } else if (style == "prop") {
    print("Proportions:")
    tab <- prop.table(tab, sum_over)
  } else if (style == "margin") {
    print("Marginal Frequencies:")
    tab <- margin.table(tab, sum_over)
  }
  print(tab)
  
  if (chi2 == T) {
    print("Chi-squared test:")
    print(
      chisq.test(
        data[[x]], 
        data[[y]], 
        rescale.p = rescale.p, 
        simulate.p.value = simulate.p.value
      )
    )
  }
  
  if (show_plot == T) {
    if (style == "prop") {
      if (sum_over == 1) {
        A <- x
        B <- y
      } else {
        A <- y
        B <- x
      }
      title = str_glue("{style} crosstab: P( {B} | {A} )")
    } else {
      title = str_glue("{style} crosstab: {x} \U0026 {y}")
    }
    plot_crosstab(
      tab,
      title = title,
      xlab = x,
      ylab = y,
      xt_rot = xt_rot,
      color = color
    )
  }
}


# Overall Sample EDA
## Read in data, if necessary
# NISPUF20 <- read.csv(IN_PATH, "IN_FILE")


## Social and Demographic Fields
### Age Groups (AGEGRP)
plot_value_counts(
  NISPUF20, 
  AGEGRP, 
  "Age Group"
)


### Number of people in household (C1R)
plot_value_counts(
  NISPUF20, 
  C1R, 
  "Number in HH"
)


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

contingency(NISPUF20, "CEN_REG", "RACE_K", "freq", show_plot = T)
contingency(NISPUF20, "CEN_REG", "RACE_K", "prop", 1, show_plot = T)
contingency(NISPUF20, "CEN_REG", "RACE_K", "prop", 2, show_plot = T)

contingency(NISPUF20, "CEN_REG", "INCPOV1", "freq", show_plot = T)
contingency(NISPUF20, "CEN_REG", "INCPOV1", "prop", 1, show_plot = T)
contingency(NISPUF20, "CEN_REG", "INCPOV1", "prop", 2, show_plot = T)


### Children under 18 in HH
plot_value_counts(
  NISPUF20, 
  CHILDNM, 
  "Children under 18 in HH", 
  xt_rot = 0
)

contingency(NISPUF20, "CHILDNM", "INCPOV1", "freq", show_plot = T)
contingency(NISPUF20, "CHILDNM", "INCPOV1", "prop", 1, show_plot = T)
contingency(NISPUF20, "CHILDNM", "INCPOV1", "prop", 2, show_plot = T)

contingency(NISPUF20, "CHILDNM", "RENT_OWN", "freq", show_plot = T)
contingency(NISPUF20, "CHILDNM", "RENT_OWN", "prop", 1, show_plot = T)
contingency(NISPUF20, "CHILDNM", "RENT_OWN", "prop", 2, show_plot = T)


### Child received WIC benefits?
plot_value_counts(
  NISPUF20, 
  CWIC_01, 
  "Child received WIC benefits?", 
  xt_rot = 0
)
wic <- NISPUF20 %>%
  filter(CWIC_01 %in% c("yes", "no"))
# dim(wic)
wic$CWIC_01 <- droplevels(wic$CWIC_01)

contingency(wic, "INCPOV1", "CWIC_01", "freq", show_plot = T)
contingency(wic, "INCPOV1", "CWIC_01", "prop", 1, show_plot = T)
contingency(wic, "INCPOV1", "CWIC_01", "prop", 2, show_plot = T)

contingency(wic, "RACE_K", "CWIC_01", "freq", show_plot = T)
contingency(wic, "RACE_K", "CWIC_01", "prop", 1, show_plot = T)
contingency(wic, "RACE_K", "CWIC_01", "prop", 2, show_plot = T)

contingency(wic, "CEN_REG", "CWIC_01", "freq", show_plot = T)
contingency(wic, "CEN_REG", "CWIC_01", "prop", 1, show_plot = T)
contingency(wic, "CEN_REG", "CWIC_01", "prop", 2, show_plot = T)


### Education of mother categories
plot_value_counts(
  NISPUF20, 
  EDUC1, 
  "Education level of mother", 
  xt_rot = 0
)

contingency(NISPUF20, "RACE_K", "EDUC1", "freq", show_plot = T)
contingency(NISPUF20, "RACE_K", "EDUC1", "prop", 1, show_plot = T)
contingency(NISPUF20, "RACE_K", "EDUC1", "prop", 2, show_plot = T)

contingency(
  NISPUF20, "RACEETHK", "EDUC1", "freq", show_plot = T, xt_rot = 20
)
contingency(
  NISPUF20, "RACEETHK", "EDUC1", "prop", 1, show_plot = T, xt_rot = 20
)
contingency(
  NISPUF20, "RACEETHK", "EDUC1", "prop", 2, show_plot = T, xt_rot = 20
)

contingency(NISPUF20, "M_AGEGRP2", "EDUC1", "freq", show_plot = T)
contingency(NISPUF20, "M_AGEGRP2", "EDUC1", "prop", 1, show_plot = T)
contingency(NISPUF20, "M_AGEGRP2", "EDUC1", "prop", 2, show_plot = T)

contingency(NISPUF20, "EDUC1", "MARITAL2", "freq", show_plot = T)
contingency(NISPUF20, "EDUC1", "MARITAL2", "prop", 1, show_plot = T)
contingency(NISPUF20, "EDUC1", "MARITAL2", "prop", 2, show_plot = T)

contingency(wic, "EDUC1", "INCPOV1", "freq", show_plot = T)
contingency(wic, "EDUC1", "INCPOV1", "prop", 1, show_plot = T)
contingency(wic, "EDUC1", "INCPOV1", "prop", 2, show_plot = T)


### Firstborn?
plot_value_counts(
  NISPUF20, 
  FRSTBRN, 
  "Firstborn status of child", 
  xt_rot = 0
)

contingency(NISPUF20, "M_AGEGRP2", "FRSTBRN", "freq", show_plot = T)
contingency(NISPUF20, "M_AGEGRP2", "FRSTBRN", "prop", 1, show_plot = T)
contingency(NISPUF20, "M_AGEGRP2", "FRSTBRN", "prop", 2, show_plot = T)


### Hispanic?
plot_value_counts(
  NISPUF20, 
  I_HISP_K, 
  "Hispanic origin of child (imputed)", 
  xt_rot = 0
)

contingency(NISPUF20, "I_HISP_K", "CEN_REG", "freq", show_plot = T)
contingency(NISPUF20, "I_HISP_K", "CEN_REG", "prop", 1, show_plot = T)
contingency(NISPUF20, "I_HISP_K", "CEN_REG", "prop", 2, show_plot = T)

contingency(NISPUF20, "I_HISP_K", "RACE_K", "freq", show_plot = T)
contingency(NISPUF20, "I_HISP_K", "RACE_K", "prop", 1, show_plot = T)
contingency(NISPUF20, "I_HISP_K", "RACE_K", "prop", 2, show_plot = T)

contingency(NISPUF20, "I_HISP_K", "INCPOV1", "freq", show_plot = T)
contingency(NISPUF20, "I_HISP_K", "INCPOV1", "prop", 1, show_plot = T)
contingency(NISPUF20, "I_HISP_K", "INCPOV1", "prop", 2, show_plot = T)

contingency(NISPUF20, "I_HISP_K", "LANGUAGE", "freq", show_plot = T)
contingency(NISPUF20, "I_HISP_K", "LANGUAGE", "prop", 1, show_plot = T)
contingency(NISPUF20, "I_HISP_K", "LANGUAGE", "prop", 2, show_plot = T)

ro <- NISPUF20 %>%
  filter(RENT_OWN %in% c("rented", "owned or being bought"))
# dim(ro)
ro$RENT_OWN <- droplevels(ro$RENT_OWN)
contingency(ro, "I_HISP_K", "RENT_OWN", "freq", show_plot = T)
contingency(ro, "I_HISP_K", "RENT_OWN", "prop", 1, show_plot = T)
contingency(ro, "I_HISP_K", "RENT_OWN", "prop", 2, show_plot = T)


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

contingency(NISPUF20, "M_AGEGRP2", "MARITAL2", "freq", show_plot = T)
contingency(NISPUF20, "M_AGEGRP2", "MARITAL2", "prop", 1, show_plot = T)
contingency(NISPUF20, "M_AGEGRP2", "MARITAL2", "prop", 2, show_plot = T)


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

