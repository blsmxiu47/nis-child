# imports
library(dplyr)
library(ggplot2)
library(stringr)
library(rlang)


# Utils (tb moved)
plot_value_count <- function (df, x, xlab, xt_rot = 0) {
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
  labels = c('19-23 months', '24-29 months', '30-35 months')
)
plot_value_count(NISPUF20, AGEGRP, "Age Group")

### Number of people in household (C1R)
plot_value_count(NISPUF20, C1R, "Number in HH")

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
plot_value_count(
  NISPUF20, 
  C5R, 
  "Relationship of Respondent to Child", 
  xt_rot = 20
)
