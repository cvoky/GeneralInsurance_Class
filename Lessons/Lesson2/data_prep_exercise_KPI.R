####### Data preparation for lesson 2 (KPI's, searching for bad looking portfolios) #####

# libraries
library(dplyr)
library(ggplot2)

# load data
dt_KPI_raw <- read.csv("data/lesson2_KPI.csv")

## what type of information we have? (column quick view)
dt_KPI_raw %>% glimpse

## some data dictionary would be helpful, do we have some?

## what the columns contains?
dt_KPI_raw %>% summary()

#### looking for defects in data ####
## are there any missings?
dt_KPI_raw %>% lapply(function(x) {is.na(x) %>% table}) 

## are there any non sense values? 
# what are not allowed vaules for specific columns? lets look individually
# be prepared to deal with continuous and categorical values as well
dt_KPI_raw$Region %>% table(useNA = "ifany") 

dt_KPI_raw$Losses %>% summary 

#### correction of defects ####
## what is the feasible way to repair missing values? 
dt_KPI_raw <- dt_KPI_raw %>% 
  mutate(Business = ifelse(is.na(Business), "Missing", as.character(Business))) 

## what is the feasible way to repair non sense values?
dt_KPI_raw <- dt_KPI_raw %>% 
  filter(Premium > 0,
          Losses > 0, 
         Expenses > 0
  )

#### Visualization of Raw Data ####
### What makes sense to visualize/compare at visual sense for KPIs?

## Data Preparation for Vizualization
# Which Unit has collected the most Premium? 
dt_KPI_raw %>% 
  group_by(Unit) %>% 
  summarize(Premium = sum(Premium, na.rm = TRUE),
            Expenses = sum(Expenses, na.rm = TRUE),
            Losses = sum(Losses, na.rm = TRUE)
  ) %>% 
  arrange(desc(Premium))

# Which Unit has spent the least Expenses? 
dt_KPI_raw %>% 
  group_by(Unit) %>% 
  summarize(Premium = sum(Premium, na.rm = TRUE),
            Expenses = sum(Expenses, na.rm = TRUE),
            Losses = sum(Losses, na.rm = TRUE)
  ) %>% 
  arrange(Expenses)

# Which Business of previous Unit has spent the most Expenses? 
dt_KPI_raw %>% 
  filter(Unit == "Unit3") %>% 
  group_by(Business) %>% 
  summarize(Premium = sum(Premium, na.rm = TRUE),
            Expenses = sum(Expenses, na.rm = TRUE),
            Losses = sum(Losses, na.rm = TRUE)
  ) %>% 
  arrange(desc(Expenses))

## Basic Viz.
## Vizualize your findings on simple bar charts - ggplot2::geom_col()

# Which Unit has collected the most Premium? 
dt_KPI_raw %>% 
  group_by(Unit) %>% 
  summarize(Premium = sum(Premium, na.rm = TRUE),
            Expenses = sum(Expenses, na.rm = TRUE),
            Losses = sum(Losses, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = reorder(Unit, -Premium), y = Losses)) + 
  geom_col()

# Which Unit has spent the least Expenses? 
dt_KPI_raw %>% 
  group_by(Unit) %>% 
  summarize(Premium = sum(Premium, na.rm = TRUE),
            Expenses = sum(Expenses, na.rm = TRUE),
            Losses = sum(Losses, na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = reorder(Unit, Expenses), y = Losses)) + 
  geom_col()

# Which Business of previous Unit has spent the most Expenses? 
dt_KPI_raw %>% 
  filter(Unit == "Unit3") %>% 
  group_by(Business) %>% 
  summarize(Premium = sum(Premium, na.rm = TRUE),
            Expenses = sum(Expenses, na.rm = TRUE),
            Losses = sum(Losses, na.rm = TRUE)
  )  %>% 
  ggplot(aes(x = reorder(Business, -Expenses), y = Losses)) + 
  geom_col()


# Bonus - Show all Costs Insurance Company has as a stacked bar for 
# Expenses and Losses grouped by Units
dt_KPI_raw %>% 
  group_by(Unit) %>% 
  summarize(Premium = sum(Premium, na.rm = TRUE),
            Expenses = sum(Expenses, na.rm = TRUE),
            Losses = sum(Losses, na.rm = TRUE)
  ) %>% 
  reshape2::melt(measure.vars = c("Expenses", "Losses"), 
                 variable.name = "Costs", value.name = "Costs_Value") %>% 
  ggplot(aes(x = Unit, y = Costs_Value, fill = Costs)) + 
  geom_col()

### Reflection (5 min) - write a short comment to your notes in repository on what do you think is the best and worst Unit performer.
# Feel free to do more data exploration.

#### Building KPIs ####
# Is there a better way to look into Losses/Expenses/Premium information?

dt_KPI_raw %>%
  mutate(LR = Losses / Premium, 
         ER = Expenses / Premium, 
         CoR = (Losses + Expenses) / Premium
  ) %>% 
  summarize(LR = mean(LR, na.rm =TRUE), 
            ER = mean(ER, na.rm = TRUE), 
            CoR = mean(CoR, na.rm = TRUE),
            Premium = sum(Premium, na.rm = TRUE)
  ) %>% 
  mutate(Dimension = "Total") %>% 
  select(Dimension, everything()) %>% 
  mutate_at(vars(Premium), funs(scales::dollar)) %>% 
  mutate_at(vars(LR, ER, CoR), funs(scales::percent))

dt_KPI_raw <- 
  dt_KPI_raw %>%
  mutate(LR = Losses / Premium , 
         ER = Expenses / Premium , 
         CoR = (Losses + Expenses) / Premium
  )

#### Visualization of KPIs ####
### What makes sense to visualize/compare at visual sense for KPIs?

## Try to find answers on next questions using new KPIs terms you learned
# Which Unit looks bad in terms of Losses? 
dt_KPI_raw %>% 
  group_by(Unit) %>% 
  summarize(LR = mean(LR, na.rm =TRUE), 
            ER = mean(ER, na.rm = TRUE), 
            CoR = mean(CoR, na.rm = TRUE),
            Premium = sum(Premium, na.rm = TRUE) 
  )  %>% 
  ggplot(aes(x = reorder(Unit, -LR), y = LR)) + 
  geom_col()

# Which Unit looks pretty good in terms of Expenses? 
dt_KPI_raw %>% 
  group_by(Unit) %>% 
  summarize(LR = mean(LR, na.rm =TRUE), 
            ER = mean(ER, na.rm = TRUE), 
            CoR = mean(CoR, na.rm = TRUE),
            Premium = sum(Premium, na.rm = TRUE) 
  )  %>% 
  ggplot(aes(x = reorder(Unit, ER), y = ER)) + 
  geom_col()

# Which Business of previous Unit looks bad in terms of Expenses? 
dt_KPI_raw %>% 
  filter(Unit == "Unit3") %>% 
  group_by(Business) %>% 
  summarize(LR = mean(LR, na.rm =TRUE), 
            ER = mean(ER, na.rm = TRUE), 
            CoR = mean(CoR, na.rm = TRUE),
            Premium = sum(Premium, na.rm = TRUE) 
  ) %>% 
  ggplot(aes(x = reorder(Business, -ER), y = ER)) + 
  geom_col()

# Bonus - Make a stacked bar for Losses and Expenses using new KPIs term you learn. 
# Does it help you to explore something new?
dt_KPI_raw %>% 
  group_by(Unit) %>% 
  summarize(LR = mean(LR, na.rm =TRUE), 
            ER = mean(ER, na.rm = TRUE), 
            CoR = mean(CoR, na.rm = TRUE),
            Premium = sum(Premium, na.rm = TRUE) 
  ) %>% 
  reshape2::melt(measure.vars = c("ER", "LR"), 
                 variable.name = "Ratio", value.name = "Ratio_Value") %>% 
  ggplot(aes(x = reorder(Unit, -Ratio_Value), y = Ratio_Value, fill = Ratio)) + 
  geom_col()

### Reflection again (5 min) - write a short comment to your notes in repository on what do you think is 
# the best and worst Unit performer based on what have you learned recently.
# Feel free to do more data exploration.

#### UWR ####
# Is there even better way on how to look on portfolios? Previously we spoke about Ratios 
# and relative performances. Can you create something similar but as a absolute performance?
# How could be absolute performance defined?

dt_KPI_raw <- dt_KPI_raw %>% 
  mutate(UWR = Premium - Losses - Expenses)

# Which Unit looks pretty good in terms of UWR? 
dt_KPI_raw %>% 
  group_by(Unit) %>% 
  summarize(UWR = sum(UWR, na.rm = TRUE) 
  )  %>% 
  ggplot(aes(x = reorder(Unit, UWR), y = UWR)) + 
  geom_col()

# Which Business of previous Unit looks bad in terms of UWR? 
dt_KPI_raw %>% 
  filter(Unit == "Unit3") %>% 
  group_by(Business) %>% 
  summarize(UWR = sum(UWR, na.rm = TRUE) 
  ) %>% 
  ggplot(aes(x = reorder(Business, -UWR), y = UWR)) + 
  geom_col()

### Reflection again (5 min) - write a short comment to your notes in repository on what do you think is 
# the best and worst Unit performer based on what have you learned recently.
# Feel free to do more data exploration.

#### shiny ####
# where it makes sense to use dynamic range / create interactivity?

# Go to the root folder of the repository and type shiny::runApp() for running the Class Shiny app.
# Edit files shiny_tab2_content.R (as ui.R for lesson2) and shiny_tab2_server.R (as server.R for lesson2)