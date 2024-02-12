# install.packages("data.table")
# install.packages("lpSolveAPI")
# install and load readxl package
#install.packages("readxl")
library(readxl)
#install.packages("openxlsx")

library(openxlsx)
library(lpSolveAPI)
library(tibble)
library(tidyr)
library(dplyr)
library(data.table)
# For reading in csv
library(readr)

###########Amends 06.12.23###############################################################################
#setting working directory
#setwd("E:/PASS Hub/Consultancy/Barbara Bray - Food/6 January")
setwd("C:/Users/40346833/OneDrive - Queen's University Belfast/Project/R Programming/Optimisation/Women_75")
source("Main/optimisation_v2022-05-04.R") # call the function "optimisation" included in "optimisation.R" file



# Read in Matrix file
model_table <- read_csv("Input/File 4 - Updated Matrixi.csv")

# if you want to exclude some constraints, please put the name here
# modify if needed
# if no constraint to exclude:
constraints_to_exclude<-NULL
# else :
#constraints_to_exclude <- c("Hydration (litres)",
#                            "Fat",
#                            "Sat fat",
#                            "Free_sugars",
#                            "Red processed meat",
#                            "Total fruit and veg")

if (!is.null(constraints_to_exclude)) {
  model_table <- model_table %>% filter(!constraint_name %in% constraints_to_exclude)
}

# name of the first variable
# modify if needed
first_var <- "10R"
# name of the last variable
# modify if needed
last_var <- "Dmax"


# coefficients of the constraints : select the columns from first to last variables,
#                                     but NOT the entries for the last row (fobj)
constraints <- as.matrix(model_table %>%
                           filter(constraint_name != "fobj") %>%
                           select(!!first_var:!!last_var))

# coefficients of the objective function : select the columns from first to last variables,
#                                           but only the entries for the last row (fobj)
objective <- model_table %>%
  filter(constraint_name == "fobj") %>%
  select(!!first_var:!!last_var)


# direction of the constraints : select the column "direction", but not the entry for the last row (fobj)
directions <- model_table %>%
  filter(constraint_name != "fobj") %>%
  pull(direction)
# direction of the objective function ("max" or "min") : select the cell corresponding to column "direction" and line "fobj"
objective_direction <- model_table %>%
  filter(constraint_name == "fobj") %>%
  pull(direction)

# values of the constraint : select the column "value", but not the entry for the last row (fobj)
values <- model_table %>%
  filter(constraint_name != "fobj") %>%
  pull(value)


# put all the parameters in a list called "input_model"
input_model <- list(
  const.mat = constraints,
  const.dir = directions,
  const.rhs = values,
  f.obj = objective,
  direction = objective_direction
)

# run the optimization and save it into a model
optimized_model <- optimisation(input_model)

# status of the optimization :
# 0: "optimal solution found"
# 1: "the model is sub-optimal"
# 2: "the model is infeasible"
# 3: "the model is unbounded"
# 4: "the model is degenerate"
# 5: "numerical failure encountered"
# 6: "process aborted"
# 7: "timeout"
# 9: "the model was solved by presolve"
# 10: "the branch and bound routine failed"
# 11: "the branch and bound was stopped because of a break-at-first or break-at-value"
# 12: "a feasible branch and bound solution was found"
# 13: "no feasible branch and bound solution was found"
status <- optimized_model$status
status

# solution : vector of optimized variables
solutions <- optimized_model$solution # optimized quantities

# change solution vector into table with corresponding columns
solutions_table <- as.data.table(t(solutions))
colnames(solutions_table) <- colnames(model_table %>% select(!!first_var:!!last_var))

# export the solution
write.xlsx(solutions_table, file = "OUTPUT/solutions_table.xlsx", rowNames = FALSE)

t_solutions_table <- as.data.frame(t(solutions_table)) %>%
  rownames_to_column(var = "variable") %>%
  rename(solution = V1)

# export the value of the constraint
constraint_optimized_values <- model_table %>%
  select(constraint_name, !!first_var:!!last_var) %>%
  pivot_longer(cols = !!first_var:!!last_var, names_to = "variable", values_to = "coefficient") %>%
  left_join(t_solutions_table, by = "variable") %>%   
  mutate(optimized_value = coefficient * solution) %>%
  group_by(constraint_name) %>%
  summarise(optimized_value = sum(optimized_value))

model_table_optimized <- left_join(model_table, constraint_optimized_values, by = "constraint_name")

write.xlsx(model_table_optimized, file = "OUTPUT/optimized_constraintsi.xlsx", rowNames = FALSE)
