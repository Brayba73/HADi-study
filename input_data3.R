#install.packages("data.table")
#install.packages("lpSolveAPI")
rm(list=ls(all=TRUE)) #to remove everything from the environment

#read in packages
library(lpSolveAPI)
library(tibble)
#library(openxlsx)
library(readxl)
library(tidyr)
library(dplyr)
library(data.table)



#new function same as left join but replacing NA by 0
left_join0 <- function(x, y, fill = 0L, ...){
  z <- left_join(x, y, ...)
  new_cols <- setdiff(names(z), names(x))
  z <- replace_na(z, setNames(as.list(rep(fill, length(new_cols))), new_cols))
  z
}

#setting working directory
setwd("C:/Users/Barbara/OneDrive - Queen's University Belfast/Project/R Programming/Barbara's project_181222")

source ("optimisation_v2022-05-04.R") #call the function "optimisation" included in "optimisation.R" file

####################################################################################################################
##################### Parameters to set before the optimization ####################################################

#setting working directory
setwd("C:/Users/Barbara/OneDrive - Queen's University Belfast/Project/R Programming/Barbara's project_181222")
#choose a name  for your optimization = folder name 
#modify if needed
folder_name<-"Test Women 75+_020323"
#modify if needed
fobj_name<-"MinSum" # "MinSum" or "MinMax"

#Read model table
# modify if needed
model_table<-read.xlsx("INPUT/Copie de WN75_MinMax_BB181222 RP.xlsx",sheet=1)

#name of the first variable
# modify if needed
first_var<-"10R"
#name of the last variable (depend on the objectif function because MinMax add a variable on the right side of the table)
if (fobj_name=="MinSum"){
  last_var<-"9H_posdev" 
}else if (fobj_name=="MinMax"){
  last_var<-"Dmax"
}   

#if you want to exclude some constraints, please put the name here
# modify if needed
# if no constraint to exclude : constraints_to_exclude<-NULL
# else :  constraints_to_exclude<-c("Hydration (litres)","Proteins_low")
constraints_to_exclude<-NULL

# modify the sheet name if needed (if you change population)
COMPO_pop <- read_excel("INPUT/2022-12-19_conso_compo_per_pop (2).xlsx", 
                        sheet = "NUTRIENTS COMPO PER g")
#<-read.xlsx("INPUT/2022-12-19_conso_compo_per_pop (2).xlsx",sheet ="NUTRIENTS COMPO PER g")
# Filter out the agesex group required
COMPO_pop <- COMPO_pop[COMPO_pop$agesex_group=="W_75+",]



 

# modify if needed : list of nutrients of interest
# the nutrients here will be in the output table called optimized_nutrients.xlsx
# it has to nutrients that are in your composition table (compo_pop)

nutrient_list<-COMPO_pop$NUTRIENT_100G

nutrient_list<-gsub("_sum","",nutrient_list)




######################################################END###################################################################  

if(!is.null(constraints_to_exclude)){
  model_table<-model_table%>%filter(!constraint_name%in%constraints_to_exclude)
}

#Create a folder in OUTPUT with the name of the optimization
if (!file.exists(file.path("OUTPUT",folder_name))) {
  dir.create(file.path("OUTPUT",folder_name))
} 


#coefficients of the constraints : select the columns from first to last variables
constraints<-as.matrix(model_table%>%filter(constraint_name!="fobj")%>%select(!!first_var:!!last_var)) 
#direction of the constraints : select the column "direction"
directions<-model_table%>%filter(constraint_name!="fobj")%>%pull(direction)
#values of the constraint : select the column "value"
values<-model_table%>%filter(constraint_name!="fobj")%>%pull(value)
#coefficients of the objective function : select the row corresponding to "fobj" and columns from first to last variable
objective<-model_table%>%filter(constraint_name=="fobj")%>%select(!!first_var:!!last_var)
#direction of the objective function ("max" or "min") : select the cell corresponding to column "direction" and line "fobj"
objective_direction<-model_table%>%filter(constraint_name=="fobj")%>%pull(direction)

#put all the parameters in a list called "input_model"
input_model<-list(const.mat=constraints,
                  const.dir=directions,
                  const.rhs=values,
                  f.obj=objective,
                  direction=objective_direction) 

#run the optimization and save it into a model
optimized_model<-optimisation(input_model,save_model = TRUE,model_name = folder_name)

###################################################SOLUTIONS ####################################################################
#statuts of the optimization : 
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

#0. first, check the optimization 
statuts<-optimized_model$status

#Objectif function value 
obj_value_table<-data.frame("obj value"=optimized_model$objval)

####### 1.Optimized quantities = main solution
solutions<-optimized_model$solution #optimized quantities

#change solution vector into table with corresponding columns
solutions_table<-as.data.table(t(solutions))
colnames(solutions_table)<-colnames(model_table%>%select(!!first_var:!!last_var))

#transpose the solution table
t_solutions_table<-as.data.frame(t(solutions_table))%>%rownames_to_column(var="code")%>%rename(solution=V1)

#merge with corresponding name instead of code
foodcode_table<-read.xlsx("INPUT/SubFoodGroups_code.xlsx")

#optimized quantities and deviations linked to the food group name
optimized_quantities<-t_solutions_table%>%
  mutate(deviation_prefix=ifelse(endsWith(code,"_posdev"),"posdev_",
                                 ifelse(endsWith(code,"_negdev"),"negdev_","")),
         new_code=gsub("_posdev|_negdev","",code))%>%
  left_join(foodcode_table,by=c("new_code"="code"))%>%mutate(name=paste0(deviation_prefix,name))%>%select(-deviation_prefix,-new_code)%>%rename(optimized=solution)

# quantities observed in the composition table
observed_quantities<-COMPO_pop%>%filter(Nutrient=="QTY")%>%select(-Nutrient)%>%
  pivot_longer(cols="10R":"9H",names_to = "code",values_to = "observed")%>%mutate(observed=as.numeric(observed))%>%
  left_join(foodcode_table,by="code")

#merge the two previous table to have observed qty on first col and optimized quantity on second col
solutions_table<-left_join0(optimized_quantities,observed_quantities,by = c("code", "name"))%>%select(code,name,observed,optimized)

###### 2. value of the constraints
constraints_optimized_table<-model_table%>%select(constraint_name,!!first_var:!!last_var)%>%
  pivot_longer(cols = !!first_var:!!last_var,names_to="code",values_to="coefficient")%>%
  left_join(solutions_table[,c("code","optimized")],by="code")%>%
  mutate(optimized=coefficient*optimized)%>%group_by(constraint_name)%>%summarise(optimized=sum(optimized))%>%
  filter(constraint_name!="fobj")

constraints_observed_table<-model_table%>%select(constraint_name,!!first_var:!!last_var)%>%
  pivot_longer(cols = !!first_var:!!last_var,names_to="code",values_to="coefficient")%>%
  left_join(solutions_table[,c("code","observed")],by="code")%>%
  mutate(observed=coefficient*observed)%>%group_by(constraint_name)%>%summarise(observed=sum(observed))%>%
  filter(constraint_name!="fobj")

solutions_constraints_table<-left_join(constraints_observed_table,constraints_optimized_table,by="constraint_name")


############## 3. key nutrients composition

#select only Nutrients of interest
COMPO_pop_select<-COMPO_pop%>%filter(Nutrient%in%nutrient_list)%>%
  #convert to numeric
  mutate_at(vars("10R":"9H"), as.numeric)

# Remove duplicates Nutrients
COMPO_pop_select<-COMPO_pop_select[!duplicated(COMPO_pop_select$Nutrient), ]

#calculate total contents with observed quantities
nutrients_observed_table<-COMPO_pop_select%>%
  pivot_longer(cols = ("10R":"9H"),names_to="code",values_to="coefficient")%>%
  left_join(solutions_table[,c("code","observed")],by="code")%>%
  mutate(observed=coefficient*observed)%>%group_by(Nutrient)%>%summarise(observed=sum(observed))

#calculate total contents with optimized quantities
nutrients_optimized_table<-COMPO_pop_select%>%
  pivot_longer(cols = ("10R":"9H"),names_to="code",values_to="coefficient")%>%
  left_join(solutions_table[,c("code","optimized")],by="code")%>%
  mutate(optimized=coefficient*optimized)%>%group_by(Nutrient)%>%summarise(optimized=sum(optimized))

nutrients_table<-left_join(nutrients_observed_table,nutrients_optimized_table,by="Nutrient")


#export the solutions 

table_list <- list('objective function' = obj_value_table, 'variables'=solutions_table, 
                   'constraints' = solutions_constraints_table, 'nutrients'=nutrients_table)

write.xlsx(table_list,file=paste0("OUTPUT/",folder_name,"/solutions_",folder_name,".xlsx")) 
print(statuts)


