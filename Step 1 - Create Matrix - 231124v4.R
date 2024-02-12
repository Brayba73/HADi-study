##########Amended 02.01.24 VON###############

#############################################
# What this file does:
#
# Creates the input file for analysis
#
# Files required:
# 1. Constraints: a file containing the constraints interested in,
#                                   the direction of constraint,
#                                   the constraint value,
#                                   the formula for any constraints which need to be generated
#                                     NOTE: the formula must use names from the "Nutrient Combo per g" file 
#                                           AND be formatted as '=(FORMULA) i.e., with an apostrophe before the = symbol
#                                   whether the fobj is max/min
#                                   the age/sex group interested in
#
# 2. File which contains two worksheets:
#    A. Nutrient Combo per g: A file containing all the nutrient information
#                          The rows need to contain at least the nutrients mentioned in the constraint file
#                          The columns will be used for the creation of the analysis file, so need to include all interested in
#                          (do not include the TOTAL column)
#    B. Max/Min/Average Consumption: a file containing the maximum, minimum and average consumption for each age/sex group
#                     nutrients are in columns,
#                     age/sex in rows
#
#############################################


## Libraries
library(readxl)
library(stringr)
library(tidyr)

# Disable scientific notation
options(scipen = 999)

#setting working directory
setwd("C://Users//40346833//OneDrive - Queen's University Belfast//Project//R Programming//Optimisation//Women_75")
#setwd("E:/PASS Hub/Consultancy/Barbara Bray - Food/6 January")

## Read in Constraints
Constraints <- read_excel("INPUT/File 1 - Constraints with formula_carb_fat__energy 1840_Alcoholzero.xlsx")
# Extract fobj
fobj <- paste0(Constraints$direction[1])
# Specify age/sex group
my.group <- paste0(Constraints$value[1])
# Remove row 1
Constraints <- Constraints[-1,]
# Sort alphabetical
Constraints <- Constraints[order(Constraints$constraint_name), ]
# Set values as numeric
Constraints$value <- as.numeric(Constraints$value)


## Read in Nutrients
Nutrients <- read_excel("INPUT/File 2 - Combo Pop Updated 231113.xlsx", 
                        sheet = "NUTRIENTS COMPO PER g", col_names = FALSE)
# Set the column names based on the second row
colnames(Nutrients) <- Nutrients[2,]



## Extract Group Names
Nutrients_Names <- Nutrients[2:1,-1]
Nutrients_Names <- t(Nutrients_Names)
# Set the row names based on the first row
colnames(Nutrients_Names) <- Nutrients_Names[1,]
# Remove the first row, as it's now the row names
Nutrients_Names <- as.data.frame(Nutrients_Names[-1,])



## Tidy up Nutrients df
# Remove rows 1&2, as no longer needed
Nutrients <- Nutrients[-c(1, 2), ]
# Extract only relevant age/sex
Nutrients <- subset(Nutrients, agesex_group == my.group)
# Remove first column, as no longer needed
Nutrients <- Nutrients[,-1]
# Convert all columns except the first one to numeric
Nutrients[, -1] <- lapply(Nutrients[, -1], as.numeric)

## Check if need to create any new nutrients:
new_needed <- length(which(Constraints$percentage_energy != "NA"))

##############################
# The below code creates the new nutrients (if needed)

# Step 1: Extract the constraints we need to create
formulas <- Constraints$percentage_energy[which(Constraints$percentage_energy != "NA")]
# Use str_extract_all to find all matches
matching_words <- unlist(str_extract_all(formulas, paste(Nutrients$NUTRIENT_100G, collapse = "|")))
# Remove duplicates
matching_words <- unique(matching_words)
# Replacing the matching words with data.frame names
new_row_numbers <- paste0("Nutrients.temp$",matching_words)
# Replace the original variable names with the corresponding row numbers
for (j in 1:new_needed) {
  for (i in 1:length(new_row_numbers)) {
    formulas[j] <- gsub(matching_words[i], new_row_numbers[i], formulas[j])
  }  
}
rm(i,j,matching_words,new_row_numbers)

# Check formulas have been updated:
formulas

# Step 2: Transpose Nutrients df to perform calculations
Nutrients.temp <- Nutrients %>%
  pivot_longer(cols=c(-NUTRIENT_100G),names_to="Original_Vars")%>%
  pivot_wider(names_from=c(NUTRIENT_100G))

# Step 3:  Evaluate the formulas needed to create the additional constraints (if applicable)
for (i in 1:new_needed) {
  # Create the column name for the data frame
  my.var_name <- Constraints$constraint_name[which(Constraints$percentage_energy != "NA")][i]
  # Check name doesn't already exist:
  if (my.var_name %in% colnames(Nutrients.temp)) {
  stop("Name for new contrstaint already exists - you need to rename it in the Constraints table")  
  }
  # Calculate the values
  Nutrients.temp$NEWVAR <- eval(parse(text=formulas[i]))  
  # Rename the new column with the value extracted from my.var_name
  colnames(Nutrients.temp)[ncol(Nutrients.temp)] <- as.character(my.var_name)
}
rm(my.var_name, formulas,i)

# Step 4: Transpose Nutrients.temp df back, replacing the original
Nutrients <- Nutrients.temp %>%
  pivot_longer(cols=c(-Original_Vars),names_to="NUTRIENT_100G")%>%
  pivot_wider(names_from=c(Original_Vars))
# Remove temp nutrients file
rm(Nutrients.temp)
rm(new_needed)

# Tidy up Constraints file (i.e., remove percentage energy)
Constraints <- Constraints[1:(length(Constraints)-1)]

##############################
####


## Calculate number of nutrients
n_nutrients <- ncol(Nutrients)-1


## Select out the correct constraints
my.constraints <- merge(Nutrients, Constraints, by.x = "NUTRIENT_100G", by.y = "constraint_name")
# Rename column 1
names(my.constraints)[1] <- "constraint_name"
rm(Nutrients)

## Read in Consumption
Consumption <- read_excel("INPUT/File 2 - Combo Pop Updated 231113.xlsx", 
                          sheet = "CONSUMPTION", col_names = FALSE)
# Set the row names based on the second row
colnames(Consumption) <- Consumption[2,]
# Remove rows 1&2, as no longer needed
Consumption <- Consumption[-c(1:2),]


# Tidy up
rm(Consumption)


### QUERY: DO WE NEED MINUMUMS??

## Create the filler matrices
matrix_0 <- matrix(0, nrow = n_nutrients, ncol = n_nutrients)
matrix_p1  <- diag(1, nrow = n_nutrients, ncol = n_nutrients)


#### BUILD THE FILE #################

## First section (First column and last two columns of input file)
#####The temp folder is the pale green section###################

temp <- as.data.frame(t(Avg_Consumption))
temp$constraint_name <- rownames(temp)
# Rename column 1 to value
names(temp)[1] <- "value"
# Add in direction column
temp$direction <- "="
# Reorder columns
temp <- temp[,c(2,3,1)]

###THIS IS THE PALE ORANGE SECTION######
Constraints_1 <- rbind(Constraints,temp)

# replace direction symbol (using the temp df again, as it's the size needed)
temp$direction <- "<="
# create column c, which contains the names
temp$c <- paste0(temp$constraint_name,"MAX")
# create column max, which contains the max values
temp$max <- t(Max_Consumption)
rm(Max_Consumption)
# reorder the columns, keeping only needed ones
temp <- temp[,c(4,2,5)]
# rename columns to match those in Constraints_1 file
names(temp)[1] <- "constraint_name"
names(temp)[3] <- "value"

Constraints_2 <- rbind(Constraints_1,temp)
rm(Constraints_1)

# replace temp df with next temp
## HAVE ASSUMED ZEROS FOR MIN, IF NEED CALCULATED VALUES OF MIN, THEN NEED TO MIMIC WHAT WAS DONE FOR THE MAX/AVG VALUES
temp <- data.frame(
  constraint_name = paste0("Dmax", 1:(nrow(temp) * 2)),
  direction = rep(">=", nrow(temp) * 2),
  value = rep(0, nrow(temp) * 2)
)

Constraints_3 <- rbind(Constraints_2,temp)
rm(Constraints_2,temp)


# Create last row
temp <- data.frame(
  constraint_name = "fobj",
  direction = fobj,
  value = NA
)

# Add last row to contraints df
Constraints_4 <- rbind(Constraints_3,temp)
rm(Constraints_3,temp)


## Second section

my.constraints <- subset(my.constraints, select = -c(constraint_name, direction, value))

# Convert filler matrices to data frames
matrix_p1 <- as.data.frame(matrix_p1)
matrix_0 <- as.data.frame(matrix_0)

# Set column names of the matrices to match my.constraints
colnames(matrix_p1) <- colnames(my.constraints)
colnames(matrix_0) <- colnames(my.constraints)

# Append the filler data frames to the original data frame (2 x diagnol and 2 x zeros)
my.constraints <-  rbind(my.constraints,matrix_p1,matrix_p1,matrix_0,matrix_0)
rm(matrix_p1,matrix_0)

# Create a data frame with all-zero values matching the structure of my.constraints
zero_row <- as.data.frame(matrix(0, ncol = ncol(my.constraints)))
# Set column names of the matrices to match my.constraints
colnames(zero_row) <- colnames(my.constraints)
# Append the zero_row to my.constraints
my.constraints <- rbind(my.constraints, zero_row)
rm(zero_row)




## Third section

# Create filler matrices
matrix_00  <- as.data.frame(matrix(0, nrow =nrow(Constraints), ncol = n_nutrients*2))
matrix_000 <- as.data.frame(matrix(0, nrow =n_nutrients,       ncol = n_nutrients*2))
matrix_0n1 <- as.data.frame(diag(-1,  nrow = n_nutrients*2,     ncol = n_nutrients*2))

# Create positive avg number matrix
Avg <- Avg_Consumption[1,]
rm(Avg_Consumption)
Avg <- as.numeric(unlist(Avg))
matrix_p1 <- diag(Avg, nrow = n_nutrients, ncol = n_nutrients)
matrix_p1 <- as.data.frame(matrix_p1)
colnames(matrix_p1) <- paste0(colnames(my.constraints),"_posdev")

# Create negative avg number matrix
matrix_n1  <- diag(-Avg, nrow = n_nutrients, ncol = n_nutrients)
matrix_n1 <- as.data.frame(matrix_n1)
colnames(matrix_n1) <- paste0(colnames(my.constraints),"_negdev")
rm(Avg)

# Combine the pos and neg matrices
matrixpn <- cbind(matrix_p1,matrix_n1)
rm(matrix_p1,matrix_n1)
# Note: is posdev and negdev need to be in a different order, then here is where to fix
matrixpn <- matrixpn[, order(names(matrixpn))]

# Copy names to the filler matrices
colnames(matrix_00) <- colnames(matrixpn)
colnames(matrix_000) <- colnames(matrixpn)
colnames(matrix_0n1) <- colnames(matrixpn)

# Append the data frames to the original data frame
my.posneg <-  rbind(matrix_00,matrixpn,matrix_000,matrix_0n1)
rm(matrix_00,matrix_000,matrix_0n1)

# Create a data frame with all-zero values matching the structure of my.posneg
zero_row <- as.data.frame(matrix(0, ncol = n_nutrients*2))
colnames(zero_row) <- colnames(matrixpn)
# Add zero row to bottom
my.posneg <- rbind(my.posneg, zero_row)
rm(zero_row,matrixpn)


## DMax Column
Dmax0 <- rep(0,nrow(Constraints)+n_nutrients*2)
Dmax1 <- rep(1,1+n_nutrients*2)
Dmax <- c(Dmax0,Dmax1)
rm(Dmax0,Dmax1)
# Convert to df
Dmax <- as.data.frame(Dmax)

## Add all Sections together

my.file <- cbind(Constraints_4[,1], my.constraints,my.posneg,Dmax,Constraints_4[,2:3])

rm(my.constraints,Constraints,Constraints_4)
rm(fobj)
rm(my.group,n_nutrients)
rm(Dmax,my.posneg)

###Create a dataframe for 10R to 9H that takes row to 156 and columns 1 to 460 and assign to a new dataframe########
a<- my.file[5:156,1:460]
###Create a dataframe for 10RMax to 9HMax that takes row to 156 and columns 1 to 460 and assign to a new dataframe########
b<- my.file[157:308,1:460]

###Replace section 1 with section 2 of the matrix#######
my.file[5:156,1:460]<-b
my.file[157:308,1:460]<-a

## SAVE FILE AS CSV
write.csv(my.file, "File 4 - Updated Matrix.csv", row.names=FALSE)

