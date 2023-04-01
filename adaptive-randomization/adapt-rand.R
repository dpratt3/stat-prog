set.seed(pi) # reproducibility
data = read.csv("phst-624-adaptive-randomization.csv")
data = data[, c(1:4)] # only the first four columns need consideration
data = apply(data, 2, trimws) # clean white spaces from data entry
data = as.data.frame(data)

# add some random columns for generalization
classes = c("low", "med", "high")
data$triglycerides = sample(classes, 30, replace = T)
data$heartrate = data$triglycerides = sample(classes, 30, replace = T)
data = data[ , c(1, 2, 3, 5, 6, 4)] # reorder columns

f = min(which(data$treatment == "")) - 1 # last randomly assigned row

# Take subset of interest, which grows, row by row
arm = NULL # this will hold the assigned group, control or treatment
control_sub = subset(data[1:f, ], treatment == "C")
treat_sub = subset(data[1:f, ], treatment == "E")
comorb_cols = c(2:(ncol(data) - 1)) # consider only the middle columns

# Consider blood pressure, sex for control, for (f + 1) row
while(f < dim(data)[[1]]){
  control_sub = subset(data[1:f, ], treatment == "C")
  treat_sub = subset(data[1:f, ], treatment == "E")
  next_row = f + 1
  
  c_list = list()
  e_list = list()
  for(n in comorb_cols){
    c_list[[n]] = sum(control_sub[, n] == data[next_row, n]) # was blood pressure
    e_list[[n]] = sum(treat_sub[, n] == data[next_row, n])
  }
  
  c_marg_sum =  sum(unlist(c_list)) # control marginal sum
  e_marg_sum =  sum(unlist(e_list))
  
  # Award next patient to either control or experimental group based on smaller marginal sum
  coin = c("C", "E") # for random assignment if marginal sums are equivalent
  if(c_marg_sum < e_marg_sum){
    arm =  "C"
  } else if(e_marg_sum < c_marg_sum){
    arm = "E"
  } else {
    arm = sample(coin, size = 1)
  }
  
  # assign patient
  data[next_row, ]$treatment = arm
  
  # increment row 
  f = f + 1
  print(f)
} 

# inspect assignments
sum(data$treatment == "C") 
sum(data$treatment == "E")

# Verify that marginal totals are balanced
control_sub = subset(data[1:f, ], treatment == "C")
treat_sub = subset(data[1:f, ], treatment == "E")

# Put together marginal sum data structure
cont_low_bp = sum(control_sub$blood_pressure == "Low")
cont_med_bp = sum(control_sub$blood_pressure == "Normal")
cont_high_bp = sum(control_sub$blood_pressure == "High")

treat_low_bp = sum(treat_sub$blood_pressure == "Low")
treat_med_bp = sum(treat_sub$blood_pressure == "Normal")
treat_high_bp = sum(treat_sub$blood_pressure == "High")

cont_male = sum(control_sub$sex == "Male")
cont_female = sum(control_sub$sex == "Female")
treat_male = sum(treat_sub$sex == "Male")
treat_female = sum(treat_sub$sex == "Female")

cont_marg_tot = cont_low_bp + 
  cont_med_bp + 
  cont_high_bp + 
  cont_male + 
  cont_female

cont_marg_tot 

treat_marg_tot = treat_low_bp + 
  treat_med_bp + 
  treat_high_bp + 
  treat_male + 
  treat_female

treat_marg_tot

# Put final marginal table together
control = c(cont_low_bp, cont_med_bp, cont_high_bp, cont_male, cont_female, cont_marg_tot)
experimental = c(treat_low_bp, treat_med_bp, treat_high_bp, treat_male, treat_female, treat_marg_tot)
marg_table = rbind.data.frame(control, experimental)
rownames(marg_table) = c("control", "exp")
colnames(marg_table) = c("low_bp", "med_bp", "high_bp", "count_male", "count_fem", "marg_total")
print(marg_table)

write.csv(data, "adapt_assignments.csv")

# Count the levels of each factor. This will be tomorrow's task.
levels = list()
for(c in comorb_cols){
  # enter code here counting the factors of each level
}
