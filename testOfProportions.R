# Statistically significantly different from 0.5
# data manipulation libraries

library(abind)
library(data.table)
library(scales)
library(reshape2)

listOfLevels = list()
listOfGroups = list()
listOfPct1 = list()
listOfPct2 = list()
listOfPval = list()

listOfSources = c("diploma15_16","diploma16_17")
gpVars = "Top choice department"
catVars = "Obtained top choice department"

# Assemble table of values from one category variable and male-female data
cat("Two datasets to compare")
tmp = readline(prompt = "?")
if (nchar(tmp) > 0) {
  tmp = as.list(strsplit(tmp, ","))[[1]]
  listOfSources=gsub("'","",tmp) # Remove quotes
  listOfSources=gsub('"','',tmp) # Remove quotes
}
nVar = length(listOfSources)
cat("Categorical variables to compare between data sets")
tmp= readline(prompt = "?")
if (nchar(tmp) > 0) {
  tmp = as.list(strsplit(tmp, ","))[[1]]
  tmp=gsub("'","",tmp) # Remove quotes
  catVars=gsub('"','',tmp) # Remove quotes
}    
cat("Group variable(s)")
tmp = readline(prompt = "?")
if (nchar(tmp) > 0) {
  tmp=gsub("'","",tmp) # Remove quotes
  gpVars=gsub('"','',tmp) # Remove quotes
}    
frame1 = get(listOfSources[1])
frame2 = get(listOfSources[2])

# Create a list of common levels
for (thisVar in catVars) {
  vec1 = frame1[,thisVar]
  select1a = !is.na(vec1)
  vec2 = frame2[,thisVar]
  select2a = !is.na(vec2)
  
  commonLvls = intersect(unique(vec1),unique(vec2))
  commonLvls=commonLvls[!is.na(commonLvls)]
  # Loop through common levels
  for (thisLvl in commonLvls) {
    # For different group variables
    for (thisGpVar in gpVars) {
      #Find unique values in group variable
      gpVar1 = frame1[,thisGpVar]
      select1b = !is.na(gpVar1)
      gpVar2 = frame2[,thisGpVar]
      select2b = !is.na(gpVar2)
      select1 = select1a & select1b
      select2 = select2a & select2b
      gpVar1_ = gpVar1[select1]
      gpVar2_ = gpVar2[select2]
      vec1_=vec1[select1]
      vec2_=vec2[select2]          
      commonGps = intersect(unique(gpVar1_),unique(gpVar2_))
      for (thisGp in commonGps) {
      # For this level and this group, compare different frequencies
        N1 = sum(gpVar1_==thisGp)
        N2 = sum(gpVar2_==thisGp)
        F1 = sum(gpVar1_==thisGp & vec1_ == thisLvl)
        F2 = sum(gpVar2_==thisGp & vec2_ == thisLvl)
        pval = prop.test(c(F1,F2),c(N1,N2))$p.value
        listOfLevels = append(listOfLevels,thisLvl)
        listOfGroups = append(listOfGroups,thisGp)
        listOfPct1 = append(listOfPct1,round(100*F1/N1,1))
        listOfPct2 = append(listOfPct2,round(100*F2/N2,1))
        listOfPval = append(listOfPval,round(pval,4))
      }
    }
  }
} 
df = cbind(listOfLevels,listOfGroups,listOfPct1,listOfPct2,listOfPval)
df = as.data.frame(df)
View(df)