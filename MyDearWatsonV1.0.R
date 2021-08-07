### Variable initialization
library(varhandle) # To check if input is numeric

### Defaults
# Input data
if (!exists("frameName")) {frameName="data16_17"}
if (!exists("outputDirectory")) {outputDirectory = frameName}
if (!exists("listofVars")) {
  listOfVars = c("Home state","Obtained 1st 2nd or 3rd choice","y",
               "SEX","Department","RankPct","Attended in home state")
  listOfVars = c("Obtained 1st 2nd or 3rd choice",
               "SEX","Department","RankPct",
               "Top choice department","Obtained top choice department","University")
}
nVar=length(listOfVars)
# If the frequency of any level is less than minfDefault, then it is not included in the data table.
if (!exists("minfDefault")){
  minfDefault = 20
}  

# Plot parameter defaults
# This specifies the type of bar ordering for the automatically-generated graph
if (!exists("defaultLevelOrderFixed")){
  defaultLevelOrderFixed = TRUE
}
# If the bar ordering is not fixed, this further specifies the ordering method of levels within each bar. 
# If decreasing, then the most frequent level is put at the top of the bars, and the least frequent at the bottom
if (!exists("defaultLevelOrder")){
  defaultLevelOrder = "Decreasing"
}

# Set default order of categories in bar plots (increasing frequency order=1, decreasing=-1)
if (!exists("levelOrder")){
  if (substr(defaultLevelOrder,1,1)=="d" | substr(defaultLevelOrder,1,1)=="D"  ){
    levelOrder = rep(-1,nVar) 
  }else {
    levelOrder = rep(1,nVar)
  }
}

if (!exists("defaultBarOrder")){
  defaultBarOrder = "none"
}
if (!exists("defaultPctPlot")){
  defaultPctPlot = "percent"
}
if (!exists("defaultColorPlot")){
  defaultColorPlot = "color"
}
# Set plot parameters to defaults.
pctPlot = defaultPctPlot
colorPlot = defaultColorPlot

# LEVEL1 tells the software whether or not to automatically generate all 1-variable plots. 
if (!exists("LEVEL1")){
  LEVEL1 = FALSE
}
# LEVEL2 tells the software whether or not to automatically generate all 1-variable plots. 
if (!exists("LEVEL2")){
  LEVEL2 = FALSE
}


## These values are not optional at the start of the program. These values specify how data is
# handled and displayed for user-generated plots and tables. The user may tell the program to change
# these options when he is creating individual plots and tables.
MAKE_TABLE = TRUE # recreate multiway frequency table from the data frame. The program will only 
# use the existing table if the user gives the instruction
plotToFile = FALSE # By default-user-generated plots are not written to a file 
plotToWindow = TRUE # By default-user-generated plots are displayed in the plot window
viewTable = FALSE # By default, when the user creates a 2- or 3-variable table it is not shown in the variable explorer
saveTable = FALSE # By default, then the user creates a 2- or 3-variable table it is not saved to a file.
computeChiSquare = FALSE # When the user creates a 2- or 3-variable table, he may also choose to display the 
# Chi square statistic relating the variables. By default, the Chi Square is not computed. The user may change
# this when he generates the table.

## Display parameters for graphs.
if (!exists("labelVec")){
  labelVec= 10*c(0:10) # labels on the graph y-axis in percentage plots. (increments of 10 percent
  # are shown in the y-axis)
}
if (!exists("relTextSize")){
  relTextSize = 1.5 # relative size of text in graphs (to improve legibility)
}
if (!exists("relTextSizeIncr")){
  relTextSizeIncr = 0.1 # When the user selects the option to increase or decrease the text size by 
  # selecting + or -, this gives
  # the proportionate increase/decrease in text size
}

if (!exists("viridis_option")){
  viridis_option = "plasma"   # Color option
} 



## Keyboard input. The user may bypass keyboard input by pressing Enter.
cat("Frame name (or press Enter to accept all defaults)\n")
tmp = readline(prompt = "?")
tmp=gsub("'","",tmp) # Remove single quotes, in case the user has put the frame name in quotes.
tmp=gsub('"','',tmp) # Remove double quotes

# If the user presses 'Enter' without giving a frame name, then assign default values to display variables
if (nchar(tmp)==0){

  levelOrderFixed = rep(defaultLevelOrderFixed,nVar) # Level ordering for 1-d plot or within bars in 2-d plots
  barOrder = rep(defaultBarOrder,nVar) # Order of bars in 2-variable plots
  pctPlot = defaultPctPlot # Options are: percentage, frequency, spineplot, doubledecker
  
  # Minimum category data frequency 
  minf=as.list(rep(minfDefault,nVar)) # Creates same min cutoff for all levels.  all levels with frequency less than
  # this value are removed from the data table.
    
# If frame name is input, enable user to specify options  
} else{
  frameName = tmp
  outputDirectory=frameName # By default, the output directory for graphs and tables is set equal to the frame name.

  inputFlag = TRUE
  while (inputFlag){
    inputFlag = FALSE
    #User options
    cat("Please select the options you want to change. The options are: \n")
    cat("0=help;  1= Choose new variables; 2= Set order of variable levels;  3= Set minimum frequencies for variable levels; \n") 
    cat("4= Set plot options; 5= Change output directory; 6= Do not recompute data table \n")
    cat("Please input your selections, separated by commas.\n")
    cat("(To accept all defaults and continue, press Enter) \n")
    
    # Read and process input
    tmp = readline(prompt = "??  ")
    if (nchar(tmp) > 0) {
      tmp = as.list(strsplit(tmp, ","))[[1]]
      # the following 3 lines are to correct user input, in case he has added quotes or spaces
      tmp=gsub("'","",tmp) # Remove quotes
      tmp=gsub('"','',tmp) # Remove quotes
      listOfCmds = trimws(tmp)
      
      # If the user selects 0, then print explanations of the options
      if ("0" %in% listOfCmds){
        inputFlag = TRUE # This will give the user another chance to input the options he wants
        # Following are the explanations that are printed:
        cat("1: Choose variables--the default is to use all variables in the input frame \n")
        cat("2: Order levels in variables--for each variable, level ordering can be increasing or decreasing freq. or fixed \n")
        cat("3: Set minimum level frequencies (levels with fewer instances are excluded)--default is ",minfDefault," \n")
        cat("4: Plot options for bar plots include percentage/nominal, color/grayscale, and bar ordering \n")
        cat("5: Change location where plots are put--default is ",outputDirectory," \n")
        cat("6: Keep the existing data table, and do not recompute (your variables will also be displayed) \n")
      } else { # Only continue if user has not requested explanation. 
        
        ## If the user does not want to use all variables in the input frame, then he can specify here 
        ## the variables that he wants to include in his data table.
        if ("1" %in% listOfCmds) {
          ## Generate list of variables
          varNames = colnames(get(frameName))
          nVar = length(varNames)
          
          # List all variables, and give numeric equivalents to make it easier for the user to
          # specify which variables he wants to retain or remove.
          cat("Current variables are:","\n")
          promptString = ""
          for (ii in seq(nVar)) {
            promptString = paste(promptString,ii,"=",varNames[ii],"; ") # Give number equivalent for vars.
          }
          cat(promptString,"\n")
          
          # Take user input
          cat("Variables to retain/remove (by name or number), + to retain, - to remove","\n")
          #Input 
          tmp = readline(prompt = "??  ")
          # Splits user input into a list of names or numbers.
          tmp = as.list(strsplit(tmp, ","))[[1]]
          # Corrects user input by removing quotes and whitespaces (if present)
          tmp = gsub('"','',tmp)
          tmp = gsub("'","",tmp)
          listOfVars = trimws(tmp) # trim whitespace
          
          # Determines whether to  remove given variables or not and creates a list of variables accordingly
          
          # remove is a logical variable that tells whether or not the user-specified variables 
          # are to be retained or removed.
          remove =  ("-" %in% listOfVars) 
          
          listOfVars = listOfVars[!(listOfVars=="-" | listOfVars =="+")] # Keeps only variable names in list
  
          # Logical vector for variable selection. If the user has selected to remove variables, 
          # then values are all TRUE. Otherwise, values are all FALSE.
          selectVarVec = rep(remove,nVar)     
          
          # If all inputs are numeric, this means the user has specified the variables as a list of 
          # numbers, and these numbers can be used as indices for the columns to be retained or removed
          if (all(check.numeric(listOfVars))){            
            selectVarVec[as.integer(listOfVars)] = !remove  # Change selection vector value for user-selected variables.
          
          # If input is not numeric, then the code must identify the variable columns associated with
          # the variable names given by the user
          } else{         
            for (ii in seq(nVar)) {             # Loop through all variables 
              
              # If column ii is in the user's list of variables, then change the selection value
              if (varNames[ii] %in% listOfVars) {
                selectVarVec[ii] = !remove
              }
            }  
          }
          
          # Use the logical vector selectVarVec to select variables to be included in the data table.
          listOfVars = varNames[selectVarVec]
          # Give the correct number of variables in the table.
          nVar=length(listOfVars)
          
          # If the user keeps all variables in the table, remind the user what the variables are.
        } else{
          cat("Note that current variables are: \n")
          cat(listOfVars, "\n")
        }
        
        # Set defaults for variables selected by the user.
        levelOrderFixed = rep(defaultLevelOrderFixed,nVar) # Level ordering for 1-d or within bar
        barOrder = rep(defaultBarOrder,nVar) # Order of bars in 2-variable plots
        
        # Specify decreasing or increasing level order within bars
        # (Note the program just reads the first letter of user input, for convenience)
        if (substr(defaultLevelOrder,1,1)=="d" | substr(defaultLevelOrder,1,1)=="D"  ){
          levelOrder = rep(-1,nVar) 
        }else {
          levelOrder = rep(1,nVar)
        }
        
        
        ## Here the user can specify the orderings of variable levels for 1-variable plots, or
        # For levels within bars in a 2 level plot
        if ("2" %in% listOfCmds){
          cat("\n Set ordering of variable levels. Current variables are:","\n\n")
          promptString = ""
          for (ii in seq(nVar)) {
            promptString = paste(promptString,ii,"=",listOfVars[ii],"; ")
          }
          cat(promptString,"\n\n")
          cat("Valid level orderings are: I=increasing, D= decreasing, F= fixed  \n\n")
          # Take user input
          cat("To set all level orderings the same, give a single input","\n")
          cat("otherwise give level orderings for different variables as a list separated by commas","\n")
          reinputFlag = TRUE
          while (reinputFlag){
            reinputFlag = FALSE
            tmp = readline(prompt = "??  ")
            # Splits user input into a list of names and removes quotes (if present)
            tmp = as.list(strsplit(tmp, ","))[[1]]
            
            # Corrects user input by removing outside spaces and quotes
            tmp = gsub('"','',tmp)
            tmp = gsub("'","",tmp)
            tmp = trimws(tmp) # trim whitespace
            tmp = substr(tmp,1,1)
            
            # If the user only gives 1 input, then set all variables with the same level ordering
            if (length(tmp)==1){
              
              # If the user gives F as single input, set all variables as fixed
              if (tmp=="F" | tmp == "f"){
                defaultLevelOrderFixed = TRUE
                
              # Otherwise, if single input I or D none of the variables are fixed  
              } else {
                defaultLevelOrderFixed = FALSE
              }
              
              # If single input is I, set all variables increasing and change the default
              if (tmp=="I" | tmp == "i"){
                defaultLevelOrder = "Increasing"
                # Set default order of categories in bar plots (increasing frequency order=1, decreasing=-1)
                levelOrder = rep(1,nVar) 
                
              # Otherwise, if single input set all variables decreasing and change the default  
              }else {
                defaultLevelOrder = "Decreasing"
                # Set default order of categories in bar plots (increasing frequency order=1, decreasing=-1)
                levelOrder = rep(-1,nVar) 
              }
              
              # If only one input set the fixed level order option the same for all variables
              levelOrderFixed = rep(defaultLevelOrderFixed,nVar) # Level ordering for 1-d or within bar

            # If more than one input make sure that the number of inputs is equal to the number of vars.
            # If not, ask the user to input again
            } else if (!(length(tmp)==nVar)){ 
                reinputFlag = TRUE
                cat("Wrong number of inputs, please try again \n")
            
            # The user has given the right number of inputs, so the program can set individual level orderings    
            } else {
              # Set fixed level order to default value
              levelOrderFixed = rep(defaultLevelOrderFixed,nVar) # Level ordering for 1-d or within bar
              
              # Go through the variables one by one and set the order. If the input is not F/f or I/i,
              # the variable order is set as decreasing
              levelOrder = rep(1,nVar)
              for (ii in seq(nVar)) {               
                
                # Variable ii is fixed order
                if (tmp[ii]=="F" | tmp[ii] == "f"){
                  levelOrderFixed[ii] = TRUE
                  
                # Variable ii is not fixed order
                } else {
                  levelOrderFixed[ii] = FALSE
                }
                
                # Variable ii is increasing
                if (tmp[ii]=="I" | tmp[ii] == "i"){
                  levelOrder[ii] = 1 
                  
                # Variable ii is  decreasing 
                }else {
                  levelOrder[ii] = -1              }
              }
            }
          }  
        }
                
        # Set minimum frequencies for variables (levels below min.freq. are excluded )
        if ("3" %in% listOfCmds) {
          
          # Give the user a list of the current variables with their numeric equivalents
          # to make it easier for the user to input values (the user has the option to 
          # specify the variables by number)
          cat("Set minimum frequencies for current variables, which are:","\n")
          promptString = ""
          for (ii in seq(length(listOfVars))) {
            promptString = paste(promptString,ii,"=",listOfVars[ii],"; ")
          }
          cat(promptString,"\n")
          
          # Query the user for input
          cat("To accept current minimum frequency (",minfDefault,"), press Enter.\n")
          cat("To set all minimum frequencies the same, input a single number.","\n")
          cat("otherwise specify min. freqs. for variables separately as a list of numbers, separated by commas","\n")
          
          # Take the user input
          # initialize reinputFlag (in case user makes a mistake and needs to repeat input)
          reinputFlag = TRUE
          
          # Take user input while reinputFlag is set
          while (reinputFlag){
            
            # If the user's input is correct, it will not be necessary to reinput
            reinputFlag = FALSE
            
            #  Receive and clean user input
            tmp = readline(prompt = "??  ")
            # Splits user input into a list of names and removes quotes (if present)
            tmp = as.list(strsplit(tmp, ","))[[1]]
            tmp = gsub('"','',tmp) # Trim single quotes
            tmp = gsub("'","",tmp) # Trim double quotes
            tmp = trimws(tmp) # trim whitespace
            
            # If the user inputs a single value, assign the same cutoff value to all variables
            if (length(tmp) > 0) {           # Check whether user has input something
              if (length(tmp)==1){           # If single input, all variables have the same cutoff
                minfDefault = as.integer(tmp)# ensures input is integer
                minf=rep(minfDefault,nVar) # Creates min cutoff
                
              # If the user enters multiple values, assign a different cutoff to each variable  
              } else {
                if (!(length(tmp)==nVar)){
                  cat("Wrong number of inputs, please re-enter minimum frequencies \n")
                  reinputFlag = TRUE
                } else{
                  minf=rep(minfDefault,nVar) # Creates min cutoff
                }
              }
            }
          }

        # If minf is not specified, assign the default minf to all variables  
        } else {
          minf=rep(minfDefault,nVar) # Creates min cutoff
        }
        
        # User can specify which plots to generate automatically
        if ("4" %in% listOfCmds) {
          cat("Change plot options:","\n \n")
          cat("Which plots do you want to generate?\n")
          cat("1= 1-variable plots;  2=2-variable plots; 3=both; Enter= no plots")

          #Input 
          tmp = readline(prompt = "?")    
          tmp = gsub('"','',tmp)
          tmp = gsub("'","",tmp)
          tmp = trimws(tmp) # trim whitespace
          if (nchar(tmp) > 0){
            if (substring(tmp,1,1)=="1"){
              LEVEL1=TRUE
              LEVEL2=FALSE
            } else if (substring(tmp,1,1)=="2"){
              LEVEL1=FALSE
              LEVEL2=TRUE
            } else {
              LEVEL1=TRUE
              LEVEL2=TRUE
            }
          } else{
            LEVEL1=FALSE
            LEVEL2=FALSE
          }
          
          # Let the user specify plot characteristics for automatically-generated plots
          # Note all plots have the same characteristics
          # Query the user
          cat("Choose between percentage/nominal and color/grayscale in your plots. Separate your choices with commas.\n")
          cat("P= percentage plots; N= nominal; C= color; G= grayscale; Enter= keep current defaults")
          cat("(Current defaults are ",defaultPctPlot," and ",defaultColorPlot,")\n")
          
          # Receive and clean user input
          tmp = readline(prompt = "?")    
          tmp = gsub('"','',tmp)
          tmp = gsub("'","",tmp)
          tmp = trimws(tmp) # trim whitespace
          
          # Check whether input is empty
          if (nchar(tmp) > 0){
            nTmp = length(tmp)
            
            # Take only the first character of each input in the list
            tmp = substring(tmp,1,1)
  
            # User has indicated percent
            if ("P" %in% tmp | "p" %in% tmp){
              defaultPctPlot = "percent"
              
            # If not percent, then the plots will be nominal (frequency)  
            } else if ("N" %in% tmp | "n" %in% tmp | "F" %in% tmp | "f" %in% tmp){
              defaultPctPlot = "nominal"
            } 
            
            # User has indicated color plot
            if ("C" %in% tmp | "c" %in% tmp){
              defaultColorPlot = "color"
            # User has indicated grayscale
            } else if ("G" %in% tmp | "g" %in% tmp){
              defaultColorPlot = "greyscale"
            }
          }
          
          # Reset defaults to agree with user input
          pctPlot = defaultPctPlot
          colorPlot = defaultColorPlot
          
          ## User specifies bar ordering in automatically-generated plots          
          cat("Set ordering of bars in bar plots:")
          cat("Current variables are:","\n")
          promptString = ""
          for (ii in seq(nVar)) {
            promptString = paste(promptString,ii,"=",listOfVars[ii],"; ")
          }
          cat(promptString,"\n")
          cat("Valid bar orderings are: T=TSP, C= cluster order, L= variable level order  \n")
          # Take user input
          cat("To accept default (",defaultBarOrder,"), press enter \n")
          cat("To set all bar orderings the same, give a single input","\n")
          cat("otherwise give bar orderings for different variables as a list separated by commas","\n")
          
          #Input bar orderings
          reinputFlag = TRUE
          while (reinputFlag){
            reinputFlag = FALSE
            tmp = readline(prompt = "??  ")
            # Splits user input into a list of names and removes quotes (if present)
            tmp = as.list(strsplit(tmp, ","))[[1]]
            tmp = gsub('"','',tmp)
            tmp = gsub("'","",tmp)
            tmp = trimws(tmp) # trim whitespace
            if (length(tmp)==0){
              barOrder = rep(defaultBarOrder,nVar) # Level ordering for 1-d or within bar
            } else if (length(tmp)==1){
              tmp = substr(tmp,1,1)
              if (tmp=="T" | tmp == "t"){
                defaultBarOrder = "TSP"
              } 
              if (tmp=="C" | tmp == "c"){
                defaultBarOrder = "Cluster"
              }else {
                defaultBarOrder = "None"
              }
              barOrder = rep(defaultBarOrder,nVar) # Level ordering for 1-d or within bar
            } else if (!(length(tmp)==nVar)){ 
              reinputFlag = TRUE
              cat("Wrong number of inputs, please try again \n")
            } else {
              barOrder = rep(defaultBarOrder,nVar) # Level ordering for 1-d or within bar
              for (ii in seq(nVar)) {               
                if (tmp[ii]=="T" | tmp[ii] == "t"){
                  barOrder[ii] = "TSP"
                }
                if (tmp[ii]=="C" | tmp[ii] == "c"){
                  barOrder[ii] = "cluster" 
                }else {
                  barOrder[ii] = "fixed"              }
              }
            }
          }  
        }
        
        # User can set output directory
        if ("5" %in% listOfCmds) {
          cat("Change output directory:\n")
          cat("Current output directory is",outputDirectory,"\n")
          cat("Please input the name of the new output directory, or press Enter if you do not want to change \n")
          tmp = readline(prompt = "? ")
          if (nchar(tmp) > 0){
            tmp = gsub('"','',tmp)
            tmp = gsub("'","",tmp)
            tmp = trimws(tmp) # trim whitespace
            outputDirectory=tmp
            dir.create(outputDirectory,showWarnings = TRUE)
          }
        }
        
        # User can decide to keep same table
        # Show the user corrent frame, variables, and min frequencies so that he can confirm
        # his choice.
        if ("6" %in% listOfCmds) {
          cat("Current frame name is: ",frameName,"\n")
          cat("Current variables are: \n")
          cat(listOfVars,"\n")
          cat("Current minimum frequencies are: \n")
          cat(minf,"\n")
          cat("Please confirm that you want to keep these settings (type Y or y to confirm) \n")
          tmp = readline(prompt = "?")    
          tmp = gsub('"','',tmp)
          tmp = gsub("'","",tmp)
          tmp = trimws(tmp) # trim whitespace
          if (substring(tmp,1,1)=="Y" | substring(tmp,1,1)=="y") {
            MAKE_TABLE = FALSE
          }
        }
      }
    }
  }    
}  

# Create output directory
dir.create(outputDirectory,showWarnings = TRUE) # If the output directory already exists, warn the user
# (the code will still execute, this is a warning only)



### Main program begins here ###
source("dataFns12.R")
source("plotFns12.R")

#plotting libraries
library(vcd)
library(grid)
library(ggplot2)
library(gridExtra)
library(viridis) # for colors

# data manipulation libraries
library(abind)
library(data.table)
library(TSP) # travelling salesman order
library(scales)
library(reshape2)

if (MAKE_TABLE){
  # Create frame of given variables
  dataTable = get(frameName)[, listOfVars]
  
  # Remove rows with NAs
  dataTable = na.omit(dataTable)

  # Remove levels with low frequency
  for (i in 1:length(listOfVars)) {
    varTmp = as.character(unlist(dataTable[, listOfVars[i]]))
    tableTmp = table(varTmp)
    dataTable = dataTable[varTmp %in% names(tableTmp)[tableTmp >= minf[i]], ]
  }
  # Remove unused levels
  dataTable = droplevels.data.frame(dataTable)
  
  # Create data table
  dataTable = table(dataTable)
  nVar = length(listOfVars)
  commaString = strrep(",", (nVar - 1))
  dataTable = orderTable(dataTable, nVar, levelOrder, levelOrderFixed)
}

# Generate comprehensive set of 1-factor plots if LEVEL1 is set
if (LEVEL1 == TRUE) {
  #create histogram and frequency plot for each variable
  for (row in  1:nVar) {
    #plot for each variable
    dataTableTmp = margin.table(dataTable, row)
    df = as.data.frame(dataTableTmp)
    df = df[df$Freq > 0,]
    varNames = colnames(df)
    barVarName = varNames[1]
    plot1Var(df, barVarName,relTextSize,TRUE,FALSE,outputDirectory,defaultColorPlot)
  }
}

### Generate comprehensive set of 2-variable plots if LEVEL2 is set
if (LEVEL2 == TRUE) {
  # Permute rows cyclically 
  permRow = c(2:nVar, 1)
  # Permute cyclically between columns for a single row
  permCol = c(1:nVar)
  if (nVar > 2) {
    # This will be the first column permutation if there are 3 or more variables
    permCol = c(1, 3:nVar, 2)
  }
  
  ## create plot for each color & bar variable combination
  listOfVarsToSave = c()
  
  # Loop through color variables  
  for (cVar in  1:nVar) {
    
    ## Loop through bar variable 
    for (bVar in 1:(nVar - 1)) {
      if (length(dim(dataTable)) > 2) {
        # Sum over all other variables so that only 2 variables remain
        dataTableTmp = rowSums(dataTable, dims = 2)
      } else{
        dataTableTmp = dataTable
      }
      # Generate 2-variable plot, using default order and pct options
      plot2VarNames = plot2Var(dataTableTmp, barOrder[2], defaultPctPlot,relTextSize,TRUE,FALSE,outputDirectory,defaultColorPlot)
      # Write to table
      if (saveTable){
        tableVarName = paste(outputDirectory,plot2VarNames[1],"_",plot2VarNames[2],sep="")
        eval(parse(text = paste(tableVarName,"=dataTableTmp",sep="")))
        eval(parse(text = paste("listOfVarsToSave = c(listOfVarsToSave,'",tableVarName,"')",sep="")))
      }
      # Move to next plot: cyclically permute table and order, pct vectors
      dataTable = aperm(dataTable, permCol)
      barOrder = barOrder[permCol]
      levelOrderFixed = levelOrderFixed[permCol]
    }
    # Finished with color variable: move it back to where it was
    dataTable = aperm(dataTable, permRow)
    barOrder = barOrder[permRow]
    levelOrderFixed = levelOrderFixed[permRow]
  }
  # Save tables if option was selected
  if (saveTable){
    tableFilename = paste(outputDirectory,"/plot2Var/plot2Tables.RData",sep="")
    save(list = listOfVarsToSave,file=tableFilename)
  }
} ### Finished with comprehensive 2-variable plots


### User  selection area: user can select different options
### for data exploration.
command = 1

while (command >= 0) {
  #User options
  cat("0=list current variables,1=plot,2=make tables, 3=select/remove levels,\n")
  cat("4=merge/rename levels,5=reorder levels,6=change directory,7=R statements,-1=quit:\n")
  command = readline(prompt = "??  ")
  
  
  ## Error catching
  result = tryCatch({ 
  
    ## List of current variables
  if (command == 0) {
    promptString = ""
    for (ii in seq(nVar)) {
      promptString = paste(promptString,ii,"=",listOfVars[ii],"; ")
    }
    cat("\n Current variables are:")
    cat(promptString,"\n")
    
  }    
    ## Create tables
  if (command == 2) {
    cat("Give row, column, and level variables (by name or number), and options:\n")
    cat("(>w = show table in window,  >f = save table to file, *c = compute Chi Square, *n = don't compute Chi Square)\n")
    stringOfVars = readline(prompt = "??  ")
    
      varNames = as.list(strsplit(stringOfVars, ","))[[1]]
      varNames = trimws(varNames)
      varNames=gsub(" ",".",varNames) # frame column names have spaces replaced with '.'
      
      ## Extract first character from strings in varNames, in order to interpret    
      varNamesInit1 = substr(varNames,1,1)
      selectVarVec = (varNamesInit1 == ">")
      if (sum(selectVarVec)>0){
        viewTable = FALSE
        saveTable = FALSE
        varNamesInit2 = substr(varNames,1,2)
        ## Set view/save table options
        selectVarTmp = (varNamesInit2 == ">w") | (varNamesInit2 == ">W") 
        if (sum(selectVarTmp)>0) {
          viewTable = TRUE
        }
        selectVarTmp = (varNamesInit2 == ">f") | (varNamesInit2 == ">F") 
        if (sum(selectVarTmp)>0) {
          saveTable = TRUE
        }
      } 

      selectVarTmp = (varNamesInit1 == "*")
      selectVarVec = selectVarVec | selectVarTmp
      if (sum(selectVarTmp)>0){
        computeChiSquare = FALSE
        varNamesInit2 = substr(varNames,1,2)
        ## Set view/save table options
        selectVarTmp = (varNamesInit2 == "*c") | (varNamesInit2 == "*C") 
        if (sum(selectVarTmp)>0) {
          computeChiSquare = TRUE
        }
      }

      varNames = varNames[!selectVarVec]
      nVarTab = length(varNames)
      df = as.data.frame(ftable(dataTable))
      dfNames = names(df)
      if (all(check.numeric(varNames,na.rm=TRUE))){
        barCol = as.numeric(varNames[1])
        barVarName = dfNames[barCol]
      } else{
        barVarName = varNames[1]
        barCol = which(dfNames==barVarName) 
      }
      
      ## 1-variable case
      if (nVarTab == 1) {
        dfTmp = margin.table(dataTable, barCol)
        dfTmp = as.data.frame(dfTmp)
        dfTmp = dfTmp[dfTmp$Freq > 0,]
        if (viewTable){
          View(dfTmp)
        }
        if (saveTable){
          tableVarName = barVarName
          tableFilename = paste(outputDirectory,"/",tableVarName,".RData",sep="")
          eval(parse(text=paste(tableVarName,"=dfTmp")))
          save(tableVarName,file=tableFilename, ascii = TRUE)
        } ##  End 1-variable tables 
      
      ## More than 1 variable    
      } else {
        if (all(check.numeric(varNames,na.rm=TRUE))){
          colorCol = as.numeric(varNames[2])
          colorVarName = dfNames[colorCol]
        } else{
          colorVarName = varNames[2]
          colorCol = which(dfNames == colorVarName)
        }
        
        ## 2-variable case
        if (nVarTab == 2) {       
          #Create a data table with only the 2 variables we want
          #Remove all other variables using rowsums
          #Move the 2 variables to the front of the table
          #create permutation that moves the 3 columns to the front
          nColTmp = ncol(df) - 1
          permCol = replicate(nColTmp, 0)
          permTmp = c(1:nColTmp)
          #Make the color column the first column
          permCol[1] = colorCol
          permTmp = permTmp[!(permTmp == colorCol)]
          permCol[2] = barCol
          permTmp = permTmp[!(permTmp == barCol)]
          if (nColTmp > 2) {
            permCol[permCol == 0] = permTmp
          }
          #rearrange columns using aperm
          dataTableTmp = aperm(dataTable, permCol)
          if (length(dim(dataTableTmp)) > 2) {
            dataTableTmp = rowSums(dataTableTmp, dims = 2)
          }
          
          # compute Chi Square
          if (computeChiSquare){
            print(chisq.test(dataTableTmp))
            # print(fisher.test(dataTableTmp))
          }
          if (viewTable){
            View(dataTableTmp)
          }
          if (saveTable){
            tableVarName = paste(colorVarName,".by.",barVarName,sep="")
            tableFilename = paste(outputDirectory,"/",tableVarName,".RData",sep="")
            eval(parse(text=paste(tableVarName,"=dataTableTmp")))
            save(tableVarName,file=tableFilename, ascii = TRUE)
          }      

          ## 3-variable tables          
        } else{
          if (all(check.numeric(varNames,na.rm=TRUE))){
            paneCol = as.numeric(varNames[3])
            paneVarName = dfNames[paneCol]
          } else{
            paneVarName = varNames[3]
            paneCol = which(dfNames == paneVarName)
          }

          #Create a data table with only the 3 variables we want
          #Remove all other variables using rowsums
          #Move the 3 variables to the front of the table
          nColTmp = ncol(df) - 1
          permCol = replicate(nColTmp, 0)
          permTmp = c(1:nColTmp)
          #Put the color,bar,pane columns into 1,2,3
          permCol[1] = colorCol
          permTmp = permTmp[!(permTmp == colorCol)]
          permCol[2] = barCol
          permTmp = permTmp[!(permTmp == barCol)]
          permCol[3] = paneCol
          permTmp = permTmp[!(permTmp == paneCol)]
          if (nColTmp > 3) {
            permCol[permCol == 0] = permTmp
          }
          #rearrange columns using aperm
          dataTableTmp = aperm(dataTable, permCol)
          if (length(dim(dataTableTmp)) > 3) {
            dataTableTmp = rowSums(dataTableTmp, dims = 3)
          }
          ## Order bars according to traveling salesman
          if (substr(barOrder[permCol],1,1) == "T" | substr(barOrder[permCol],1,1) == "t") {
            dataTablePct = rowSums(dataTableTmp, dims = 2)
            dataTablePct = sweep(dataTablePct, 2, colSums(dataTablePct), "/")
            orderBars = TSPorder(as.table(dataTablePct))
            dataTableTmp = dataTableTmp[, orderBars, , drop = FALSE]# Reorder the table
          }else if (substr(barOrder[permCol],1,1) == "C" | substr(barOrder[permCol],1,1) == "c"){
            dataTablePct = rowSums(dataTableTmp, dims = 2)
            dataTablePct = sweep(dataTablePct, 2, colSums(dataTablePct), "/")
            corMx = cor(dataTablePct)
            # In some cases, the variance may be 0, and no clustering is possible.
            if (sum(is.na(corMx))==0){
              # Perform clustering of data to group together 
              # bars that are similar.
              dd <- as.dist((1-corMx)/2)
              hc <- hclust(dd)
              dataTableTmp = dataTableTmp[,hc$order, , drop = FALSE]# Reorder the table
            }
          }
          dfTmp = as.data.frame(ftable(dataTableTmp))
          if (viewTable){
            View(dfTmp)
          }
          if (saveTable){
            tableVarName = paste(colorVarName,".by.",barVarName,".per.",paneVarName,sep = "")
            eval(parse(text = paste(tableVarName,"=dataTableTmp",sep="")))
            tableFilename = paste(outputDirectory,"/",tableVarName,".RData",sep="")
            eval(parse(text=paste(tableVarName,"=dataTableTmp")))
            save(tableVarName,file=tableFilename, ascii = TRUE)
          }
          # compute Chi Square
          if (computeChiSquare){
            levelNamesTmp = row.names(as.table(dataTableTmp[1,1,]))
            pValsTmp = levelNamesTmp
            fisher_pValsTmp = levelNamesTmp
            for ( ii in seq(dim(dataTableTmp)[3]) ){
              pValsTmp[ii] = chisq.test(dataTableTmp[,,ii])$p.value
              #fisher_pValsTmp[ii] = fisher.test(dataTableTmp[,,ii])$p.value
            }
            dfTmp = data.frame(levelNamesTmp,pValsTmp,fisher_pValsTmp)
            chisqVarName = paste(colorVarName,".by.",barVarName,".per.",paneVarName,sep = "")
            eval(parse(text = paste(chisqVarName,"=dfTmp",sep="")))
            eval(parse(text = paste("View(",chisqVarName,")",sep="")))
            eval(parse(text = paste("print(",chisqVarName,")",sep="")))
            
          }
        } ## End 3-variable table
      }
    
  ## Select/remove levels option
  } else if (command == 3) {
    cat("Variable name or number \n")
    tmp = readline(prompt = "??  ")
    #Divide user input into individual names
    tmp = gsub('"','',tmp)
    tmp = gsub("'","",tmp)
    tmp = trimws(tmp)
    if (!check.numeric(tmp)){
      tmp = which(listOfVars == tmp)
    }
    # Create a permutation to switch the var. column with the first column
    permCol = c(1:nVar)
    # Switches the first column out
    tmp = as.numeric(tmp)
    permCol[1] = tmp
    # Switches the variable column in
    permCol[tmp] = 1
    # Performs the permutation
    dataTable = aperm(dataTable, permCol)
    
    # Generate list of levels with indices
    cat("Current levels are:","\n")
    promptString = ""
    levelNames = row.names(dataTable)
    for (ii in seq(length(levelNames))) {
      promptString = paste(promptString,ii,"=",levelNames[ii],"; ")
    }
    cat(promptString,"\n")
    cat("Levels to add/remove (by name or number), + to keep, - to remove","\n")
    #Input 
    listOfLevels = readline(prompt = "??  ")
    # Splits user input into a list of names
    listOfLevels = as.list(strsplit(listOfLevels, ","))[[1]]
    listOfLevels = gsub('"','',listOfLevels)
    listOfLevels = gsub("'","",listOfLevels)
    listOfLevels = trimws(listOfLevels) # trim whitespace
    if ("-" %in% listOfLevels){
      remove = TRUE
      listOfLevels = listOfLevels[!(listOfLevels=="-")]
    } else{
      remove = FALSE
      listOfLevels = listOfLevels[!(listOfLevels=="+")]
    }
    selectLevelVec = rep(remove,length(levelNames))
    if (all(check.numeric(listOfLevels,na.rm=TRUE))){
      listOfLevels=as.numeric(listOfLevels)
      selectLevelVec[listOfLevels] = !remove
    } else{
      for (ii in seq(length(levelNames))) {
        if (levelNames[ii] %in% listOfLevels) {
          selectLevelVec[ii] = !remove
        }
      }  
    }
    listOfLevels = levelNames[selectLevelVec]
    commandString = paste("dataTable=dataTable[listOfLevels",
                            commaString,
                            ",drop=FALSE]")
    eval(parse(text = commandString))
    # Move the column back again
    dataTable = aperm(dataTable, permCol)

  ##Merge levels
  } else if (command == 4) {

    cat("Name or number of variable to merge \n")
    selectVar = readline(prompt = "??  ")
    #Divide user input into individual names
    selectVar = gsub('"','',selectVar)
    selectVar = gsub("'","",selectVar)
    selectVar = trimws(selectVar)
    if (check.numeric(selectVar)){
      tmp = as.numeric(selectVar)
    } else {
      tmp = which(listOfVars == selectVar)
    }
    # Create a permutation to switch the var. column with the first column
    permCol = c(1:nVar)
    # Switches the first column out
    permCol[1] = tmp
    # Switches the variable column in
    permCol[tmp] = 1
    # Performs the permutation
    dataTable = aperm(dataTable, permCol)
    
    # Generate list of levels with indices
    cat("Current levels are:","\n")
    promptString = ""
    levelNames = row.names(dataTable)
    for (ii in seq(length(levelNames))) {
      promptString = paste(promptString,ii,"=",levelNames[ii],"; ")
    }
    cat(promptString,"\n")
    cat("Levels to merge,followed by new level name","\n")
    #Input 
    listOfLevels = readline(prompt = "??  ")
    # Splits user input into a list of names
    listOfLevels = as.list(strsplit(listOfLevels, ","))[[1]]
    listOfLevels = gsub('"','',listOfLevels)
    listOfLevels = gsub("'","",listOfLevels)
    listOfLevels = trimws(listOfLevels) # trim whitespace
    lenTmp = length(listOfLevels)
    newLevel = listOfLevels[lenTmp]
    mergedLevels = listOfLevels[c(-lenTmp)]
    if (all(check.numeric(mergedLevels,na.rm=TRUE))){
      mergedLevels=levelNames[as.numeric(mergedLevels)]
    }
    
    # Select rows corresponding to merged levels
    ## Note "drop=FALSE' prevents the removal of a dimension if there's only 1 level
    commandString = paste("tmpTable=dataTable[row.names(dataTable)%in%mergedLevels",
                          commaString,",drop=FALSE]")
    eval(parse(text = commandString))
    # Add them together to put into the merged level
    dataTableMergedRow = colSums(tmpTable, dims = 1)
    rm(tmpTable) # No longer needed
    # Put the new merged level into the place where the first original level was
    rowToChange = mergedLevels[1]
    # Put the new data in this place
    commandString = paste("dataTable[rowToChange",
                          commaString,
                          "]=dataTableMergedRow")
    eval(parse(text = commandString))
    rm(dataTableMergedRow) # Not needed any more
    # Locate the name to be changed to the new merged level name
    selectName = (row.names(dataTable) == rowToChange)
    # Assign the new merged name to the correct row
    row.names(dataTable)[selectName] = newLevel
    # Remove rows from other levels that have been merged
    ## Note "drop=FALSE' prevents the removal of a dimension if there's only 1 level
    commandString = paste(
      "dataTable = dataTable[!row.names(dataTable)%in%mergedLevels",
      commaString,
      ",drop=FALSE]"
    )
    eval(parse(text = commandString))
    # Permute the variable column back to its original position
    dataTable = aperm(dataTable, permCol)
    # Order the table in case the changes have disturbed the order
    dataTable = orderTable(dataTable, nVar, levelOrder, levelOrderFixed)
    
  ## Reorder levels
  } else if (command == 5) {
    #user input
    
    cat("Variable name or number \n")
    tmp = readline(prompt = "??  ")
    #Divide user input into individual names
    tmp = gsub('"','',tmp)
    tmp = gsub("'","",tmp)
    tmp = trimws(tmp)
    if (!check.numeric(tmp)){
      tmp = which(listOfVars == tmp)
    }
    tmp = as.numeric(tmp)
    permCol = c(1:nVar)
    permCol[1] = tmp
    # Switches the variable column in
    permCol[tmp] = 1
    # Performs the permutation
    dataTable = aperm(dataTable, permCol)
    
    # Generate list of levels with indices
    cat("Current level ordering is:","\n")
    promptString = ""
    levelNames = row.names(dataTable)
    for (ii in seq(length(levelNames))) {
      promptString = paste(promptString,ii,"=",levelNames[ii],"; ")
    }
    cat(promptString,"\n")
    cat("New level ordering (by name or number)","\n")
    #Input 
    listOfLevels = readline(prompt = "??  ")
    # Splits user input into a list of names
    listOfLevels = as.list(strsplit(listOfLevels, ","))[[1]]
    listOfLevels = gsub('"','',listOfLevels)
    listOfLevels = gsub("'","",listOfLevels)
    listOfLevels = trimws(listOfLevels) # trim whitespace
    if (length(listOfLevels) == 0) {
      levelOrderFixed[tmp] = FALSE
    } else {
      levelOrderFixed[tmp] = TRUE
      if (all(check.numeric(listOfLevels,na.rm=TRUE))){
        listOfLevels=as.numeric(listOfLevels)
      }
      # All other names are names of levels
      commandString = paste("dataTable=dataTable[listOfLevels",
                            commaString,
                            ",drop=FALSE]")
      eval(parse(text = commandString))
      # Move the column back again
    }
    dataTable = aperm(dataTable, permCol)

  ## Change directory  
  } else if (command == 6) {
    #user input
    cat("New directory name") 
    outputDirectory = readline(prompt = "?")
  
  ## R commands  
  } else if (command == 7) {
    #user input
    #cat("Input R statements (q for quit) \n") 
    tmp = readline(prompt = "(quit=q)?")
    while (!(tmp=="q")){
      eval(parse(text=tmp))
      tmp = readline(prompt = "?")
    }

    #### Plot histograms of 1,2,or 3 variables
  } else if (command == 1) {
    inputFlag = TRUE
    while(inputFlag==TRUE){
      cat("Give bar,color,pane variable, and options.\n")
      cat("(To show possible options, enter '?')\n")
      tmp = readline(prompt = "??  ")
      tmp = as.list(strsplit(tmp, ","))[[1]]
      tmp=gsub("'","",tmp) # Remove quotes
      tmp=gsub('"','',tmp) # Remove quotes
      plotOptions = trimws(tmp)
      plotOptionsInit1 = substr(plotOptions,1,1)  # first character
      if (plotOptionsInit1[1] == "?"){
        cat("Options:\n\n *d=doubledecker,*f=*n=nominal,*p=percentage,*s=spineplot;*c for color, *g for grayscale; \n")
        cat("+ or - for incr. or decr. font size (add multiple +'s or -'s for larger change); \n")
        cat(">w for print to window, >f for print to file (default is print to window only);\n")
        cat("@TSP, @cluster, or @none for bar ordering algorithm,\n\n")
        cat("Variables may be entered by name or number.  For variable list, enter >v or *v \n\n")
      } else{ 
        plotOptionsInit2 = substr(plotOptions,1,2)  # first two characters
        if (any(plotOptionsInit2 == "*v") | any(plotOptionsInit2 == "*V")
            |any(plotOptionsInit2 == "@v") | any(plotOptionsInit2 == "@V")
            |any(plotOptionsInit2 == ">v") | any(plotOptionsInit2 == ">V")){
          ## Generate list of variables and give user numeric equivalentss
          cat("\n Current variables are:","\n")
          promptString = ""
          for (ii in seq(nVar)) {
            promptString = paste(promptString,ii,"=",listOfVars[ii],"; ")
          }
          cat(promptString,"\n")
        } else{
          inputFlag=FALSE
        }
      }
    }
    ## Decide whether plot is percentage or nominal
    selectVec = (plotOptionsInit2=="*f")|(plotOptionsInit2=="*F")|(plotOptionsInit2 =="*n")|(plotOptionsInit2=="*N")   
    if (sum(selectVec)>0){
      pctPlot = "nominal"
    } 
    selectVecTmp = (plotOptionsInit2=="*p")|(plotOptionsInit2=="*P")
    if (sum(selectVecTmp)>0){
      pctPlot = "percent"
      selectVec = selectVec | selectVecTmp
    }
    selectVecTmp = (plotOptionsInit2=="*s")|(plotOptionsInit2=="*S")
    if (sum(selectVecTmp)>0){
      pctPlot = "spineplot"
      selectVec = selectVec | selectVecTmp
    }
    selectVecTmp = (plotOptionsInit2=="*d")|(plotOptionsInit2=="*D")
    if (sum(selectVecTmp)>0){
      pctPlot = "doubledecker"
      selectVec = selectVec | selectVecTmp
    }
    
    
    ## Decide whether plot is color or grayscale
    selectVecTmp = (plotOptionsInit2=="*c")|(plotOptionsInit2=="*C")   
    if (sum(selectVecTmp)>0){
      colorPlot = "color"
      selectVec = selectVec | selectVecTmp
    } else {
      selectVecTmp = (plotOptionsInit2=="*g")|(plotOptionsInit2=="*G")
      if (sum(selectVecTmp)>0){
        colorPlot = "grayscale"
        selectVec = selectVec | selectVecTmp
      } 
    }
      
    ## Decide whether to increase or decrease font size
    selectVecTmp = (plotOptionsInit1=="+")   
    fontSizeScale = sum(nchar(plotOptions[selectVecTmp]))
    selectVec = selectVec | selectVecTmp
    
    selectVecTmp = (plotOptionsInit1=="-")   
    fontSizeScale = fontSizeScale - sum(nchar(plotOptions[selectVecTmp]))
    selectVec = selectVec | selectVecTmp
    relTextSize = relTextSize + relTextSizeIncr*fontSizeScale
    fontSizeScale=0 # Reset font size scale

    
    ## Decide whether to plot to screen or plot to file
    selectVecTmp = (plotOptionsInit1==">")
    selectVec=selectVec | selectVecTmp
    if (sum(selectVecTmp)>0){
      plotToFile = FALSE
      plotToWindow = FALSE
      selectVecTmp = (plotOptionsInit2==">w")|(plotOptionsInit2==">W")   
      if (sum(selectVecTmp)>0){
        plotToWindow = TRUE
      } 
      selectVecTmp = (plotOptionsInit2==">f")|(plotOptionsInit2==">F")   
        if (sum(selectVecTmp)>0){
        plotToFile = TRUE
      } 
    }

    ## Change bar order, if specified
    selectVecTmp = (plotOptionsInit1=="@")
    selectVec = selectVec | selectVecTmp
    newBarOrder = FALSE
    if (sum(selectVecTmp)>0){
      selectVecTmp = (plotOptionsInit2=="@T")|(plotOptionsInit2=="@t")   
      if (sum(selectVecTmp)>0){
        barOrderTmp = "TSP"
        newBarOrder = TRUE
      } 
      selectVecTmp = (plotOptionsInit2=="@c")|(plotOptionsInit2=="@C")   
      if (sum(selectVecTmp)>0){
        barOrderTmp = "cluster"
        newBarOrder = TRUE
      } 
      if (newBarOrder == FALSE) {
        barOrderTmp = "none"
        newBarOrder = TRUE
      }
      plotOptions = plotOptions[!selectVec]
    }
    # Remaining fields should be variable names
    varNames = plotOptions[!selectVec]
    
    ## This changes the table back to a data frame to use ggplot
    df = as.data.frame(ftable(dataTable))
    dfNames = names(df)
    #choose bar variable
    if (check.numeric(varNames[1])){
      barCol = as.integer(varNames[1])
      barVarName = dfNames[barCol]
    } else{
      barVarName = varNames[1]
      barCol = which(dfNames == barVarName)
    }
    # Change bar ordering as specified by the user
    if (newBarOrder){
      barOrder[barCol] = barOrderTmp
    }
    
    nVarPlt = length(varNames)
    
    if (nVarPlt == 1) {
      row = which(colnames(df)==barVarName)
      dfTmp = margin.table(dataTable, row)
      dfTmp = as.data.frame(dfTmp)
      dfTmp = dfTmp[dfTmp$Freq > 0,]
      plot1Var(dfTmp, barVarName,relTextSize,plotToFile,plotToWindow,outputDirectory,colorPlot)
      if (viewTable){
        View(dataTableTmp)
      }
      if (saveTable){
        tableVarName = barVarName
        eval(parse(text = paste(tableVarName,"=dataTableTmp",sep="")))
        tableFilename = paste(outputDirectory,"/",tableVarName,".RData",sep="")
        eval(parse(text=paste(tableVarName,"=dataTableTmp")))
        save(tableVarName,file=tableFilename, ascii = TRUE)
      } ##  End 1-variable plot 
      
    } else {
      if (check.numeric(varNames[2])){
        colorCol = as.integer(varNames[2])
        colorVarName = dfNames[colorCol]
      } else{
        colorVarName = varNames[2]
        colorCol = which(dfNames == colorVarName)
      }
      ## 2-variable plot
      if (nVarPlt == 2) {       
        #Create a data table with only the 2 variables we want
        #Remove all other variables using rowsums
        #Move the 2 variables to the front of the table
        nColTmp = ncol(df) - 1
        permCol = replicate(nColTmp, 0)
        permTmp = c(1:nColTmp)
        #Make the color column the first column
        permCol[1] = colorCol
        permTmp = permTmp[!(permTmp == colorCol)]
        permCol[2] = barCol
        permTmp = permTmp[!(permTmp == barCol)]
        if (nColTmp > 2) {
          permCol[permCol == 0] = permTmp
        }
        #rearrange columns using aperm
        dataTableTmp = aperm(dataTable, as.integer(permCol))
        if (length(dim(dataTableTmp)) > 2) {
          dataTableTmp = rowSums(dataTableTmp, dims = 2)
        }
        plot2VarNames = plot2Var(dataTableTmp, barOrder[barCol], pctPlot,relTextSize,plotToFile,plotToWindow,outputDirectory,colorPlot)
        
      } else{
        ## 3-variable plot
        if (check.numeric(varNames[3])){
          paneCol = as.numeric(varNames[3])
          paneVarName = dfNames[paneCol]
        } else{
          paneVarName = varNames[3]
          paneCol = which(dfNames == paneVarName)
        }
        plotTitle = paste(gsub("\\."," ",colorVarName),"by",gsub("\\."," ",barVarName),"per",
              gsub("\\."," ",paneVarName),sep = " ")
        
        #create permutation that moves the 3 columns to the front
        nColTmp = ncol(df) - 1
        permCol = replicate(nColTmp, 0)
        permTmp = c(1:nColTmp)
        #Put the color,bar,pane columns into 1,2,3
        permCol[1] = colorCol
        permTmp = permTmp[!(permTmp == colorCol)]
        permCol[2] = barCol
        permTmp = permTmp[!(permTmp == barCol)]
        permCol[3] = paneCol
        permTmp = permTmp[!(permTmp == paneCol)]
        if (nColTmp > 3) {
          permCol[permCol == 0] = permTmp
        }
        #rearrange columns using aperm
        dataTablePlot = aperm(dataTable, permCol)
        if (length(dim(dataTablePlot)) > 3) {
          dataTablePlot = rowSums(dataTablePlot, dims = 3)
        }
        ## Order bars according to traveling salesman
        if (barOrder[barCol] == "TSP") {
          dataTablePct = rowSums(dataTablePlot, dims = 2)
          dataTablePct = sweep(dataTablePct, 2, colSums(dataTablePct), "/")
          orderBars = TSPorder(as.table(dataTablePct))
          dataTablePlot = dataTablePlot[, orderBars, , drop = FALSE]# Reorder the table
        }else if (barOrder[barCol] == "cluster"){
          dataTablePct = rowSums(dataTablePlot, dims = 2)
          dataTablePct = sweep(dataTablePct, 2, colSums(dataTablePct), "/")
          corMx = cor(dataTablePct)
          # In some cases, the variance may be 0, and no clustering is possible.
          if (sum(is.na(corMx))==0){
            # Perform clustering of data to group together 
            # bars that are similar.
            dd <- as.dist((1-corMx)/2)
            hc <- hclust(dd)
            dataTablePlot = dataTablePlot[,hc$order, , drop = FALSE]# Reorder the table
          }
        }
        dfPlot = as.data.frame(ftable(dataTablePlot))
        # Colors for plot levels (spineplot or doubledecker)
        nColors = length(levels(dfPlot[,1]))

        ## Multiple spineplots 
        if ((substr(pctPlot,1,1)=="s")|(substr(pctPlot,1,1)=="S")){
          # Number and names of plots
          lvls = levels(dfPlot[,3])
          nl = length(lvls)
          
          # User specifies how to arrange plots
          cat("Number of plots in each row")
          nc = as.numeric(readline(prompt = "?"))
          nr = ceiling(nl/nc)
          par(mfrow=c(nr,nc))
          
          # Generate plots
          for (pnum in seq(nl)){
            dataTableTmp = dataTablePlot[,,pnum]
            colsTmp = colSums(dataTableTmp)>0
            dataTableTmp = t(dataTableTmp[,colsTmp]) 
            if ((substr(colorPlot,1,1)=="c") | (substr(colorPlot,1,1)=="C")){
              # Color plot
              spineplot(dataTableTmp,
                        col=hcl.colors(nColors),main=lvls[pnum])
            } else{
              # Grayscale plot
              spineplot(dataTableTmp,main=lvls[pnum])
            }
            # horizontal gridlines
            abline(h=seq(from=0.1,to=0.9,by=0.1),lty=2,lwd = 1)
            
          }
          # make title
          mtext(plotTitle, outer=TRUE,  cex=1.5)
          
          # Reset plot arrangement to 1 plot
          par(mfrow=c(1,1))
          
          ## Doubledecker plot
        } else if ((substr(pctPlot,1,1)=="d")|(substr(pctPlot,1,1)=="D")){
          dataTablePlot = aperm(dataTablePlot,c(3,2,1))
          if ((substr(colorPlot,1,1)=="c") | (substr(colorPlot,1,1)=="C")){
            # Color plot
            doubledecker(dataTablePlot,gp = gpar(fill = rev(hcl.colors(nColors))),main=plotTitle)
          } else{
            # Grayscale plot
            doubledecker(dataTablePlot,main=plotTitle)          }
        
          ## ggplot otherwise  
        } else{
        
          # User specifies number of plots in each row
          cat("Number of plots in each row")
          nc = as.numeric(readline(prompt = "?"))
          
            
          dfPlot$Percent = dfPlot$Freq #Better labeling
          dfPlot$Frequency = dfPlot$Freq
          
          # This is the ggplot command
          plot1 = ggplot(dfPlot)
          
          if ((substr(pctPlot,1,1)=="p")|(substr(pctPlot,1,1)=="P")){
                        plot1=plot1+ 
              aes_string(x = barVarName,y = "Percent",fill = colorVarName) +
              geom_bar(position = "fill",stat = "identity",color = "grey85",size = 0.25) +
              scale_y_continuous(breaks = seq(0.1, 0.9, .1),minor_breaks = seq(0.02, 0.98, 0.02),
                                 labels = c("0.1"="10","0.2"="20","0.3"="30","0.4"="40",
                                            "0.5"="50","0.6"="60","0.7"="70","0.8"="80",
                                            "0.9"="90")) 
          } else{
            plot1=plot1+ 
              aes_string(x = barVarName,y = "Frequency",fill = colorVarName) +
              geom_bar(position = "stack",stat = "identity",color = "grey85",size = 0.25)
          }
          plot1 = plot1 + facet_wrap(~ get(paneVarName),ncol = nc) +
            theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
            theme(panel.grid.minor.y = element_line(colour = "black")) +
            theme(panel.grid.major.y = element_line(colour = "black",size=1)) +
            theme(plot.title = element_text( size=rel(relTextSize)))+
            theme(axis.title.x = element_text( size=rel(relTextSize)))+
            theme(axis.title.y = element_text( size=rel(relTextSize)))+
            theme(axis.text.x = element_text(size = rel(relTextSize))) +
            theme(axis.text.y = element_text(size = rel(relTextSize)))+
            theme(legend.text=element_text(size = rel(relTextSize))) +
            labs(title = plotTitle)
          if (!(substr(colorPlot,1,1)=="c") & !(substr(colorPlot,1,1)=="C")){
            plot1 = plot1 + scale_fill_grey()
          } else{
            plot1 = plot1 + scale_fill_viridis(option=viridis_option,discrete=TRUE)
          }
          if (plotToFile==TRUE) {
            # Create file name for completed plot
            dirTmp = paste(outputDirectory,"/plot3Var/",sep="")
            dir.create(dirTmp,showWarnings = FALSE)
            cat("File name (press Enter for default)")
            tmp = readline(prompt = "??  ")
            if (nchar(tmp) > 0) {
              plotName = tmp
            } else {
              plotName = paste(dirTmp,colorVarName,".by.",barVarName,
                             ".per.",paneVarName,sep = "")
              # Create new file if the file already exists
              while (file.exists(paste(plotName, ".pdf", sep = ""))) {
               plotName = paste(plotName, "_", sep = "")
              }
              plotName = paste(plotName, ".pdf", sep = "")
            }
            pdf(plotName, width = 11, height = 8.5)
            print(plot1)
            dev.off()
          } 
          if (plotToWindow==TRUE){
            print(plot1)
          }
        }
      } ## End 3-variable plot
    }
    ## tryCatch warning  
  }}, error =function(err) {
    cat("\n Input error. The following error was encountered:\n\n")
    print(err)
    cat("\nPlease try again.\n\n")
    return(0)
  }
  )# End of tryCatch
}