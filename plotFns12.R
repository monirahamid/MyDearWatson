# Layout function for 1-d variable plot routines
vplayout <-
  function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)

# 1-variable plot routine
plot1Var <- function(df1,barVarName,relTextSize,plotToFile,plotToWindow,outputDirectory,colorPlot) {
  PctVals =   as.character(round(100*df1$Freq/sum(df1$Freq),digits=1))
  PctVals = paste(PctVals,"%",sep="")
  df1$Frequency = df1$Freq
  # This is the ggplot command
  plot1 = ggplot(df1, aes_string(x = barVarName, y = "Frequency", fill = barVarName)) +
    geom_bar( stat="identity")  +
    geom_text(aes(label=PctVals, angle=30),nudge_y=df1$Freq[1]/40,size=4*relTextSize) +
    theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) +
    theme(plot.title = element_text( size=rel(relTextSize)))+
    theme(axis.title.x = element_text( size=rel(relTextSize)))+
    theme(axis.title.y = element_text( size=rel(relTextSize)))+
    theme(axis.text.x = element_text( size=rel(relTextSize)))+
    theme(axis.text.y = element_text( size=rel(relTextSize)))+
    theme(legend.title=element_text(size=rel(relTextSize))) +
    theme(legend.text=element_text(size=rel(relTextSize))) +
    labs(title = barVarName)
  if (!(substr(colorPlot,1,1)=="c") & !(substr(colorPlot,1,1)=="C")){
    plot1 = plot1 + scale_fill_grey()
  }else{
    plot1 = plot1 + scale_fill_viridis(option=viridis_option,discrete=TRUE)
  }
  
  # Create single color bar
  #Dummy group variable (to create single bar)
  df1$dummy = 1
  
  #Create a label variable
  df1$percent = round((df1$Freq/sum(df1$Freq)) * 100,digits=1)
  
  plot2 = ggplot(df1, aes_string(x = "dummy", y = "percent", fill = barVarName)) +
    geom_bar(stat="identity") + 
    geom_text(aes(label = percent), 
              position = position_stack(vjust = 0.5),size=5*relTextSize) +
    theme(plot.title = element_text( size=rel(relTextSize)))+
    theme(axis.title.x = element_text( size=rel(relTextSize)))+
    theme(axis.title.y = element_text( size=rel(relTextSize)))+
    theme(axis.text.x = element_text( size=rel(relTextSize)))+
    theme(axis.text.y = element_text( size=rel(relTextSize)))+
    theme(legend.title=element_text(size=rel(relTextSize))) +
    theme(legend.text=element_text(size=rel(relTextSize))) +
    scale_y_continuous(breaks=labelVec,labels=as.character(100-labelVec)) +
    scale_x_continuous(name=NULL, breaks=NULL,labels=NULL)
  if (!(substr(colorPlot,1,1)=="c") & !(substr(colorPlot,1,1)=="C")){
    plot2 = plot2 + scale_fill_grey()
  }else{
    plot2 = plot2 + scale_fill_viridis(option=viridis_option,discrete=TRUE)
  }
  
  
  if (plotToFile==TRUE) {
    # Create file name for completed plot
    dirTmp = paste(outputDirectory,"/plot1Var/",sep="")
    dir.create(dirTmp,showWarnings = TRUE)
    plotName = paste(dirTmp,barVarName,sep="")
    # If file name is already in use, add '_'
    while (file.exists(paste(plotName,".pdf",sep=""))) {
      plotName = paste(plotName,"_",sep="")
    }
    plotName = paste(plotName,".pdf",sep="")
    # pdf fills an entire page
    pdf(plotName, width = 11, height = 8.5)
    # Arrange plots on page
    grid.newpage()
    # Create space for histogram and stacked bars
    # 80% for the bar plot, 20% for the colorbar
    pushViewport(viewport(layout = grid.layout(1, 5)))
    print(plot1, vp = vplayout(1, 1:4))
    print(plot2, vp = vplayout(1, 5))
    dev.off()
    
    # Plot colorbar plot separately
    # Create file name for completed plot
    dirTmp = paste(outputDirectory,"/plot1Var/colorBar/",sep="")
    dir.create(dirTmp,showWarnings = TRUE)
    plotName = paste(dirTmp,barVarName,sep="")
    # If file name is already in use, add '_'
    while (file.exists(paste(plotName,".pdf",sep=""))) {
      plotName = paste(plotName,"_",sep="")
    }
    plotName = paste(plotName,".pdf",sep="")
    pdf(plotName, width = 4, height = 8.5)
    print(plot2)  
    dev.off()
    
    # Plot bar plot separately
    dirTmp = paste(outputDirectory,"/plot1Var/barPlot/",sep="")
    dir.create(dirTmp,showWarnings = TRUE)
    plotName = paste(dirTmp,barVarName,sep="")
    # If file name is already in use, add '_'
    while (file.exists(paste(plotName,".pdf",sep=""))) {
      plotName = paste(plotName,"_",sep="")
    }
    plotName = paste(plotName,".pdf",sep="")
    pdf(plotName, width = 11, height = 8.5)
    print(plot1)  
    dev.off()
  }  
  if (plotToWindow==TRUE) { # Print to window
    print(plot2)
    print(plot1)
  }
} ## end 1-variable plot routine

## 2-variable plot routine
plot2Var <- function(dataTablePlot,barOrder,pctPlot,relTextSize,plotToFile,plotToWindow,outputDirectory,colorPlot) {
  
  ## Compute correlation matrix
  corMx = cor(dataTablePlot)
  # In some cases, the variance may be 0, and no clustering is possible.
  if (sum(is.na(corMx))==0){
    # Perform clustering of data to group together 
    # bars that are similar.
    dd <- as.dist((1-corMx)/2)
    hc <- hclust(dd)
    corMx <-corMx[hc$order, hc$order]
  }
  corMx = melt((corMx))
  
  ## Change frequency to percentage if flag is set
  if (substr(pctPlot,1,1)=="p" | substr(pctPlot,1,1)=="P"){
    dataTablePlot=sweep(dataTablePlot,2,colSums(dataTablePlot),"/")
  }
  ## Order bars as indicated
  if (barOrder=="TSP"){
    orderBars=TSPorder(dataTablePlot)
    dataTablePlot=dataTablePlot[,orderBars,drop=TRUE]# Reorder the table
  }else if (barOrder == "cluster"){
    dataTablePlot=dataTablePlot[,hc$order,drop=TRUE]# Reorder the table
  }
  
  # No need to plot if only one category
  if ((ncol(dataTablePlot)>1)&nrow(dataTablePlot)>1){
  
    # print result of chi-square and Fisher test
    print(chisq.test(dataTablePlot))
    # print(fisher.test(dataTablePlot))
    print(dataTablePlot)
    
    # Put data in frame to plot histograms
    df = as.data.frame(ftable(dataTablePlot))
    varNames = colnames(df)    
    colorVarName = varNames[1]
    barVarName = varNames[2]
    plotTitle = paste(gsub("\\."," ",colorVarName),"by",gsub("\\."," ",barVarName),sep=" ")
    
    if ((substr(pctPlot,1,1)=="s")|(substr(pctPlot,1,1)=="S")
        |(substr(pctPlot,1,1)=="d")|(substr(pctPlot,1,1)=="D")){

      nColors = length(levels(df[,1]))
      
      colsTmp = colSums(dataTablePlot)>0
      dataTablePlot = t(dataTablePlot[,colsTmp])
      # Generate plots
        if ((substr(colorPlot,1,1)=="c") | (substr(colorPlot,1,1)=="C")){
          # Color plot
          spineplot(dataTablePlot,col=hcl.colors(nColors),main=plotTitle)
        } else{
          # Grayscale plot
          spineplot(dataTablePlot,
                    main=plotTitle)
        }
        # horizontal gridlines
        abline(h=seq(from=0.1,to=0.9,by=0.1),lty=2,lwd = 1)
      ## Plot other than spineplot or doubledecker    
    } else{  

    # This is the ggplot command for the bar plot
    df$Percent = df$Freq # Better labeling
    df$Frequency = df$Freq # Better plot labeling
    plot1=ggplot(df)
    
    # In case of percentage plot
    if (substr(pctPlot,1,1)=="p" | substr(pctPlot,1,1)=="P"){      
      plot1=plot1+
        aes_string(x=barVarName,y="Percent",fill=colorVarName)+
        geom_bar(position="fill",stat="identity",color="grey85",size=0.25)  +
        scale_y_continuous(breaks=seq(0.1,0.9,.1),minor_breaks=seq(0.02,0.98,0.02),
                           labels = c("0.1"="10","0.2"="20","0.3"="30","0.4"="40",
                                      "0.5"="50","0.6"="60","0.7"="70","0.8"="80",
                                      "0.9"="90"))
    # In case of frequency plot
    } else {
      plot1=plot1+
        aes_string(x=barVarName,y="Frequency",fill=colorVarName) +
        geom_bar(position="stack",stat="identity",color="grey85",size=0.25)
    }
    plot1=plot1+        
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
      theme( panel.grid.minor.y = element_line(colour = "black"))+
      theme( panel.grid.major.y = element_line(colour = "black",size=1))+
      theme(plot.title = element_text( size=rel(relTextSize)))+
      theme(axis.title.x = element_text( size=rel(relTextSize)))+
      theme(axis.title.y = element_text( size=rel(relTextSize)))+
      theme(axis.text.x = element_text( size=rel(relTextSize)))+
      theme(axis.text.y = element_text( size=rel(relTextSize)))+
      theme(legend.title=element_text(size=rel(relTextSize))) +
      theme(legend.text=element_text(size=rel(relTextSize))) +
      labs(title=plotTitle)  
    if (!(substr(colorPlot,1,1)=="c") & !(substr(colorPlot,1,1)=="C")){
      plot1 = plot1 + scale_fill_grey()
    } else {
      plot1 = plot1 + scale_fill_viridis(option=viridis_option,discrete=TRUE)
    }
    # If plotting percentages, then set y scale from 0 to 1
    
    ## Boxplot with means
    cmdTxt = paste("means <- aggregate(Freq ~ ",colorVarName,", df, mean)")
    eval(parse(text = cmdTxt))
    plot2=ggplot(data=df, aes_string(x=colorVarName, y="Freq", fill=colorVarName)) + 
      geom_boxplot() +
      stat_summary(fun.y=mean, colour="darkred", geom="point", 
                   shape=18, size=1,show.legend = FALSE) + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6,size=rel(relTextSize))) +
      theme( panel.grid.major.y = element_line(colour = "white",size=0.8))+
      theme(plot.title = element_text( size=rel(relTextSize)))+
      theme(axis.title.x = element_text( size=rel(relTextSize)))+
      theme(axis.title.y = element_text( size=rel(relTextSize)))+
      theme(axis.text.x = element_text(size=rel(relTextSize)))+
      theme(axis.text.y = element_text(size=rel(relTextSize)))+
      theme(legend.title=element_text(size=rel(relTextSize))) +
      theme(legend.text=element_text(size=rel(relTextSize))) +
      xlab(NULL) +
      ylab("Percentage values")
    # If percentages, then add y scale from 0 to 1
    if (substr(pctPlot,1,1)=="p" | substr(pctPlot,1,1)=="P"){
      plot2=plot2 +
        scale_y_continuous(breaks=seq(0.1,0.9,.1),minor_breaks=seq(0.02,0.98,0.02), 
                           labels = c("0.1"="10","0.2"="20","0.3"="30","0.4"="40",
                                      "0.5"="50","0.6"="60","0.7"="70","0.8"="80",
                                      "0.9"="90"))
    }
    if (!(substr(colorPlot,1,1)=="c") & !(substr(colorPlot,1,1)=="C")){
      plot2 = plot2 + scale_fill_grey()
    } else{
      plot2 = plot2 + scale_fill_viridis(option=viridis_option,discrete=TRUE)
    }
    
    ## Create correlation plot
    plot3=ggplot(data=corMx, aes(Var1,Var2, fill=value))+
      geom_raster() +
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+
      coord_fixed(ratio=0.7) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       hjust = 1,size=rel(relTextSize))) +
      xlab(NULL)+ylab(NULL) +
      theme(panel.grid.major = element_line(color="black")) +
      scale_y_discrete(position = "right")

    if (plotToFile==TRUE) {
      
      # Create directory for completed plots
      dirTmp = paste(outputDirectory,"/plot2Var/",sep="")
      dir.create(dirTmp,showWarnings = FALSE)
      ## Put the 3 plots together and send to a file
      plotName = paste(dirTmp,colorVarName,"by",barVarName,sep="") 
      # Create new file if the file already exists
      while (file.exists(paste(plotName,".pdf",sep=""))) {
        plotName = paste(plotName,"_",sep="")
      }
      plotName = paste(plotName,".pdf",sep="")
      pdf(plotName, width = 11, height = 8.5)
      
      p3 <- grid.arrange(plot1,arrangeGrob(
        plot2 + theme(legend.position="none"),
        plot3,
        ncol=2,widths=c(2,2)),
        nrow=2,heights=c(2, 1))
      print(p3)
      dev.off()
      
      # Plot bar plot separately
      dirTmp = paste(outputDirectory,"/plot2Var/barPlots/",sep="")
      dir.create(dirTmp,showWarnings = FALSE)
      plotName = paste(dirTmp,colorVarName,"by",barVarName,sep="")
      while (file.exists(paste(plotName,".pdf",sep=""))) {
        plotName = paste(plotName,"_",sep="")
      }
      plotName = paste(plotName,".pdf",sep="")
      pdf(plotName, width = 11, height = 8.5)
      print(plot1)  
      dev.off()
      
      # Plot box plot separately
      dirTmp = paste(outputDirectory,"/plot2Var/boxPlots/",sep="")
      dir.create(dirTmp,showWarnings = FALSE)
      plotName = paste(dirTmp,
                       colorVarName,"by",barVarName,sep="")
      while (file.exists(paste(plotName,".pdf",sep=""))) {
        plotName = paste(plotName,"_",sep="")
      }
      plotName = paste(plotName,".pdf",sep="")
      pdf(plotName, width = 11, height = 8.5)
      print(plot2)  
      dev.off()
      
      # Plot correlation matrix separately
      dirTmp = paste(outputDirectory,"/plot2Var/correlationPlots/",sep="")
      dir.create(dirTmp,showWarnings = FALSE)
      plotName = paste(dirTmp,colorVarName,"by",barVarName,sep="")
      while (file.exists(paste(plotName,".pdf",sep=""))) {
        plotName = paste(plotName,"_",sep="")
      }
      plotName = paste(plotName,".pdf",sep="")
      pdf(plotName, width = 11, height = 8.5)
      print(plot3)  
      dev.off()
    } 
    if (plotToWindow==TRUE) { ## Print to window
      print(plot3)
      print(plot2)
      print(plot1)
    }
    plot2VarNames = c(colorVarName,barVarName)
    return(plot2VarNames)
  }  
  } ## End of 2-variable plot routine
}