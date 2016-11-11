require(ggplot2)
require(grid) #ESSENTIAL TO SPECIFY THIS HERE, OTHERWISE SOME GGPLOT2 FEATURES WON'T WORK!
require(plyr)

### START OF FUNCTIONS

# Tick mark lengths and graph dimensions -------------------------------------------------------

## Calculate tick mark lengths
axistickcalc = function(xlimits, ylimits,custommultiplier,graphproportion){
  
  yaxissize = ylimits[2] - ylimits[1]
  xaxissize = xlimits[2] - xlimits[1]
  
  xticklength = yaxissize * custommultiplier  
  xyratio = yaxissize / xaxissize
  
  ####Accounting for  proportions of graph in number terms, then the physical
  ####proportions of the graph 
  yticklength = (xticklength / xyratio) * graphproportion 
  ticklengths = c(xticklength, yticklength)  
  return(ticklengths) 
}

tickdataframe2 = function (graphno, xlimit, ylimit,custommultiplier,xbreaks,ybreaks,graphdims,
                          graphcentres,groupvar,groupvalue,xlabel,ylabel,firsttimetreat){
  ybreaklengthasones = (ave(ybreaks, FUN=length)/ave(ybreaks, FUN=length))
  
  breaksxstart = 
    c(ybreaklengthasones * xlimit[1],xbreaks,ybreaklengthasones * xlimit[2],xbreaks)
}

# Create a data frame that contains all the position data for the internal ticks
tickdataframe = function (graphno, xlimit, ylimit,custommultiplier,xbreaks,ybreaks,graphdims,
                          graphcentres,groupvar,groupvalue,xlabel,ylabel,firsttimetreat){
   
  graphprop = graphdims[2] / graphdims[1]  
  yaxissize = ylimit[2] - ylimit[1]
  xaxissize = xlimit[2] - xlimit[1]
  xticklength = yaxissize * custommultiplier  
  xyratio = yaxissize / xaxissize
  
  ####Accounting for  proportions of graph in number terms, then the physical
  ####proportions of the graph 
  yticklength = (xticklength / xyratio) * graphprop 
  ticklengths = c(xticklength, yticklength)
  
  #The following line is a complex way to make a vector of length ybreaks and
  #value x lower limit
  xbreaklengthasones = (ave(xbreaks, FUN=length)/ave(xbreaks, FUN=length))
  ybreaklengthasones = (ave(ybreaks, FUN=length)/ave(ybreaks, FUN=length))
  
  breaksxstart = 
    c(ybreaklengthasones * xlimit[1],xbreaks,ybreaklengthasones * xlimit[2],xbreaks)
  breaksxend = 
    c((ybreaklengthasones * xlimit[1] + ticklengths[2]),xbreaks,
      (ybreaklengthasones * xlimit[2] - ticklengths[2]),xbreaks)
  breaksystart = 
    c(ybreaks,xbreaklengthasones * ylimit[2],ybreaks,xbreaklengthasones * ylimit[1])

  if (is.na(ylabel[1])) (ylabels = breaksystart)
  else (ylabels = c(ylabel,rep(NA, len = length(xbreaks)),ylabel,rep(NA, len = length(xbreaks))))
  #ylabels = as.character(ylabels)
  
  if (is.na(xlabel[1])) (xlabels = breaksxstart)
  else (xlabels = c(rep(NA, len = length(ybreaks)),xlabel,rep(NA, len = length(ybreaks)),xlabel))
  #xlabels = as.character(xlabels)  
  
  breaksyend = 
    c(ybreaks,xbreaklengthasones * (ylimit[2] - ticklengths[1]),
      ybreaks,xbreaklengthasones * (ylimit[1] + ticklengths[1]))
  axisno = c(ybreaklengthasones,xbreaklengthasones*2,ybreaklengthasones*3,xbreaklengthasones*4)
  
  ### Next, the function finds the position of the x and y breaks in the graph in terms of
  ### cm from the bottom left corner of the page. This data is used to create the axis
  ### labels later in the code
  
  graphxleftcm = (graphcentres$graphxcentre[graphcentres$graphnum == graphno] - (graphdims[1]/2))
  farrightscaledistfromleft = xlimit[2] - xlimit[1]  
  breakscaledistfromleft = c(breaksxstart - xlimit[1])
  xbreakcmpos = c(graphxleftcm + ((breakscaledistfromleft/farrightscaledistfromleft) * graphdims[1]))
    
  graphybotcm = (graphcentres$graphycentre[graphcentres$graphnum == graphno] - (graphdims[2]/2))
  fartopscaledistfrombot = ylimit[2] - ylimit[1]  
  breakscaledistfrombot = c(breaksystart - ylimit[1])
  ybreakcmpos = c(graphybotcm + ((breakscaledistfrombot/fartopscaledistfrombot) * graphdims[2]))
    
  #This ensures that the ticks have a solid linetype rather than dotted. 
  group = rep(paste(groupvalue),length(breaksyend))
  ncharlabel = nchar(xlabels)
    
  ticks = data.frame(breaksxstart,breaksxend,breaksystart,breaksyend,group,
                     xbreakcmpos,ybreakcmpos,axisno,ylabels,xlabels,ncharlabel,graphxleftcm)
  ticks$timetreatno = rep(firsttimetreat,length(ticks$breaksxstart))
  colnames(ticks)[5] = groupvar
  
  return(ticks)
}

# Calculate the dimensions of the graphs and related viewppoints
calcgraphdims = function(graphno, graphposdata, layout, graphgroupdims, graphdims,pagedims){
  # Looking at graph dimensions and positions  
  
  leftpagemarg = graphposdata$leftpagemarg[graphposdata$graphnum == graphno]
  rightpagemarg = graphposdata$rightpagemarg[graphposdata$graphnum == graphno]
  toppagemarg = graphposdata$toppagemarg[graphposdata$graphnum == graphno]
  botpagemarg = graphposdata$botpagemarg[graphposdata$graphnum == graphno]

  graphxmargin = leftpagemarg + rightpagemarg
  graphymargin = toppagemarg + botpagemarg

  totalleftpagemarg = sum(graphposdata$leftpagemarg[graphposdata$graphvertpos == 1],na.rm = true)
  totalrightpagemarg = sum(graphposdata$rightpagemarg[graphposdata$graphvertpos == 1],na.rm = true)
  totaltoppagemarg = sum(graphposdata$toppagemarg[graphposdata$graphhoripos == 1],na.rm = true)
  totalbotpagemarg = sum(graphposdata$botpagemarg[graphposdata$graphhoripos == 1],na.rm = true)
  
  leftgapmarg = graphposdata$leftgapmarg[graphposdata$graphnum == graphno]
  rightgapmarg = graphposdata$rightgapmarg[graphposdata$graphnum == graphno]
  topgapmarg = graphposdata$topgapmarg[graphposdata$graphnum == graphno]
  botgapmarg = graphposdata$botgapmarg[graphposdata$graphnum == graphno]  


### The x and y centres of the graph are calculated from margin sizes and graph
### dimensions
############ x
  gapmargtotal = 2*(leftgapmarg + rightgapmarg)
  graphxlayoutpos = graphposdata$graphhoripos[graphposdata$graphnum == graphno]
  graphleftgapmarg = ((graphxlayoutpos - 1)*(gapmargtotal))

  rightedgegraphsizealone = (graphdims[1] * graphxlayoutpos)  
  rightedgexpoint = rightedgegraphsizealone + totalleftpagemarg + graphleftgapmarg

  halfgraphwidth = (0.5*graphdims[1])
  graphpagexcentre = rightedgexpoint - halfgraphwidth

############# y
  
  yspacetotal = (2*botgapmarg)#(topgapmarg + botgapmarg)
  graphylayoutpos = graphposdata$graphvertpos[graphposdata$graphnum == graphno]
  graphbotyspacemarg = ((graphylayoutpos - 1)*(yspacetotal))

  topedgegraphsizealone = (graphdims[2] * graphylayoutpos)
  topedgeypoint = topedgegraphsizealone + totalbotpagemarg + graphbotyspacemarg
    
  halfgraphheight = (0.5*graphdims[2])    
  graphpageycentre = topedgeypoint  - halfgraphheight
  
#Calculating viewport dimensions and positions

  pagexsize = pagedims[1]
  nocolumns = layout[2]
  centreofleftviewpoint = (pagexsize/nocolumns)/2

  norows = layout[1]
  graphcolumnno = graphposdata$graphhoripos[graphposdata$graphnum == graphno]

  graphvpxcentre = (centreofleftviewpoint * graphcolumnno +
                  centreofleftviewpoint * (graphcolumnno - 1))
    
  graphvpycentre = graphpageycentre + (0.5*toppagemarg) - (0.5*botpagemarg)   

  graphvpxsize = pagexsize / nocolumns
  graphvpysize = graphdims[2] + graphymargin
  
  graphvpsize = c(graphvpxsize, graphvpysize) 
  
  graphpos = c(graphpagexcentre, graphpageycentre, graphdims[1], graphdims[2],
               graphvpxcentre, graphvpycentre, graphvpxsize, graphvpysize)
  
  return (graphpos)
}

## Add the data derived from the calcgraphdims function into a data frame
returnasdataframe = function(graphno, graphposdata, graphpos) {
  graphposdata$graphxcentre[graphposdata$graphnum == graphno] = graphpos[1]
  graphposdata$graphycentre[graphposdata$graphnum == graphno] = graphpos[2]
  graphposdata$graphxsize[graphposdata$graphnum == graphno] = graphpos[3]
  graphposdata$graphysize[graphposdata$graphnum == graphno] = graphpos[4]
  graphposdata$graphvpxcentre[graphposdata$graphnum == graphno] = graphpos[5]
  graphposdata$graphvpycentre[graphposdata$graphnum == graphno] = graphpos[6]
  graphposdata$graphvpxsize[graphposdata$graphnum == graphno] = graphpos[7]
  graphposdata$graphvpysize[graphposdata$graphnum == graphno] = graphpos[8]
  
  return(graphposdata)
}

calcse = function (x,y) {
  se = x/sqrt(y)
  
  return(se)
}

# Data selection functions ------------------------------------------------

graphdatainput = function(inputframe,xvar,yvar,sevar,ysimvar,groupvar,treatno,outputframe){
  ### NOTE: data type can be of form "average" or "raw". Ave works with all geoms that can use 
  ### averaged data, e.g. points, bars. Whereas boxplot will use the raw data.
    rm(outputframe)
  
    outputframe = data.frame(1:(length(inputframe[,1])))
    outputframe$xvar = xvar
    outputframe$yvar = yvar
    outputframe$yvarse = sevar
    
    outputframe$ysimvar = ysimvar
    outputframe$group = groupvar
    outputframe$timetreatno = inputframe$timetreatno
    outputframe$treatno = inputframe$treatno
    outputframe[1] = NULL
    
    outputframe$lines = inputframe$lines
    outputframe$fills = inputframe$fills
    outputframe$shape = inputframe$shape
    outputframe$outlines = inputframe$outlines
        
  return(outputframe)  
}

# Viewport and graph printing functions -----------------------------------

# Extract viewpoint dimensions for the graph
calcviewport = function (graphno,graphposdata) {
  vp = viewport(x = unit(graphposdata$graphvpxcentre[graphposdata$graphnum == graphno],"cm"), 
                y = unit(graphposdata$graphvpycentre[graphposdata$graphnum == graphno],"cm"), 
                width = (graphposdata$graphvpxsize[graphposdata$graphnum == graphno]),
                height = (graphposdata$graphvpysize[graphposdata$graphnum == graphno]),
                default.units = "cm")
  return (vp)
}

# Paste a red rectangle delineating the outline of the graph viewpoint onto the page
drawvprect = function (graphno,graphposdata) {
  rect = grid.rect(x = unit(graphposdata$graphvpxcentre[graphposdata$graphnum == graphno],"cm"), 
                y = unit(graphposdata$graphvpycentre[graphposdata$graphnum == graphno],"cm"), 
                width = (graphposdata$graphvpxsize[graphposdata$graphnum == graphno]),
                height = (graphposdata$graphvpysize[graphposdata$graphnum == graphno]),
                default.units = "cm",gp=gpar(col="red",fill="transparent"))
  return (rect)
}

# Past a green rectangle delineating the outline of the graph itself onto the page
drawgraph = function (graphno,graphposdata) {
  rect = grid.rect(x = unit(graphposdata$graphxcentre[graphposdata$graphnum == graphno],"cm"), 
                   y = unit(graphposdata$graphycentre[graphposdata$graphnum == graphno],"cm"), 
                   width = (graphposdata$graphxsize[graphposdata$graphnum == graphno]),
                   height = (graphposdata$graphysize[graphposdata$graphnum == graphno]),
                   default.units = "cm",gp=gpar(col="green",fill="transparent"))
  return (rect)
}

# Print the graph onto the page
printgraph = function (graphno,graphname,graphposdata) {
  print(graphname, 
        vp = viewport(x = unit(graphposdata$graphvpxcentre[graphposdata$graphnum == graphno],"cm"), 
                      y = unit(graphposdata$graphvpycentre[graphposdata$graphnum == graphno],"cm"), 
                      width = (graphposdata$graphvpxsize[graphposdata$graphnum == graphno]),
                      height = (graphposdata$graphvpysize[graphposdata$graphnum == graphno]),
                      default.units = "cm"))  
}

# Add the letter label to the graph
annotateletters = function (graphno, ylimit, text, labeltextsize, labelyposperc, xpos) {
  annotate("text", x = xpos, y = (ylimit[1] + (labelyposperc*(ylimit[2] - ylimit[1]))), 
           label = text, size = labeltextsize)
}

# Prints a title for the whole graph set
printtitle = function(expression,pagedims,rotation,fontsize, textdist, fontfam){
      xposition = pagedims[1]/2
      yposition = pagedims[2] - textdist
      
      grid.text(expression,x = unit(xposition,"cm"), 
                y = unit(yposition,"cm"), rot=rotation,gp=gpar(fontsize=fontsize, fontfamily=fontfam))
}

# Prints text to somewhere on the page
printaxistext = function(graphno, true, whichaxis, textlevel,expression,graphdata,
                         rotation,fontsize, textdist,fontfam){
  if (true == T) {
    if (whichaxis == 1) { # left y axis
  xposition = with(graphdata, graphxcentre[graphnum == graphno] - 
                    (0.5*graphxsize[graphnum == graphno]) - textdist - (0.05*textlevel*fontsize))
  yposition = with(graphdata, graphycentre[graphnum == graphno])
  
  grid.text(expression,x = unit(xposition,"cm"), 
            y = unit(yposition,"cm"), rot=rotation,gp=gpar(fontsize=fontsize,fontfamily=fontfam))
    }
    if (whichaxis == 2) { # top x axis
      xposition = with(graphdata, graphxcentre[graphnum == graphno])
      yposition = with(graphdata, graphycentre[graphnum == graphno] + 
        (0.5*graphysize[graphnum == graphno]) + (textdist*1.3))
      
      grid.text(expression,x = unit(xposition,"cm"), 
                y = unit(yposition,"cm"), rot=rotation,gp=gpar(fontsize=fontsize,fontfamily=fontfam))
    }
    if (whichaxis == 3) { # right y axis
      xposition = with(graphdata, graphxcentre[graphnum == graphno] + 
        (0.5*graphxsize[graphnum == graphno]) + textdist + (0.05*textlevel*fontsize))
      yposition = with(graphdata, graphycentre[graphnum == graphno])
      
      grid.text(expression,x = unit(xposition,"cm"), 
                y = unit(yposition,"cm"), rot=rotation,gp=gpar(fontsize=fontsize,fontfamily=fontfam))
    }
    if (whichaxis == 4) { # bottom x axis
      xposition = with(graphdata, graphxcentre[graphnum == graphno])
      yposition = with(graphdata, graphycentre[graphnum == graphno] - 
        (0.5*graphysize[graphnum == graphno]) - (textdist*1.3))
      
      grid.text(expression,x = unit(xposition,"cm"), 
                y = unit(yposition,"cm"), rot=rotation,gp=gpar(fontsize=fontsize,fontfamily=fontfam))
    }
  }
}

# Print axis labels (hopefully) at the correct place
printaxislabel = function(graphno, true, axisno, graphdata,fontsize, textdist, xrounded, yrounded,
                          rotation,xmod,fontfam){
  if (true == T) {
  
  if (axisno == 1){ #left axis
  graphdatacut = graphdata[graphdata$axisno == 1,]
  adjustedx = graphdatacut$xbreakcmpos - textdist + xmod
  xposition = adjustedx
  yposition = graphdatacut$ybreakcmpos
  expression = format(graphdatacut$ylabels,nsmall = yrounded)
  just = "right"
  }
  else if (axisno == 2){ #top axis
  graphdatacut = graphdata[graphdata$axisno == 2,]
  adjustedy = graphdatacut$ybreakcmpos + textdist
  xposition = graphdatacut$xbreakcmpos + xmod
  yposition = adjustedy
  expression = format(graphdatacut$xlabels,nsmall = xrounded)
  just = "centre"
  }
  else if (axisno == 3){ #right axis
  graphdatacut = graphdata[graphdata$axisno == 3,]
  adjustedx = graphdatacut$xbreakcmpos + textdist + xmod
  xposition = adjustedx
  yposition = graphdatacut$ybreakcmpos
  expression = format(graphdatacut$ylabels,nsmall = yrounded)
  just = "left"
  }
  else if (axisno == 4){ #bottom axis
  graphdatacut = graphdata[graphdata$axisno == 4,]
  adjustedy = graphdatacut$ybreakcmpos - textdist 
  xposition = graphdatacut$xbreakcmpos + xmod
  yposition = adjustedy
  if(rotation == 0) (just = "centre")
  else (just = "right")
  expression = format(graphdatacut$xlabels,nsmall = xrounded, justify = just)
  }    
  grid.text(expression,x = unit(xposition,"cm"), 
  y = unit(yposition,"cm"), rot=rotation,gp=gpar(fontsize=fontsize,fontfamily=fontfam), just = just)
  }
}

# GGplot2 functions -------------------------------------------------------

## Extracts the ggplot2 margins for each graph from the default margin data frame
extractggmargins = function(graphno, data) {
  
  graphmarginunits = c(data$toppagemarg[data$graphnum == graphno],
                       data$righttotmarg[data$graphnum == graphno],
                       data$botpagemarg[data$graphnum == graphno],
                       data$lefttotmarg[data$graphnum == graphno])
  
  ggplot2marginopt = theme(plot.margin = unit(c(graphmarginunits),"cm"))
  
  return (ggplot2marginopt)
}

# Get the GGplot2 axes to focus on the user-chosen graph limits
definegglimitsscale = function (xlimits,ylimits,xbreaks,ybreaks) {
  list(coord_cartesian(xlim = xlimits, ylim = ylimits, expand = FALSE),
       scale_x_continuous(breaks = xbreaks),
       scale_y_continuous(breaks = ybreaks))
}


## This function can be used if you have already calculated average and se in another function
calcminmaxerrorsimple = function (dataout) {
  dataout$semax = dataout$yvar + dataout$yvarse
  dataout$semin = dataout$yvar - dataout$yvarse
  return(dataout)
}

### Create graph error bars, axis ticks and graph points
definegeomerrbarticks = function (data, errwidth,tickdata,showerrs,geom, showleg) {
  
  xstartname = colnames(tickdata[1])
  xendname = colnames(tickdata[2])
  ystartname = colnames(tickdata[3])
  yendname = colnames(tickdata[4])
  
  data$semax = data$yvar + data$yvarse
  data$semin = data$yvar - data$yvarse
  
  if (geom == "point" | geom == "line" | geom == "area" | geom == "rect") {
    #all the above data types work with AVERAGED data
    list(  
      #works with "point","line","area"
      ###This segment control governs the axis ticks
      if (showerrs == TRUE){
        geom_errorbar(aes_string(ymax = "semax",ymin= "semin"), 
                      width = errwidth, data = data,linetype = 1, colour = "grey30")},
      
      geom_segment(aes_string(x = paste(xstartname),
                              xend = paste(xendname),
                              y = paste(ystartname),                            
                              yend = paste(yendname)), data=tickdata, 
                   show.legend = FALSE, 
                   linetype = 1),
      if (showleg == TRUE) {
      # stat_summary(fun.y = mean, geom= paste(geom),linetype = 1,colour = 1)
        stat_summary(fun.y = mean, geom= paste(geom),colour = 1)
      }
	    else {
	    #stat_summary(fun.y = mean, geom= paste(geom),linetype = 1,colour = 1, show.legend = FALSE)
	  stat_summary(fun.y = mean, geom= paste(geom),colour = 1, show.legend = FALSE)
	  } 	   
    )
  }
  else if (geom == "bar") {
    #all the above data types work with AVERAGED data
    
    data$semax = data$yvar + data$yvarse
    data$semin = data$yvar - data$yvarse
    
    list(  
      #works with "point","line","area"
      ###This segment control governs the axis ticks
      geom_segment(aes_string(x = paste(xstartname),
                              xend = paste(xendname),
                              y = paste(ystartname),                            
                              yend = paste(yendname)), data=tickdata, 
                   show.legend = FALSE, 
                   linetype = 1),
           
      if (showleg == TRUE) {
      stat_summary(fun.y = mean, geom= paste(geom),linetype = 1,colour = 1,position = position_dodge())
      }
	else{
	stat_summary(fun.y = mean, geom= paste(geom),linetype = 1,colour = 1,position = position_dodge(), show.legend = FALSE)
	},
      
      if (showerrs == TRUE){
        geom_errorbar(aes_string(ymax = "semax",ymin= "semin"), 
                      width = errwidth, data = data,linetype = 1,position = position_dodge())}
    )
  }
  else if (geom == "boxplot") {
    list(  
      #works with "boxplot", which needs the RAW data
      
      if (showleg == TRUE) {
       geom_boxplot(width = 3.5)
      }
	else{
	geom_boxplot(width = 3.5, show.legend = FALSE)
	},
      
      #geom_point(aes_string(colour = paste("group")), fill = NA, size = 2, shape = 21, show.legend = FALSE,
      #           position = position_jitter(width = 2)), #alpha = 0.5 (translucency not supported
      # by postscript)
      
      ###This segment control governs the axis ticks
      geom_segment(aes_string(x = paste(xstartname),
                              xend = paste(xendname),
                              y = paste(ystartname),                            
                              yend = paste(yendname)), data=tickdata, 
                   show.legend = FALSE, 
                   linetype = 1)
	)
  }
  else if (geom == "horizbar" ) {
    list(
      geom_segment(aes_string(x = paste(xstartname),
                              xend = paste(xendname),
                              y = paste(ystartname),                            
                              yend = paste(yendname)), data=tickdata, 
                   show.legend = FALSE, 
                   linetype = 1))
  }
  else if (geom == "NA" ) {
    list(
      if (showerrs == TRUE){
        geom_errorbar(aes_string(ymax = "semax",ymin= "semin"), 
                      width = errwidth, data = data,linetype = 1, colour = "grey30")},

      geom_segment(aes_string(x = paste(xstartname),
                              xend = paste(xendname),
                              y = paste(ystartname),                            
                              yend = paste(yendname)), data=tickdata, 
                   show.legend = FALSE, 
                   linetype = 1))
  }
}

# Gives the option "don't show legend" to the ggplot2 graph
makenolegend = function (fills, lines, shapes, legendbreak, lbreakorder, lbreaklimit){
  cl = colors()
  list(
    scale_fill_manual("",values = cl[fills],breaks=legendbreak, limits = lbreaklimit),
    #scale_fill_brewer("",type = "seq",palette = "Greys", breaks=legendbreak, limits = legendbreak),#scale_fill_grey("",start = 1, end = 0.4), 
       scale_linetype_manual("", values=lines, breaks = lbreakorder, limits = lbreaklimit),
       scale_shape_manual("", values = shapes, breaks = lbreakorder, limits = lbreaklimit),
       theme(legend.position = "none"))
 }

 #Gives the option "show legend" to the ggplot2 graph
makewithlegend = function (fills, lines, shapes, legendbreak, lbreakorder,lbreaklimit){
  cl = colors()
  list(scale_fill_manual("",values = cl[fills],breaks=lbreakorder, limits =lbreaklimit),
    #scale_fill_brewer("",type = "seq",palette = "Greys", breaks = legendbreak, limits = legendbreak),
       #c(grouporder[1] = fills[1],grouporder[2] = fills[2])),#scale_fill_grey("",start = 1, end = 0.4),
       scale_linetype_manual("",values=lines, breaks = lbreakorder, limits = lbreaklimit),
       scale_shape_manual("", values = shapes, breaks = lbreakorder, limits = lbreaklimit))
}

 #Gives the option "show legend, just points and shapes" to the ggplot2 graph
makewithlegendpoint = function (fills, shapes, legendbreak){
  #attr(fills,"names") = legendbreaks
  list(scale_fill_brewer("",type = "seq",palette = "Greys", breaks=legendbreak, limits=legendbreak),#scale_fill_manual("",values = c(fills),breaks=legendbreaks),#c(grouporder[1] = fills[1],grouporder[2] = fills[2])),#scale_fill_grey("",start = 1, end = 0.4),
       scale_shape_manual("", values = shapes))
}

 #Gives the option "show legend, just lines" to the ggplot2 graph
makewithlegendline = function (lines, legendbreaks){
  list(scale_linetype_manual("", values=lines))
}

#Gives the option "show legend" to the ggplot2 graph
makewithlegendbar = function (fills, shapes, legendbreak){
  #attr(fills,"names") = legendbreaks
  list(scale_fill_brewer("",type = "seq",palette = "Greys", breaks = legendbreak, limits = legendbreak),#scale_fill_manual("",values = fills,breaks=legendbreaks),#c(grouporder[1] = fills[1],grouporder[2] = fills[2])),#scale_fill_grey("",start = 1, end = 0.4),
       scale_shape_manual("", values = shapes, breaks = legendbreak, limits = legendbreak))
}

#colourlist = c("Control" = "white", "Un-amended" = "lightgrey", "Amended" = "grey50")
#attr(colourlist)
#paste(legendbreaks)
#attr(fills,"names") = legendbreaks
#fills

# Useful matching functions -----------------------------------------------

#intersect <- function(x, y) y[match(x, y, nomatch = 0)]
#intersect # the R function in base, slightly more careful
#matchnos = as.integer(as.character(intersect(fieldexp1graphdata$timetreatno, n2operc$timetreatno)))

#(x <- c(sort(sample(1:20, 9)),NA))
#(y <- c(sort(sample(3:23, 7)),NA))
# union(x, y)
# intersect(x, y)
# setdiff(x, y)
# setdiff(y, x)
# setequal(x, y)
# 
# is.element(x, y)# length 10
# is.element(y, x)# length  8

#################################
### END OF FUNCTIONS