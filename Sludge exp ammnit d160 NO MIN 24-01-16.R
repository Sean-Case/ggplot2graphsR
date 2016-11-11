require(ggplot2)
require(grid) #ESSENTIAL TO SPECIFY THIS HERE, OTHERWISE SOME GGPLOT2 FEATURES WON'T WORK!
require(plyr)
require(extrafont) # https://cran.r-project.org/web/packages/extrafont/README.html
require(extrafontdb)
require(Rttf2pt1)
# font_import()
# loadfonts(device = "postscript")

#ggplot2 colour map: http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# User entry --------------------------------------------------------------
setwd("C:\\Users\\s_cas\\Dropbox\\Sludge experiment 2013\\Sean incubation\\Data\\Incubation data\\Ammonium nitrate")
#setwd("D:\\Dropbox\\Sludge experiment 2013\\Sean incubation\\Data\\Incubation data\\Ammonium nitrate")

#getwd()
{
a=read.table(file="ammnitsludgeexp29-11-15.csv",sep=",",header=TRUE,fill = TRUE,stringsAsFactors = FALSE)
b=read.table(file="mineralisendata29-11-15.csv",sep=",",header=TRUE,fill = TRUE,stringsAsFactors = FALSE)

axismins = data.frame(1)
axislimits = data.frame(c(1:2))
printaxes = data.frame(1)
rounddata = data.frame(1)

# OVERALL LAYOUT
#the overall layout of the graphs (a combination of up to 10 graphs)
totalgraphs = 4
## Vertical, Horizontal layout
layout = c(2,2)

# Graph position codes determine margin options: 1 = topleft, 2 = midleft, 3 = midleft2,
# 4 = midleft3, 5 = botleft, 6 = topright, 7 is midright1, 8 is midright2, 9 is midright3, 10 is botright,
# 11 is top centre, 12 is centre 1, 13 is centre 2, 14 is centre 3, 15 is bot centre
chosengraphpositions = c(1,5,6,10)
#Need one graph at vertpos 1 and horipos 1 (bottom left) for the functions to work
chosenvertpos = c(2,1,2,1)
chosenhoripos = c(1,1,2,2)
#The chosen names of the graphs (a number) - defaults to 1 to totalgraphs
chosengraphnum = c(1,2,3,4)

##### GRAPH NAMES
####            1    2    3   4     5    6    7    8    9   10
textlabels = c("a.","c.","b.","d.","g.","h.","e.","f.","a.","b.")

# PAGE DIMENSIONS
pageheight = ((29.7 - 4) * (0.55)) # 29.7 = Size of A4 - 2.5cm margin (assumed), then 2/3 height of page, then * 1.5 figure size for SSSAJ guideline
pagewidth = (17.4 + 1.2) # 174 mm = The width given by ESPR guide. # 21 = Size of A4
pagedims = c(pagewidth,pageheight)
rm(pageheight,pagewidth)

### MARGIN OVERALL OPTIONS
# Outer margins
outerpagemargin = 2 # Margin at edge of page in cm
topoutermargin = 0 # Just in case you need a custom title at top
singletopmarg = outerpagemargin # cm at top of graphs, set to custom value if only one row of graphs, otherwise set to same as yspace
labelbotmarg = 0 # The margin height underneath the graph. Useful to increase if you have e.g. diagonal labels

#Outer margins for right or left side
extraright = 1.5
extraleft = extraright # 1.5

# Margins between graphs
yspace = 0.5 # Distance between graphs in y direction in cm
halfymargin = yspace/2
normalymargins = yspace
thinymargins = 0

xmarginbetweengraphs = 1 # Margin in between column 1 and 2. If you have just one column, set it to 0
halfcentremargin = xmarginbetweengraphs/2

############## FONTS
windowsFonts(sans=windowsFont("Arial")) # For the WindowsFonts function you need the variable name to be exactly the same as the postscript font, and the name in quotes to be exactly # the same as the Windows font
windowsFonts(Times=windowsFont("TT Times New Roman"))
chosenfont = "sans" # Discussion about using custom fonts with ggplot2 here: http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html # If you want to output to .ps files instead of .pdf, use: # loadfonts(device="postscript")

############## LABELS
labelxpos = 155 ## X position in units that graph is in
labelyposperc = 0.93 # Position of the label up the y axis as a percentage
labeltextsize = 5
legendtextsize = 10
legendpos = c(1.28,0.7) # Position of legend in plots as proportion of total graph width / height. x then y

### AXES
# All
axisfontsize = 12
textdist = 0.75 # Distance centre of text line from the y axis in cm
xtitletextdist = textdist*(4/5)
axislabeldist = 0.15; yaxislabeldist = 0.15 # distance of the axis numbers from the axis in cm
xaxislabeldist = 0.25
titlepos = 3 # Position of graph title in units along xaxis
yaxisrotation = 90 #Y axis label rotation
tickmultiplier = 0.02 # Axis ticklength as a proportion of the graph width

##### X axis limits and breaks, common for all graphs
axislimits$xlimit = c(-7, 167)
xbreaks = c(0, 40, 80, 120, 160)
xaxissize = axislimits$xlimit[2] - axislimits$xlimit[1]
xaxistitle = expression("Incubation day")
rounddata$xrounded = 0
xlabel = NA
errwidth = 0.04*xaxissize # Error bar widths in units of the x axis of the graph

############## GENERAL GRAPH OPTIONS
############## Name of grouping variables
groupvarname = "group" ## the default group var name is the name of the 1st (lowest level) group
defaultgroupvarname = "C" ## the 'lowest' treatment name in the dataset
firsttimetreat = 1

legendbreaks = data.frame(unique(a$treatno))
legendbreaks$treatname = unique(a$newtreatname)
colnames(legendbreaks) = c("treatno","treatname")

### FILLS LINES SHAPES
# Using the R black and white colour palette from here: http://research.stowers-institute.org/mcm/efg/R/Color/Chart/index.htm
# On this page, no 261 = pure black, 361 = white
lightestcolour = 361
darkestcolour = 291
colournumbers = 0
for (i in 1:(length(legendbreaks$treatname)/2)) {
colournumbers[i] = as.integer((lightestcolour - ((lightestcolour - darkestcolour) * ((i-1)/(length(legendbreaks$treatname)/2)))))
}
defaultfills = data.frame(c(1:length(legendbreaks$treatname)),colournumbers, 
                          length.out = (length(legendbreaks$treatname)))
colnames(defaultfills) = c("no","fill","notreats")

legendbreaks$fills = defaultfills$fill
legendbreaks$lines = rep_len(c(1:6),length(defaultfills$fill)/2)
legendbreaks$shapes = rep_len(c(21:25),length(defaultfills$fill)/2)
legendbreaks$outlines = rep_len("black",length(defaultfills$fill)/2)
}
{
if (is.element(1,chosengraphnum)) {
  
axismins$g1ymin = -10
axislimits$g1ylimit = c(axismins$g1ymin,320) #* 1000
g1ybreaks = c(0, 100, 200, 300) #* 1000
rounddata$firstyrounded = 0
g1ylabel = NA

### titles
g1yaxistitle1 = expression("Ammonium")
g1yaxistitle2 = expression("("*NH[4]^" +"*-N~"mg"*~kg^-1*")")

#the following are for true/false axes variables
printaxes$g1ytitle = T
printaxes$g1xtitle = F
printaxes$g1xaxislab = F
printaxes$g1yaxislab = T
} # Y Axis settings for graph 1 NH4 pF2 graph
if (is.element(2,chosengraphnum)) {
  
axismins$g2ymin = -20
axislimits$g2ylimit = c(axismins$g2ymin,650)
g2ybreaks = c(0, 200, 400, 600)
rounddata$secondyrounded = 0
g2ylabel = NA

### titles  
g2yaxistitle1 = expression("Nitrate")
g2yaxistitle2 = expression("("*NO[3]^" -"*-N~"mg"*~kg^-1*")")
 
#the following are for true/false axes variables
printaxes$g2ytitle = T
printaxes$g2xtitle = T
printaxes$g2xaxislab = T
printaxes$g2yaxislab = T
} # Y Axis settings for graph 2 NO3 pF2 graph
if (is.element(3,chosengraphnum)) {
axismins$g3ymin = -10
axislimits$g3ylimit = c(axismins$g3ymin, 320)
g3ybreaks = c(0, 100, 200, 300)
rounddata$thirdyrounded = 0
  g3ylabel = NA
  
  ### titles
g3yaxistitle1 = expression("") #expression("Ammonium")
g3yaxistitle2 = expression("Ammonium ("*NH[4]^" +"*-N~"mg"*~kg^-1*")")
  
  #the following are for true/false axes variables
printaxes$g3ytitle = F
printaxes$g3xtitle = F
printaxes$g3xaxislab = F
printaxes$g3yaxislab = T
} # Y Axis settings for graph 3 NH4 pF1 graph
if (is.element(4,chosengraphnum)) {
  axismins$g4ymin = -20
  axislimits$g4ylimit = c(axismins$g4ymin, 650)
  g4ybreaks = c(0, 200, 400, 600)
  rounddata$fourthyrounded = 0
g4ylabel = NA
  
g4yaxistitle1 = expression("") #expression("Nitrate")
g4yaxistitle2 = expression("Nitrate ("*NO[3]^" -"*-N~"mg"*~kg^-1*")")
 
  #the following are for true/false axes variables
  printaxes$g4ytitle = F
  printaxes$g4xtitle = T
  printaxes$g4xaxislab = T
  printaxes$g4yaxislab = T
} # Y Axis settings for graph 4 NO3 pF1 graph
if (is.element(5,chosengraphnum)) {
  axismins$g5ymin = -.05
  axislimits$g5ylimit = c(axismins$g5ymin,.6)
  g5ybreaks = c(0, .15, .30, .45, .60)
  rounddata$fifthyrounded = 0
g5ylabel = g5ybreaks*100

g5yaxistitle1 = expression(N~mineralised)
g5yaxistitle2 = expression("(%)")

#the following are for true/false axes variables
printaxes$g5ytitle = T
printaxes$g5xtitle = T
printaxes$g5xaxislab = T
printaxes$g5yaxislab = T
} # Y Axis settings for graph 5 N mineralised % pF2
if (is.element(6,chosengraphnum)) {
  axismins$g6ymin = -.2
  axislimits$g6ylimit = c(axismins$g6ymin,.4)
  g6ybreaks = c(-.2, -.1, 0, .1, .2, .3, .4)
  rounddata$sixthyrounded = 0
g6ylabel = g6ybreaks*100

g6yaxistitle1 = expression(N~mineralised)
g6yaxistitle2 = expression("(%)")

#the following are for true/false axes variables
printaxes$g6ytitle = F
printaxes$g6xtitle = T
printaxes$g6xaxislab = T
printaxes$g6yaxislab = T
} # Y Axis settings for graph 6 N mineralised % pF1
if (is.element(7,chosengraphnum)) {
  axismins$g7ymin = -50 
  axislimits$g7ylimit = c(axismins$g7ymin, 600) 
  g7ybreaks = c(0, 100, 200, 300, 400, 500, axislimits$g7ylimit[2]) 
  rounddata$seventhyrounded = 0
  g7ylabel = NA
  
  g7yaxistitle1 = expression(N~mineralised)
  g7yaxistitle2 = expression("(mg"*~N~kg^-1*")")
  
  #the following are for true/false axes variables
  printaxes$g7ytitle = T
  printaxes$g7xtitle = F
  printaxes$g7xaxislab = F
  printaxes$g7yaxislab = T
} # Y Axis settings for graph 7 N mineralised mg kg pF2
if (is.element(8,chosengraphnum)) {
  axismins$g8ymin = -150
  axislimits$g8ylimit = c(axismins$g8ymin,300)
  g8ybreaks = c(-100, 0, 100, 200, 300)
  rounddata$eighthyrounded = 0
  g8ylabel = NA
  
  g8yaxistitle1 = expression(N~mineralised)
  g8yaxistitle2 = expression("(mg"*~N~kg^-1*")")
  
  #the following are for true/false axes variables
  printaxes$g8ytitle = F
  printaxes$g8xtitle = F
  printaxes$g8xaxislab = F
  printaxes$g8yaxislab = T
} # Y Axis settings for graph 8 N mineralised mg kg pF1
if (is.element(9,chosengraphnum))  {
  
axismins$g9ymin = 0
axislimits$g9ylimit = c(axismins$g9ymin,0.6)
g9ybreaks = c(0, .15, .3, .45, axislimits$g9ylimit[2])
rounddata$ninthyrounded = 2
g9ylabel = NA

g9yaxistitle1 = expression(N~mineralised)
g9yaxistitle2 = expression("(%)")

#the following are for true/false axes variables
printaxes$g9ytitle = T
printaxes$g9xtitle = F
printaxes$g9xaxislab = T
printaxes$g9yaxislab = T
} # Y Axis settings for graph 5 N mineralised % pF2 day 120
if (is.element(10,chosengraphnum)) {
axismins$g10ymin = -.2
axislimits$g10ylimit = c(axismins$g10ymin,.4)
g10ybreaks = c(axismins$g10ymin, -.1, 0, .1, .2, .3, axislimits$g10ylimit[2])
rounddata$tenthyrounded = 2
g10ylabel = NA

g10yaxistitle1 = expression(N~mineralised)
g10yaxistitle2 = expression("(%)")

#the following are for true/false axes variables
printaxes$g10ytitle = F
printaxes$g10xtitle = F
printaxes$g10xaxislab = T
printaxes$g10yaxislab = T
} # Y Axis settings for graph 5 N mineralised % pF1 day 120
}

# Graph data entry -------------------------
{
{
alldataraw = a#[!is.na(alltopdat$C),]
alldataraw$treatno = as.integer(alldataraw$treatno)
alldataraw$timepoint = as.integer(alldataraw$timepoint)
notreats = unique(alldataraw$treatno)
alldataraw$timetreatno = as.integer(paste(alldataraw$timepoint, alldataraw$treatno, sep = ""))

for (i in notreats) {
  alldataraw$fills[alldataraw$treatno == i] = as.character(legendbreaks$fill[legendbreaks$treatno == i])
  alldataraw$lines[alldataraw$treatno == i] = as.integer(legendbreaks$lines[legendbreaks$treatno == i])
  alldataraw$shape[alldataraw$treatno == i] = as.integer(legendbreaks$shapes[legendbreaks$treatno == i])
  alldataraw$outlines[alldataraw$treatno == i] = as.integer(legendbreaks$outlines[legendbreaks$treatno == i])
}

# CREATING AVERAGED DATA DATA FRAMES FOR EACH GRAPH

} # CREATING RAW DATA FRAMES FOR NH4 NO3 GRAPHS
{  
  averaged = data.frame(rep(1,length(unique(alldataraw$timetreatno))))
  averaged$treatno = as.integer(tapply(alldataraw$treatno,alldataraw$timetreatno,mean))
  averaged$treatname = as.character(factor(averaged$treatno, levels = c(notreats), labels = rep(unique(alldataraw$newtreatname),2)))
  averaged$timepoint = as.integer(tapply(alldataraw$timepoint,alldataraw$timetreatno,mean))
  averaged$timetreatno = as.integer(tapply(alldataraw$timetreatno,alldataraw$timetreatno,mean))
  averaged$day = as.numeric(tapply(alldataraw$day,alldataraw$timetreatno,mean))

  for (i in notreats) {
    averaged$fills[averaged$treatno == i] = as.character(legendbreaks$fills[legendbreaks$treatno == i])
    averaged$outlines[averaged$treatno == i] = as.character(legendbreaks$outlines[legendbreaks$treatno == i])
  }
  
  averaged$lines = as.integer(tapply(alldataraw$lines,alldataraw$timetreatno,mean))
  averaged$shape = as.integer(tapply(alldataraw$shape,alldataraw$timetreatno,mean))
  averaged[,1] = NULL
  
} # Averaged data for amm nit dataframe
{
averaged$nh4cont = as.numeric(tapply(alldataraw$nh4n.mgkg.1,alldataraw$timetreatno,mean)) #* 1000
averaged$nh4contsd = as.numeric(tapply(alldataraw$nh4n.mgkg.1,alldataraw$timetreatno,sd)) #* 1000
averaged$nh4contcount = as.integer(tapply(alldataraw$nh4n.mgkg.1,alldataraw$timetreatno,length))
averaged$nh4contse = calcse(averaged$nh4contsd,averaged$nh4contcount)

} # FIRST GRAPH - NH4 contents pF 2
{
## Need to create another data frame for proportion of nitrification/denitrification graph
## Because the data frames are of a different length compared to the other ones!
  averaged$no3cont = as.numeric(tapply(alldataraw$no3n.mgkg.1,alldataraw$timetreatno,mean)) #* 1000
  averaged$no3contsd = as.numeric(tapply(alldataraw$no3n.mgkg.1,alldataraw$timetreatno,sd)) #* 1000
  averaged$no3contcount = as.integer(tapply(alldataraw$no3n.mgkg.1,alldataraw$timetreatno,length))
  averaged$no3contse = calcse(averaged$no3contsd,averaged$no3contcount)

} # SECOND GRAPH - NO3 contents pF 2
{
  } # THIRD GRAPH - NH4 content pF 1
{
} # FOURTH GRAPH - NO3 content pF 1
} # Graph data setup

# Graph position calculations ---------------------------------------------
{
  #default number of graphs and the position of each graph on the page
  defaultgraphlayout = data.frame(c(1:15))
  defaultgraphlayout$graphnum = c(1:15)
  defaultgraphlayout$graphvertpos = c(5,4,3,2,1,5,4,3,2,1,5,4,3,2,1)
  defaultgraphlayout$graphhoripos = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3)
  defaultgraphlayout$graphpositions = c(1:15)
  defaultgraphlayout = defaultgraphlayout[,2:ncol(defaultgraphlayout)]

  # Top, right, bottom, centre
outermarg = data.frame(c(1:4))
outermarg$topleftmarg = c(topoutermargin,0,0,outerpagemargin + extraleft) ## + yspace/2 for inner margins
outermarg$midleftmarg = c(0,0,0,outerpagemargin + extraleft)
outermarg$midleft2marg = c(0,0,0,outerpagemargin + extraleft)
outermarg$midleft3marg = c(0,0,0,outerpagemargin + extraleft)
outermarg$botleftmarg = c(singletopmarg,0,outerpagemargin,outerpagemargin + extraleft)

outermarg$toprightmarg = c(topoutermargin,outerpagemargin + extraright,0,0)
outermarg$midrightmarg = c(0,outerpagemargin + extraright,0,0)
outermarg$midright2marg = c(0,outerpagemargin + extraright,0,0)
outermarg$midright3marg = c(0,outerpagemargin + extraright,0,0)
outermarg$botrightmarg = c(singletopmarg,outerpagemargin + extraright,outerpagemargin,0)

outermarg$topcentmarg = c(topoutermargin,0,0,0)
outermarg$midcentmarg = c(0,0,0,0)
outermarg$midcent2marg = c(0,0,0,0)
outermarg$midcent3marg = c(0,0,0,0)
outermarg$botcentmarg = c(singletopmarg,0,outerpagemargin,0)

outermargT = data.frame(t(outermarg)) # Transposed
outermargT = outermargT[2:nrow(outermargT),]
colnames(outermargT) = c("toppagemarg","rightpagemarg","botpagemarg","leftpagemarg")

# Top, right, bottom, left
gapmargdf = data.frame(c(1:4))
gapmargdf$topleftgapmarg = c(0,halfcentremargin,halfymargin,0)
gapmargdf$midleftgapmarg = c(halfymargin,halfcentremargin,halfymargin,0)
gapmargdf$midleft2gapmarg = c(halfymargin,halfcentremargin,halfymargin,0)
gapmargdf$midleft3gapmarg = c(halfymargin,halfcentremargin,halfymargin,0)
gapmargdf$botleftgapmarg = c(halfymargin,halfcentremargin,0,0)

gapmargdf$toprightgapmarg = c(0,0,halfymargin,halfcentremargin)
gapmargdf$midrightgapmarg = c(halfymargin,0,halfymargin,halfcentremargin)
gapmargdf$midright2gapmarg = c(halfymargin,0,halfymargin,halfcentremargin)
gapmargdf$midright3gapmarg = c(halfymargin,0,halfymargin,halfcentremargin)
gapmargdf$botrightgapmarg = c(halfymargin,0,0,halfcentremargin)

gapmargdf$topcentgapmarg = c(0,halfcentremargin,halfymargin,halfcentremargin)
gapmargdf$midcentgapmarg = c(halfymargin,halfcentremargin,halfymargin,halfcentremargin)
gapmargdf$midcent2gapmarg = c(halfymargin,halfcentremargin,halfymargin,halfcentremargin)
gapmargdf$midcent3gapmarg = c(halfymargin,halfcentremargin,halfymargin,halfcentremargin)
gapmargdf$botcentgapmarg = c(halfymargin,halfcentremargin,0,halfcentremargin)

#rm(halfcentremargin,halfymargin)

gapmargdfT = data.frame(t(gapmargdf))
gapmargdfT = gapmargdfT[2:nrow(gapmargdfT),]
colnames(gapmargdfT) = c("topgapmarg","rightgapmarg","botgapmarg","leftgapmarg")

totmargdf = data.frame(c(1:length(defaultgraphlayout$graphnum)))
totmargdf$toptotmarg = outermargT$toppagemarg + gapmargdfT$topgapmarg
totmargdf$righttotmarg = outermargT$rightpagemarg + gapmargdfT$rightgapmarg
totmargdf$bottotmarg = outermargT$botpagemarg + gapmargdfT$botgapmarg
totmargdf$lefttotmarg = outermargT$leftpagemarg + gapmargdfT$leftgapmarg
totmargdf = totmargdf[,2:ncol(totmargdf)]

#################### GGPLOT2 MARGINS
defaultgraphpositionsdf = data.frame(defaultgraphlayout,outermargT,gapmargdfT,totmargdf)
#rm(defaultgraphlayout,outermarg,outermargT,gapmargdf,gapmargdfT,totmargdf) #Remove unecessary dataframes from the R environment

#### The default dataframe is split according to the number of graphs and the graph
#### positions selected by the user

### %in% is used for selecting multiple values
graphposmargdf = defaultgraphpositionsdf[defaultgraphpositionsdf$graphpositions %in% chosengraphpositions,]
graphposmargdf$graphnum = chosengraphnum
graphposmargdf$graphvertpos = chosenvertpos
graphposmargdf$graphhoripos = chosenhoripos

#rm(chosenvertpos,chosenhoripos)

summargins = data.frame(1)
summargins$sumleftpagemargins = sum(graphposmargdf$leftpagemarg[graphposmargdf$graphvertpos == 1],na.rm=TRUE)
summargins$sumrightpagemargins = sum(graphposmargdf$rightpagemarg[graphposmargdf$graphvertpos == 1],na.rm=TRUE)
summargins$sumtoppagemargins = sum(graphposmargdf$toppagemarg[graphposmargdf$graphhoripos == 1],na.rm=TRUE)
summargins$sumbotpagemargins = sum(graphposmargdf$botpagemarg[graphposmargdf$graphhoripos == 1],na.rm=TRUE)
summargins$sumygap = yspace + ((layout[1]-2)*yspace)
# If only one column, normal size horizontal margin. If two columns, this is halved.
if (layout[2] == 1) summargins$xmarginbetweengraphs = outerpagemargin
if (layout[2] == 2) summargins$xmarginbetweengraphs = xmarginbetweengraphs

summargins$totalmarginwidth = with(summargins, (sumleftpagemargins + sumrightpagemargins + xmarginbetweengraphs))
summargins$totalmarginheight = with(summargins, (sumtoppagemargins + sumbotpagemargins + sumygap))

#### The following variables are important enough to be standalone vectors
#the overall size of the group of graphs that will be created by this code in cm
graphgroupwidth = pagedims[1] - summargins$totalmarginwidth[1]
graphgroupheight = pagedims[2] - summargins$totalmarginheight[1]
graphgroupdims = c(graphgroupwidth, graphgroupheight)

#the size of each individual graph in cm
graphwidth = (graphgroupdims[1] / layout[2])
graphheight = (graphgroupdims[2] / layout[1])
graphdims = c(graphwidth,graphheight)
graphprop = graphdims[2] / graphdims[1]

##### All the graph positions are calculated and then written into a data frame 
#debug(calcgraphdims)

if (is.element(1,chosengraphnum))  g1pos = calcgraphdims(1,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(1,chosengraphnum))  graphposmargdf = returnasdataframe(1,graphposmargdf,g1pos)
if (is.element(2,chosengraphnum))  g2pos = calcgraphdims(2,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(2,chosengraphnum))  graphposmargdf = returnasdataframe(2,graphposmargdf,g2pos)
if (is.element(3,chosengraphnum))  g3pos = calcgraphdims(3,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(3,chosengraphnum))  graphposmargdf = returnasdataframe(3,graphposmargdf,g3pos)
if (is.element(4,chosengraphnum))  g4pos = calcgraphdims(4,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(4,chosengraphnum))  graphposmargdf = returnasdataframe(4,graphposmargdf,g4pos)
if (is.element(5,chosengraphnum))  g5pos = calcgraphdims(5,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(5,chosengraphnum))  graphposmargdf = returnasdataframe(5,graphposmargdf,g5pos)
if (is.element(6,chosengraphnum))  g6pos = calcgraphdims(6,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(6,chosengraphnum))  graphposmargdf = returnasdataframe(6,graphposmargdf,g6pos)
if (is.element(7,chosengraphnum))  g7pos = calcgraphdims(7,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(7,chosengraphnum))  graphposmargdf = returnasdataframe(7,graphposmargdf,g7pos)
if (is.element(8,chosengraphnum))  g8pos = calcgraphdims(8,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(8,chosengraphnum))  graphposmargdf = returnasdataframe(8,graphposmargdf,g8pos)
if (is.element(9,chosengraphnum))  g9pos = calcgraphdims(9,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(9,chosengraphnum))  graphposmargdf = returnasdataframe(9,graphposmargdf,g9pos)
if (is.element(10,chosengraphnum)) g10pos = calcgraphdims(10,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(10,chosengraphnum)) graphposmargdf = returnasdataframe(10,graphposmargdf,g10pos)
if (is.element(11,chosengraphnum)) g11pos = calcgraphdims(11,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(11,chosengraphnum)) graphposmargdf = returnasdataframe(11,graphposmargdf,g11pos)
if (is.element(12,chosengraphnum)) g12pos = calcgraphdims(12,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(12,chosengraphnum)) graphposmargdf = returnasdataframe(12,graphposmargdf,g12pos)
if (is.element(13,chosengraphnum)) g13pos = calcgraphdims(13,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(13,chosengraphnum)) graphposmargdf = returnasdataframe(13,graphposmargdf,g13pos)
if (is.element(14,chosengraphnum)) g14pos = calcgraphdims(14,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(14,chosengraphnum)) graphposmargdf = returnasdataframe(14,graphposmargdf,g14pos)
if (is.element(15,chosengraphnum)) g15pos = calcgraphdims(15,graphposmargdf,layout, graphgroupdims,graphdims,pagedims)
if (is.element(15,chosengraphnum)) graphposmargdf = returnasdataframe(15,graphposmargdf,g15pos)

rm(g1pos,g2pos,g3pos,g4pos,g5pos,g6pos,
g7pos,g8pos,g9pos,g10pos)

############## graph tick lengths
#debug(tickdataframe)
#undebug(tickdataframe)

if (is.element(1,chosengraphnum)) g1ticks = tickdataframe(1,axislimits$xlimit,axislimits$g1ylimit,tickmultiplier,xbreaks,g1ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),xlabel,g1ylabel, firsttimetreat)
if (is.element(2,chosengraphnum)) g2ticks = tickdataframe(2,axislimits$xlimit,axislimits$g2ylimit,tickmultiplier,xbreaks,g2ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),xlabel,g2ylabel, firsttimetreat)
if (is.element(3,chosengraphnum)) g3ticks = tickdataframe(3,axislimits$xlimit,axislimits$g3ylimit,tickmultiplier,xbreaks,g3ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),xlabel,g3ylabel, firsttimetreat)
if (is.element(4,chosengraphnum)) g4ticks = tickdataframe(4,axislimits$xlimit,axislimits$g4ylimit,tickmultiplier,xbreaks,g4ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),xlabel,g4ylabel, firsttimetreat)
if (is.element(5,chosengraphnum)) g5ticks = tickdataframe(5,axislimits$xlimit,axislimits$g5ylimit,tickmultiplier,xbreaks,g5ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),xlabel,g5ylabel, firsttimetreat)
if (is.element(6,chosengraphnum)) g6ticks = tickdataframe(6,axislimits$xlimit,axislimits$g6ylimit,tickmultiplier,xbreaks,g6ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),xlabel,g6ylabel, firsttimetreat)
if (is.element(7,chosengraphnum)) g7ticks = tickdataframe(7,axislimits$xlimit,axislimits$g7ylimit,tickmultiplier,xbreaks,g7ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),xlabel,g7ylabel, firsttimetreat)
if (is.element(8,chosengraphnum)) g8ticks = tickdataframe(8,axislimits$xlimit,axislimits$g8ylimit,tickmultiplier,xbreaks,g8ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),xlabel,g8ylabel, firsttimetreat)
if (is.element(9,chosengraphnum)) g9ticks = tickdataframe(9,axislimits$barxlimit,axislimits$g9ylimit,tickmultiplier,barxbreaks,g9ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),barxlabel[2:7],g9ylabel, firsttimetreat)
if (is.element(10,chosengraphnum)) g10ticks = tickdataframe(10,axislimits$barxlimit,axislimits$g10ylimit,tickmultiplier,barxbreaks,g10ybreaks,graphdims,graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),barxlabel[9:14],g10ylabel, firsttimetreat)
if (is.element(11,chosengraphnum)) g11ticks = tickdataframe2(11,
                                                                       axislimits$g11xlimit,axislimits$eleventhbarylimit,
                                                                       tickmultiplier,g11xbreaks,eleventhbarybreaksgraphdims,
                                                                       graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),
                                                                       eleventhbarxlabel,g11ylabel, firsttimetreat,horizbargraph)
if (is.element(12,chosengraphnum)) g12ticks = tickdataframe2(12,
                                                                      axislimits$g12xlimit,axislimits$twelfthbarylimit,
                                                                      tickmultiplier,g12xbreaks,twelfthbarybreaksgraphdims,
                                                                      graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),
                                                                      twelfthbarxlabel,g12ylabel, firsttimetreat,horizbargraph)
if (is.element(13,chosengraphnum)) g13ticks = tickdataframe2(13,
                                                                         axislimits$g13xlimit,axislimits$thirteenthbarylimit,
                                                                         tickmultiplier,g13xbreaks,thirteenthbarybreaksgraphdims,
                                                                         graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),
                                                                         thirteenthbarxlabel,g13ylabel, firsttimetreat,horizbargraph)
if (is.element(14,chosengraphnum)) g14ticks = tickdataframe2(14,
                                                                        axislimits$g14xlimit,axislimits$forteenthbarylimit,
                                                                        tickmultiplier,g14xbreaks,forteenthbarybreaksgraphdims,
                                                                        graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),
                                                                        forteenthbarxlabel,g14ylabel, firsttimetreat,horizbargraph)
if (is.element(15,chosengraphnum)) g15ticks = tickdataframe2(15,
                                                                        axislimits$g15xlimit,axislimits$fifteenthbarylimit,
                                                                        tickmultiplier,g15xbreaks,fifteenthbarybreaksgraphdims,
                                                                        graphposmargdf,paste(groupvarname),paste(defaultgroupvarname),
                                                                        fifteenthbarxlabel,g15ylabel, firsttimetreat,horizbargraph)


##### X axis limits and breaks, just for the denitri vs nitri prop graphs

# axislimits$propxlimit = c(0.4, 4.6)
# propxbreaks = c(1, 2, 3, 4)
# propxaxissize = axislimits$propxlimit[2] - axislimits$propxlimit[1]
# propxaxistitle = expression("Time interval (days)")
# rounddata$xrounded = 0
# propxlabel = c("0-1","1-2","2-4","4-6")

rm(g1ybreaks,g2ybreaks,g3ybreaks,g4ybreaks,g5ybreaks,
   g6ybreaks,g7ybreaks,g8ybreaks,g9ybreaks,g10ybreaks,
   graphwidth, graphprop, graphheight, graphgroupheight, graphgroupwidth, summargins, tickmultiplier,
   xaxissize)
} # Graph positions

# GGplot2 settings --------------------------------------------------------

####SETTING THE GGPLOT2 THEME
previous_theme <- theme_set(theme_bw())
theme_set(theme_bw())

###The main theme is set, divided into sections too a bit later on
{
seangraph <- 
theme( 
  text = element_text(family = chosenfont),
  line = element_line(colour = "black",size = 0.25),
  rect = element_rect(fill = NA, colour = "black",size = 0.25,linetype = 1),
  strip.text = element_blank(),
  strip.text.x = element_blank(),
  strip.text.y = element_blank(),
  strip.background = element_blank(),  
  panel.background = element_blank(), #The panel is the gap around the axis text
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black",size = 0.25),
  panel.margin = unit(c(0,0,0,0),"null"), #top,right,bottom,left
  plot.title = element_text(face = "bold", size = 16, angle = 0),
  plot.background = element_blank(),
  plot.margin = unit(c(0,0,0,0),"null"), #top,right,bottom,left
  axis.line = element_line(colour = "black",size = 0.5),
  axis.text = element_blank(),
  axis.text.x = element_blank(), #axis.text.x = element_text(colour = "Black", size = 10),
  axis.title.x = element_blank(), #axis.title.x = element_text(colour = "Black", size = 10),
  axis.text.y = element_blank(), #axis.text.y = element_text(colour = "Black", hjust = 1, size = 10),
  axis.title.y = element_blank(), #axis.title.y = element_text(colour = "Black", angle = 90, vjust = 0.4, size = 10),
  axis.ticks = element_blank(),
  axis.ticks.margin = unit(0,"cm"), #put the next 2 as 0 cm and not 0 "null" because ggplot 2 gives an error otherwise when putting y text
  axis.ticks.length = unit(0.01,"cm"),
  axis.line.x = element_blank(), #element_segment(),
  axis.line.y = element_blank(), #element_segment(),  
  legend.box = NULL,
  legend.justification = NULL,
  legend.key = element_rect(fill = NA, colour = NA),
  legend.key.height = unit(0.3,"cm"),
  legend.key.width = unit(0.8,"cm"),
  legend.text = element_text(size = 10, angle = 0, family = chosenfont),
  legend.text.align = NULL,
  legend.title = element_text(size = 10, angle = 0,face = "bold"),
  legend.background = element_blank(),
  legend.direction = NULL,
  legend.position = legendpos, # x then y
  legend.title.align = 0,
  legend.margin = unit(0,"null"))

theme_set(theme_bw() + seangraph)

#Some default geom settings, most of these will be overwritten when making the graphs
#Default point shapes here: http://rgraphics.limnology.wisc.edu/images/miscellaneous/pch.png
#Default line types here: http://www.statmethods.net/advgraphs/images/lines.png
#Default colour codes here: http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
update_geom_defaults("point", list(colour = "black", pch = 21, bg = "white", size = 2)) 
update_geom_defaults("bar", list(colour = "black", bg = "white", size = .25, width = errwidth))
update_geom_defaults("rect", list(colour = "black", bg = "white", size = .1))
update_geom_defaults("line", list(colour = "black", lty = 1, size = .25)) 
update_geom_defaults("errorbar", list(colour = "black", lty = 1, size = .25))
update_geom_defaults("segment", list(colour = "black", lty = 1, size = .25))
update_geom_defaults("text", list(family = chosenfont, size = legendtextsize))
}

# Making the graphs in ggplot2 --------------------------------------------

firsthalf = 8
secondhalf = 7

if (is.element(1,chosengraphnum)) {
# INPUT DATA FRAME CREATION
# graphdatainput = function(inputframe,xvar,yvar,sevar,ysimvar,groupvar,outputframe){}
g1vars = graphdatainput(averaged,averaged$day,averaged$nh4cont,
              averaged$nh4contse,averaged$nh4cont,averaged$treatname,
              g1vars)
g1vars = g1vars[g1vars$treatno < firsthalf,]
g1ticks$group = "C"

lines = rep_len(unique(g1vars$lines),length(unique(g1vars$group))) #
fills = as.integer(rep_len(unique(g1vars$fills),length(unique(g1vars$group))))
shapes = rep_len(unique(g1vars$shape),length(unique(g1vars$group)))
outlines = unique(g1vars$outlines)

legenddata = data.frame(lines,fills,shapes,outlines)
legenddata$fills = as.integer(legenddata$fills)
legenddata$legendbreaks = unique(g1vars$group)
legenddata$legendbreakorder = legenddata$legendbreaks # You can select custom legend symbol orders if you like

g1vars$group = factor(g1vars$group, levels = c("S-LD250","S-LD190","S-LD130","S-LD70", "S-FD","S-AD","C"), ordered = TRUE)
g1vars = g1vars[order(g1vars$group, decreasing = TRUE),] # These two lines determine symbol display order


graph = 1
# Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
ggplot2marginopt = extractggmargins(graph,graphposmargdf)
limitsscale = definegglimitsscale(axislimits$xlimit,axislimits$g1ylimit,xbreaks,g1ticks$breaksystart[g1ticks$axisno == 1])
addgeoms = definegeomerrbarticks(g1vars,errwidth,g1ticks, TRUE, "point", TRUE)                                        

## Here the graph is created
g1 = ggplot(g1vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
  limitsscale + 
  ggplot2marginopt + 
   annotate("segment", x = axislimits$xlimit[1], xend = axislimits$xlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
   annotateletters(graph,axislimits$g1ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +
  #annotateletters(graph,axislimits$g1ylimit,"***",5,0.25,5) +
  geom_line((aes(x = xvar, y = yvar)),show.legend = TRUE) +
  #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
  addgeoms +
  with(legenddata, makenolegend(fills,lines,shapes,legendbreaks,legendbreaks,legendbreaks))
  g1

} # Graph 1 - NH4 graph pF 2
if (is.element(2,chosengraphnum)) {
  # INPUT DATA FRAME CREATION
  # graphdatainput = function(inputframe,xvar,yvar,sevar,ysimvar,groupvar,outputframe){}
  g2vars = graphdatainput(averaged,averaged$day,averaged$no3cont,
                                  averaged$no3contse,averaged$no3cont,averaged$treatname,
                                  g2vars)
  g2vars = g2vars[g2vars$treatno < firsthalf,]
  g2ticks$group = "C"
  
  lines = rep_len(unique(g2vars$lines),length(unique(g2vars$group))) #
  fills = as.integer(rep_len(unique(g2vars$fills),length(unique(g2vars$group))))
  shapes = rep_len(unique(g2vars$shape),length(unique(g2vars$group)))
  outlines = unique(g2vars$outlines)
  
  legenddata = data.frame(lines,fills,shapes,outlines)
  legenddata$fills = as.integer(legenddata$fills)
  legenddata$legendbreaks = unique(g2vars$group)
  
  legenddata$legendbreakorder = legenddata$legendbreaks # You can select custom legend symbol orders if you like
  
  g2vars$group = factor(g2vars$group, levels = c("S-LD250","S-LD190","S-LD130","S-LD70", "S-FD","S-AD","C"), ordered = TRUE)
  g2vars = g2vars[order(g2vars$group, decreasing = TRUE),] # These two lines determine symbol display order
  
  graph = 2
  # Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
  ggplot2marginopt = extractggmargins(graph,graphposmargdf)
  limitsscale = definegglimitsscale(axislimits$xlimit,axislimits$g2ylimit,xbreaks,g2ticks$breaksystart[g2ticks$axisno == 1])
  addgeoms = definegeomerrbarticks(g2vars,errwidth,g2ticks, TRUE, "point", TRUE)                                        
  
  ## Here the graph is created
  g2 = ggplot(g2vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
    limitsscale + 
    ggplot2marginopt + 
    annotate("segment", x = axislimits$xlimit[1], xend = axislimits$xlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
    annotateletters(graph,axislimits$g2ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +
    
    geom_line((aes(x = xvar, y = ysimvar)),show.legend = TRUE) +
    #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
    addgeoms +
    with(legenddata, makenolegend(fills,lines,shapes,legendbreaks,legendbreaks,legendbreaks))
#   g2
} # Graph 2 - NO3 graph pF 2
if (is.element(3,chosengraphnum)) {
  # INPUT DATA FRAME CREATION
  # graphdatainput = function(inputframe,xvar,yvar,sevar,ysimvar,groupvar,outputframe){}
  # Use fivepointavecomb dataframe for averaged data (e.g. for points and error bars, bar graphs)
  g3vars = graphdatainput(averaged,averaged$day,averaged$nh4cont,
                                  averaged$nh4contse,averaged$nh4cont,averaged$treatname,
                                  g3vars)
  g3vars = g3vars[g3vars$treatno > secondhalf,]
  g3ticks$group = "C"
  
  lines = rep_len(unique(g3vars$lines),length(unique(g3vars$group))) #
  fills = as.integer(rep_len(unique(g3vars$fills),length(unique(g3vars$group))))
  shapes = rep_len(unique(g3vars$shape),length(unique(g3vars$group)))
  outlines = unique(g3vars$outlines)
  
  legenddata = data.frame(lines,fills,shapes,outlines)
  legenddata$legendbreaks = unique(g3vars$group)
  
  legenddata$legendbreakorder = legenddata$legendbreaks # You can select custom legend symbol orders if you like
  
  g3vars$group = factor(g3vars$group, levels = c("S-LD250","S-LD190","S-LD130","S-LD70", "S-FD","S-AD","C"), ordered = TRUE)
  g3vars = g3vars[order(g3vars$group, decreasing = TRUE),] # These two lines determine symbol display order
  
    graph = 3
  # Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
  ggplot2marginopt = extractggmargins(graph,graphposmargdf)
  limitsscale = definegglimitsscale(axislimits$xlimit,axislimits$g3ylimit,xbreaks,g3ticks$breaksystart[g3ticks$axisno == 1])
  addgeoms = definegeomerrbarticks(g3vars,errwidth,g3ticks, TRUE, "point", TRUE)                                        
  
  ## Here the graph is created
  g3 = ggplot(g3vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
    limitsscale + 
    ggplot2marginopt + 
    annotate("segment", x = axislimits$xlimit[1], xend = axislimits$xlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
    annotateletters(graph,axislimits$g3ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +
    
    geom_line((aes(x = xvar, y = ysimvar)),show.legend = TRUE) +
    #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
    addgeoms +
    #makewithlegend(legenddata)      
    with(legenddata, makewithlegend(fills,lines,shapes,legendbreaks,legendbreaks,legendbreaks))
   g3
} # Graph 3 - NH4 graph pF 1
if (is.element(4,chosengraphnum)) {
  # INPUT DATA FRAME CREATION
  # graphdatainput = function(inputframe,xvar,yvar,sevar,ysimvar,groupvar,outputframe){}
  g4vars = graphdatainput(averaged,averaged$day,averaged$no3cont,
                                   averaged$no3contse,averaged$no3cont,averaged$treatname,
                                   g4vars)
  g4vars = g4vars[g4vars$treatno > secondhalf,]
  g4ticks$group = "C"
  
  lines = rep_len(unique(g4vars$lines),length(unique(g4vars$group))) #
  fills = as.integer(rep_len(unique(g4vars$fills),length(unique(g4vars$group))))
  shapes = rep_len(unique(g4vars$shape),length(unique(g4vars$group)))
  outlines = unique(g4vars$outlines)
  
  legenddata = data.frame(lines,fills,shapes,outlines)
  legenddata$fills = as.integer(legenddata$fills)
  legenddata$legendbreaks = unique(g4vars$group)
  legenddata$legendbreakorder = legenddata$legendbreaks # You can select custom legend symbol orders if you like
  
  g4vars$group = factor(g4vars$group, levels = c("S-LD250","S-LD190","S-LD130","S-LD70", "S-FD","S-AD","C"), ordered = TRUE)
  g4vars = g4vars[order(g4vars$group, decreasing = TRUE),] # These two lines determine symbol display order
  
  graph = 4
  # Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
  ggplot2marginopt = extractggmargins(graph,graphposmargdf)
  limitsscale = definegglimitsscale(axislimits$xlimit,axislimits$g4ylimit,xbreaks,g4ticks$breaksystart[g4ticks$axisno == 1])
  addgeoms = definegeomerrbarticks(g4vars,errwidth,g4ticks, TRUE, "point", TRUE)                                        
  
  ## Here the graph is created
  g4 = ggplot(g4vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
    limitsscale + 
    ggplot2marginopt + 
    annotate("segment", x = axislimits$xlimit[1], xend = axislimits$xlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
    annotateletters(graph,axislimits$g4ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +

    geom_line((aes(x = xvar, y = ysimvar)),show.legend = TRUE) +
    #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
    addgeoms +
    with(legenddata, makenolegend(fills,lines,shapes,legendbreaks,legendbreaks,legendbreaks))
   g4
} # Graph 4 - NO3 graph pF 1
if (is.element(5,chosengraphnum)) {
  # INPUT DATA FRAME CREATION
  # graphdatainput = function(inputframe,xvar,yvar,sevar,ysimvar,groupvar,outputframe){}
 
  g5vars = graphdatainput(mineraldataraw,mineraldataraw$day,mineraldataraw$mineralisenpercent,
                                   mineraldataraw$mineralisenpercentsd,mineraldataraw$mineralisenpercent,mineraldataraw$sludgetypeshort,
                                   g5vars)
  g5vars = g5vars[g5vars$lines < firsthalf,]
  
  #g5vars = g5vars[g5vars$group != "S pF 2",]
  
  g5vars$yvar = as.numeric(as.character(g5vars$yvar))
  g5vars$ysimvar = as.numeric(as.character(g5vars$ysimvar))
  g5vars$yvarse = as.numeric(as.character(g5vars$yvarse))
  g5vars[is.na(g5vars)] = 0
  g5vars$group = as.character(g5vars$group)
  g5ticks$group = "C"
  
  lines = rep_len(unique(g5vars$lines),length(unique(g5vars$group))) #
  fills = as.integer(rep_len(unique(g5vars$fills),length(unique(g5vars$group))))
  shapes = rep_len(unique(g5vars$shape),length(unique(g5vars$group)))
  outlines = unique(g5vars$outlines)
  
  legenddata = data.frame(lines,fills,shapes,outlines)
  legenddata$fills = as.integer(legenddata$fills)
  legenddata$legendbreaks = unique(g5vars$group)
  
  graph = 5
  # Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
  ggplot2marginopt = extractggmargins(graph,graphposmargdf)
  limitsscale = definegglimitsscale(axislimits$xlimit,axislimits$g5ylimit,xbreaks,g5ticks$breaksystart[g5ticks$axisno == 1])
  addgeoms = definegeomerrbarticks(g5vars,errwidth,g5ticks, TRUE, "point", TRUE)                                        
  
  ## Here the graph is created
  g5 = ggplot(g5vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
    limitsscale + 
    ggplot2marginopt + 
    annotate("segment", x = axislimits$xlimit[1], xend = axislimits$xlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
    annotateletters(graph,axislimits$g5ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +
    
    geom_line((aes(x = xvar, y = yvar)),show.legend = TRUE) +
    #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
    addgeoms +
    makenolegend(legenddata$fills, legenddata$lines, legenddata$shapes, legenddata$legendbreaks)
   g5
  
  } # Graph 5 - N mineralised % pF 2
if (is.element(6,chosengraphnum)) {
  g6vars = graphdatainput(mineraldataraw,mineraldataraw$day,mineraldataraw$mineralisenpercent,
                                  mineraldataraw$mineralisenpercentsd,mineraldataraw$mineralisenpercent,mineraldataraw$sludgetypeshort,
                                  g6vars)
  g6vars = g6vars[g6vars$lines > secondhalf,]
  
  #g6vars = g6vars[g6vars$group != "S pF 1",]
  
  g6vars$yvar = as.numeric(as.character(g6vars$yvar))
  g6vars$ysimvar = as.numeric(as.character(g6vars$ysimvar))
  g6vars$yvarse = as.numeric(as.character(g6vars$yvarse))
  g6vars[is.na(g6vars)] = 0
  g6vars$group = as.character(g6vars$group)
  g6ticks$group = "C"
  
  lines = rep_len(unique(g6vars$lines),length(unique(g6vars$group))) #
  fills = as.integer(rep_len(unique(g6vars$fills),length(unique(g6vars$group))))
  shapes = rep_len(unique(g6vars$shape),length(unique(g6vars$group)))
  outlines = unique(g6vars$outlines)
  
  legenddata = data.frame(lines,fills,shapes,outlines)
  legenddata$fills = as.integer(legenddata$fills)
  legenddata$legendbreaks = unique(g6vars$group)
  
  graph = 6
  # Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
  ggplot2marginopt = extractggmargins(graph,graphposmargdf)
  limitsscale = definegglimitsscale(axislimits$xlimit,axislimits$g6ylimit,xbreaks,g6ticks$breaksystart[g6ticks$axisno == 1])
  addgeoms = definegeomerrbarticks(g6vars,errwidth,g6ticks, TRUE, "point", TRUE)                                        
  
  ## Here the graph is created
  g6 = ggplot(g6vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
    limitsscale + 
    ggplot2marginopt + 
    annotate("segment", x = axislimits$xlimit[1], xend = axislimits$xlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
    annotateletters(graph,axislimits$g6ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +
    
    geom_line((aes(x = xvar, y = yvar)),show.legend = TRUE) +
    #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
    addgeoms +
    makenolegend(legenddata$fills, legenddata$lines, legenddata$shapes, legenddata$legendbreaks)
   g6
  } # Graph 6 - N mineralised % pF 1
if (is.element(7,chosengraphnum)) {
  # INPUT DATA FRAME CREATION
  # graphdatainput = function(inputframe,xvar,yvar,sevar,ysimvar,groupvar,outputframe){}
  
  g7vars = graphdatainput(mineraldataraw,mineraldataraw$day,mineraldataraw$mineralisenmgkg,
                                  mineraldataraw$mineralisensd,mineraldataraw$mineralisenmgkg,mineraldataraw$sludgetypeshort,
                                  g7vars)
  g7vars = g7vars[g7vars$lines < firsthalf,]
  
  g7vars$yvar = as.numeric(as.character(g7vars$yvar))
  g7vars$ysimvar = as.numeric(as.character(g7vars$ysimvar))
  g7vars$yvarse = as.numeric(as.character(g7vars$yvarse))
  g7vars[is.na(g7vars)] = 0
  g7vars$group = as.character(g7vars$group)
  g7ticks$group = "C"
  
  lines = rep_len(unique(g7vars$lines),length(unique(g7vars$group))) #
  fills = as.integer(rep_len(unique(g7vars$fills),length(unique(g7vars$group))))
  shapes = rep_len(unique(g7vars$shape),length(unique(g7vars$group)))
  outlines = unique(g7vars$outlines)
  
  legenddata = data.frame(lines,fills,shapes,outlines)
  legenddata$fills = as.integer(legenddata$fills)
  legenddata$legendbreaks = unique(g7vars$group)
  
  graph = 7
  # Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
  ggplot2marginopt = extractggmargins(graph,graphposmargdf)
  limitsscale = definegglimitsscale(axislimits$xlimit,axislimits$g7ylimit,xbreaks,g7ticks$breaksystart[g7ticks$axisno == 1])
  addgeoms = definegeomerrbarticks(g7vars,errwidth,g7ticks, TRUE, "point", TRUE)                                        
  
  ## Here the graph is created
  g7 = ggplot(g7vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
    limitsscale + 
    ggplot2marginopt + 
    annotate("segment", x = axislimits$xlimit[1], xend = axislimits$xlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
    annotateletters(graph,axislimits$g7ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +
    
    geom_line((aes(x = xvar, y = yvar)),show.legend = TRUE) +
    #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
    addgeoms +
    makenolegend(legenddata$fills, legenddata$lines, legenddata$shapes, legenddata$legendbreaks)
   g7
  } # Graph 7 - N mineralised mg kg-1 pF 2
if (is.element(8,chosengraphnum)) {
  g8vars = graphdatainput(mineraldataraw,mineraldataraw$day,mineraldataraw$mineralisenmgkg,
                                  mineraldataraw$mineralisensd,mineraldataraw$mineralisenmgkg,mineraldataraw$sludgetypeshort,
                                  g8vars)
  g8vars = g8vars[g8vars$lines > secondhalf,]
  
  g8vars$yvar = as.numeric(as.character(g8vars$yvar))
  g8vars$ysimvar = as.numeric(as.character(g8vars$ysimvar))
  g8vars$yvarse = as.numeric(as.character(g8vars$yvarse))
  g8vars[is.na(g8vars)] = 0
  g8vars$group = as.character(g8vars$group)
  g8ticks$group = "C"
  
  lines = rep_len(unique(g8vars$lines),length(unique(g8vars$group))) #
  fills = as.integer(rep_len(unique(g8vars$fills),length(unique(g8vars$group))))
  shapes = rep_len(unique(g8vars$shape),length(unique(g8vars$group)))
  outlines = unique(g8vars$outlines)
  
  legenddata = data.frame(lines,fills,shapes,outlines)
  legenddata$fills = as.integer(legenddata$fills)
  legenddata$legendbreaks = unique(g8vars$group)
  
  graph = 8
  # Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
  ggplot2marginopt = extractggmargins(graph,graphposmargdf)
  limitsscale = definegglimitsscale(axislimits$xlimit,axislimits$g8ylimit,xbreaks,g8ticks$breaksystart[g8ticks$axisno == 1])
  addgeoms = definegeomerrbarticks(g8vars,errwidth,g8ticks, TRUE, "point", TRUE)                                        
  
  ## Here the graph is created
  g8 = ggplot(g8vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
    limitsscale + 
    ggplot2marginopt + 
    annotate("segment", x = axislimits$xlimit[1], xend = axislimits$xlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
    annotateletters(graph,axislimits$g8ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +
    
    geom_line((aes(x = xvar, y = yvar)),show.legend = TRUE) +
    #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
    addgeoms +
    makenolegend(legenddata$fills, legenddata$lines, legenddata$shapes, legenddata$legendbreaks)
    g8
  } # Graph 8 - N mineralised mg kg-1 pF 1
if (is.element(9,chosengraphnum)) {
  # INPUT DATA FRAME CREATION
  # graphdatainput = function(inputframe,xvar,yvar,sevar,ysimvar,groupvar,outputframe){}
  mineralday120 = mineraldataraw[mineraldataraw$day == 120,]
  mineralday120 = mineralday120[mineralday120$treatno != 1 & mineralday120$treatno != 8,]
  
  g9vars = graphdatainput(mineralday120,barxbreaks,mineralday120$mineralisenpercent,
                                  mineralday120$mineralisenpercentsd,mineralday120$mineralisenpercent,mineralday120$treatname,
                                  g9vars)
  g9vars = g9vars[g9vars$lines < firsthalf,]
  
  g9vars$yvar = as.numeric(as.character(g9vars$yvar))
  g9vars$ysimvar = as.numeric(as.character(g9vars$ysimvar))
  g9vars$yvarse = as.numeric(as.character(g9vars$yvarse))
  g9vars[is.na(g9vars)] = 0
  g9vars$group = as.character(g9vars$group)
  g9ticks$group = "C"
  
  lines = rep_len(unique(g9vars$lines),length(unique(g9vars$group))) #
  fills = as.integer(rep_len(unique(g9vars$fills),length(unique(g9vars$group))))
  shapes = rep_len(unique(g9vars$shape),length(unique(g9vars$group)))
  outlines = unique(g9vars$outlines)
  
  legenddata = data.frame(lines,fills,shapes,outlines)
  legenddata$fills = as.integer(legenddata$fills)
  legenddata$legendbreaks = unique(g9vars$group)
  
  graph = 9
  # Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
  ggplot2marginopt = extractggmargins(graph,graphposmargdf)
  limitsscale = definegglimitsscale(axislimits$barxlimit,axislimits$g9ylimit,barxbreaks,g9ticks$breaksystart[g9ticks$axisno == 1])
  addgeoms = definegeomerrbarticks(g9vars,barerrwidth,g9ticks[g9ticks$axisno == 1 | g9ticks$axisno == 3,], TRUE, "bar", TRUE)                                        
  
  ## Here the graph is created
  g9 = ggplot(g9vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
    limitsscale + 
    ggplot2marginopt + 
    annotate("segment", x = axislimits$barxlimit[1], xend = axislimits$barxlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
    annotateletters(graph,axislimits$g9ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +
    #geom_line((aes(x = xvar, y = yvar)),show.legend = TRUE) +
    #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
    addgeoms +
    theme(legend.position = c(0.7,0.8),
          legend.key.width = unit(0.3,"cm"),
          legend.text = element_text(size = 7, angle = 0)) +  
    
    makenolegend(legenddata$fills, legenddata$lines, legenddata$shapes, legenddata$legendbreaks)
  g9
   } # Graph 9 - N mineralised day 120 pF 2
if (is.element(10,chosengraphnum)) {
  g10vars = graphdatainput(mineralday120,barxbreaks,mineralday120$mineralisenpercent,
                                  mineralday120$mineralisenpercentsd,mineralday120$mineralisenpercent,mineralday120$treatname,
                                  g10vars)
  g10vars = g10vars[g10vars$lines > secondhalf,]
  
  g10vars$yvar = as.numeric(as.character(g10vars$yvar))
  g10vars$ysimvar = as.numeric(as.character(g10vars$ysimvar))
  g10vars$yvarse = as.numeric(as.character(g10vars$yvarse))
  g10vars[is.na(g10vars)] = 0
  g10vars$group = as.character(g10vars$group)
  g10ticks$group = "C"
  
  lines = rep_len(unique(g10vars$lines),length(unique(g10vars$group))) #
  fills = as.integer(rep_len(unique(g10vars$fills),length(unique(g10vars$group))))
  shapes = rep_len(unique(g10vars$shape),length(unique(g10vars$group)))
  outlines = unique(g10vars$outlines)
  
  legenddata = data.frame(lines,fills,shapes,outlines)
  legenddata$fills = as.integer(legenddata$fills)
  legenddata$legendbreaks = unique(g10vars$group)
  
  graph = 10
  # Extract ggplot2 margins for graph, define scale options, calculate min/max error, define points, err bars and ticks
  ggplot2marginopt = extractggmargins(graph,graphposmargdf)
  limitsscale = definegglimitsscale(axislimits$barxlimit,axislimits$g10ylimit,barxbreaks,g10ticks$breaksystart[g10ticks$axisno == 1])
  addgeoms = definegeomerrbarticks(g10vars,barerrwidth,g10ticks[g10ticks$axisno == 1 | g10ticks$axisno == 3,], TRUE, "bar", TRUE)                                        
  
  ## Here the graph is created
  g10 = ggplot(g10vars, aes(xvar, yvar, fill = group, shape = group, linetype = group)) +
    limitsscale + 
    ggplot2marginopt + 
    annotate("segment", x = axislimits$xlimit[1], xend = axislimits$xlimit[2], y = 0, yend = 0, lty = 3,size = .25) +
    annotateletters(graph,axislimits$g10ylimit,textlabels[graph],labeltextsize,labelyposperc,labelxpos) +
    
    geom_line((aes(x = xvar, y = yvar)),show.legend = TRUE) +
    #stat_summary((aes(x = xvar, y = ysimvar)), fun.y = mean, geom="line",show.legend = TRUE) +
    addgeoms +
    theme(legend.position = c(0.7,0.8),
          legend.key.width = unit(0.3,"cm"),
          legend.text = element_text(size = 7, angle = 0)) +  
    makenolegend(legenddata$fills, legenddata$lines, legenddata$shapes, legenddata$legendbreaks)
  g10
} # Graph 10 - N mineralised day 120 pF1

# Creating viewpoints, printing graphs and adding text ---------------------

### The graphs are printed one by one into the viewport and then axis text added
# vppage = viewport(width = pagedims[1], height = pagedims[2], default.units = "cm",
#                  name = "vppage")
# push.viewport(vppage)
# grid.show.viewport(vppage)
# push.viewport(viewport(width = pagedims[1], height = pagedims[2], default.units = "cm",name = "vppage"))
# grid.show.viewport(viewport(width = pagedims[1], height = pagedims[2], default.units = "cm",
#                            name = "vppage"))

grid.newpage()

if (is.element(1,chosengraphnum)){
### Printing the graph
graph = 1

vp1 = calcviewport(graph,graphposmargdf)
# drawvprect(graph,graphposmargdf)
# drawgraph(graph,graphposmargdf)
printgraph(graph,g1,graphposmargdf)

#first number = graph number, second = TRUE/FALSE show,
#third = x axis or y axis?, fourth = show on axis numbers 1 = left, 2 = top, 3 = right, 4 = bottom
printaxistext(graph, printaxes$g1xtitle,4, 4, xaxistitle, graphposmargdf, 0, axisfontsize,xtitletextdist,chosenfont)
printaxistext(graph, printaxes$g1ytitle,1, 2, g1yaxistitle1, graphposmargdf, yaxisrotation, axisfontsize,textdist,chosenfont)
printaxistext(graph, printaxes$g1ytitle,1, 1, g1yaxistitle2, graphposmargdf, yaxisrotation, axisfontsize,textdist,chosenfont)
printaxislabel(graph,printaxes$g1yaxislab,1,g1ticks,axisfontsize,axislabeldist,rounddata$xrounded,rounddata$firstyrounded,0,0,chosenfont)
printaxislabel(graph,printaxes$g1xaxislab,4,g1ticks,axisfontsize,xaxislabeldist,rounddata$xrounded,rounddata$firstyrounded,0,0,chosenfont)

titlefontsize = axisfontsize + 2
printaxistext(graph, TRUE,2, 1, "pF 2", graphposmargdf, 0, titlefontsize,(textdist* (2/3)),chosenfont)
} # NH4 contents pF2
if (is.element(2,chosengraphnum)) {
graph = 2

vp2 = calcviewport(graph,graphposmargdf)
# drawvprect(graph,graphposmargdf)
# drawgraph(graph,graphposmargdf)
printgraph(graph,g2,graphposmargdf)

#first number = graph number, second = TRUE/FALSE show,
#third = x axis or y axis?, fourth = show on axis numbers 1 = left, 2 = top, 3 = right, 4 = bottom
printaxistext(graph, printaxes$g2xtitle,4, 4, xaxistitle, graphposmargdf, 0, axisfontsize,xtitletextdist,chosenfont)
printaxistext(graph, printaxes$g2ytitle,1, 2, g2yaxistitle1, graphposmargdf, yaxisrotation, axisfontsize,textdist,chosenfont)
printaxistext(graph, printaxes$g2ytitle,1, 1, g2yaxistitle2, graphposmargdf, yaxisrotation, axisfontsize,textdist,chosenfont)
printaxislabel(graph,printaxes$g2yaxislab,1,g2ticks,axisfontsize,axislabeldist,rounddata$xrounded,rounddata$secondyrounded,0,0,chosenfont)
#undebug(printaxislabel)
printaxislabel(graph,printaxes$g2xaxislab,4,g2ticks,axisfontsize,xaxislabeldist,rounddata$xrounded,rounddata$secondyrounded,0,0,chosenfont)
} # NO3 contents pF2
if (is.element(3,chosengraphnum)) {
graph = 3

vp3 = calcviewport(graph,graphposmargdf)
# drawvprect(graph,graphposmargdf)
# drawgraph(graph,graphposmargdf)
printgraph(graph,g3,graphposmargdf)

#first number = graph number, second = TRUE/FALSE show,
#third = x axis or y axis?, fourth = show on axis numbers 1 = left, 2 = top, 3 = right, 4 = bottom
printaxistext(graph, printaxes$g3xtitle,4, 4, xaxistitle, graphposmargdf, 0, axisfontsize,xtitletextdist,chosenfont)
printaxistext(graph, printaxes$g3ytitle,1, 2, g3yaxistitle1, graphposmargdf, yaxisrotation, axisfontsize,textdist,chosenfont)
printaxistext(graph, printaxes$g3ytitle,1, 1, g3yaxistitle2, graphposmargdf, yaxisrotation, axisfontsize,textdist,chosenfont)
printaxislabel(graph,printaxes$g3yaxislab,1,g3ticks,axisfontsize,axislabeldist,rounddata$xrounded,rounddata$thirdyrounded,0,0,chosenfont)
printaxislabel(graph,printaxes$g3xaxislab,4,g3ticks,axisfontsize,xaxislabeldist,rounddata$xrounded,rounddata$thirdyrounded,0,0,chosenfont)

titlefontsize = axisfontsize + 2
printaxistext(graph, TRUE,2, 1, "pF 1", graphposmargdf, 0, titlefontsize,(textdist* (2/3)),chosenfont)
} # NH4 contents pF1
if (is.element(4,chosengraphnum)) {
graph = 4
vp4 = calcviewport(graph,graphposmargdf)
# drawvprect(graph,graphposmargdf)
# drawgraph(graph,graphposmargdf)
printgraph(graph,g4,graphposmargdf)

#first number = graph number, second = TRUE/FALSE show,
#third = x axis or y axis?, fourth = show on axis numbers 1 = left, 2 = top, 3 = right, 4 = bottom
printaxistext(graph, printaxes$g4xtitle,4, 4, xaxistitle, graphposmargdf, 0, axisfontsize,xtitletextdist,chosenfont)
printaxistext(graph, printaxes$g4ytitle,1, 2, g4yaxistitle1, graphposmargdf, yaxisrotation, axisfontsize,textdist,chosenfont)
printaxistext(graph, printaxes$g4ytitle,1, 1, g4yaxistitle2, graphposmargdf, yaxisrotation, axisfontsize,textdist,chosenfont)
printaxislabel(graph,printaxes$g4yaxislab,1,g4ticks,axisfontsize,axislabeldist,rounddata$xrounded,rounddata$fourthyrounded,0,0,chosenfont)
printaxislabel(graph,printaxes$g4xaxislab,4,g4ticks,axisfontsize,xaxislabeldist,rounddata$xrounded,rounddata$fourthyrounded,0,0,chosenfont)
} # NO3 contents pF1

# Export graphs to picture ------------------------------------------------------------

#setwd("C:\\Users\\Sean\\Dropbox\\PhD\\Experimental data\\Lab exp 5 March 2012\\Analysis")
#To verify version, from https://www.stat.math.ethz.ch/pipermail/r-devel/2013-April/066439.html
#Sys.setenv(R_GSCMD="C:/Users/zkf134/AppData/Local/gs9.16/bin/gswin64c.exe")
# Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.16/bin/gswin64c.exe")
Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.18/bin/gswin64c.exe")

# Sys.getenv("R_GSCMD")
# Sys.getenv()

# This prints the graph to a tif file with tifflzw compression
## Need to have postscript for this to work
dev2bitmap("Fig 1 AmmNit 26-01-16.tiff", type = "tifflzw", width = pagedims[1], 
             height = pagedims[2], res = 800,
             units = "cm", method = "postscript", taa = 4, gaa = 4, family = "sans")
#taa and gaa (text and graphics) govern anti-aliasing quality
dev.off()

#### PDF and EPS formats

# Trying this from the 'extrafonts' package instead. Not sure this works.
# instructions are here: https://cran.r-project.org/web/packages/extrafont/README.html
#font_import()

# ## This prints the graph to a pdf file
# dev2bitmap("Fig 1 AmmNit 12-12-15.pdf", 
#      type = "pdfwrite", width = pagedims[1], 
#      height = pagedims[2], res = 1200,
#      units = "cm", method = "pdf", taa = 4, gaa = 4, family = chosenfont)
#taa and gaa (text and graphics) govern anti-aliasing quality
# dev.off()

#fonts()

# Trying this R function to embed fonts doesn't work
# embedFonts("Fig 1 AmmNit 12-12-15.pdf", 
#            fontpaths = "C:/Windows/Fonts/", outfile = "Fig 1 AmmNit 12-12-15embed.pdf")
# Trying this from the 'extrafonts' package instead. Not sure this works either.
#embed_fonts("Fig 1 AmmNit 12-12-15.pdf", outfile = "Fig 1 AmmNit 12-12-15embed.pdf")

# This example works, but you need to use ggsave
# windowsFonts(Georgia=windowsFont("Georgia"))
# g1georgia = g1 + geom_text(label=g1vars$timetreatno, family = "Georgia", 
#                                             position = "jitter")
# g1georgia
# ggsave("embedfonttest1.pdf", g1georgia, width=3.5, height=3.5)
# embed_fonts("embedfonttest1.pdf", outfile = "embedfonttest1_embedded.pdf")

# This prints the graph to an eps file
# dev2bitmap("Fig 1 AmmNit 26-01-16.eps", type = "eps2write", width = pagedims[1], 
#            height = pagedims[2], res = 1200,
#            units = "cm", method = "pdf", family = chosenfont)
# #taa and gaa (text and graphics) govern anti-aliasing quality
# dev.off()
# 
# #Trying this from the 'extrafonts' package instead. Not sure this works either.
# embed_fonts("Fig 1 AmmNit 26-01-16.eps", outfile = "Fig 1 AmmNitembed 26-01-16.eps")

# #embedFonts("Fig 1 AmmNit 24-01-16.eps", outfile = "Fig 1 AmmNit 24-01-16.eps")