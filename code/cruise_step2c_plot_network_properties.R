source('github/code/cruise_load_library.R')
source('github/code/cruise_plot_colour.R')

load('github/data/dataNodes.RData')

# retain nodes in the largest graph component of each sailing
dataNodes = dataNodes[COMPONENT=='1',]
dataNodesSummary = dataNodes[,.(quantile(STRENGTH, probs=0.5, na.rm=T),quantile(STRENGTH, probs=0.025, na.rm=T),quantile(STRENGTH, probs=0.975, na.rm=T),quantile(STRENGTH, probs=0.25, na.rm=T),quantile(STRENGTH, probs=0.75, na.rm=T),
                                quantile(EIGEN, probs=0.5, na.rm=T),quantile(EIGEN, probs=0.025, na.rm=T),quantile(EIGEN, probs=0.975, na.rm=T),quantile(EIGEN, probs=0.25, na.rm=T),quantile(EIGEN, probs=0.75, na.rm=T),
                                quantile(BETWEEN, probs=0.5, na.rm=T),quantile(BETWEEN, probs=0.025, na.rm=T),quantile(BETWEEN, probs=0.975, na.rm=T),quantile(BETWEEN, probs=0.25, na.rm=T),quantile(BETWEEN, probs=0.75, na.rm=T),
                                quantile(CC, probs=0.5, na.rm=T),quantile(CC, probs=0.025, na.rm=T),quantile(CC, probs=0.975, na.rm=T),quantile(CC, probs=0.25, na.rm=T),quantile(CC, probs=0.75, na.rm=T)),
                                by=.(SAIL,COHORT)]

setnames(dataNodesSummary, c('SAIL','COHORT',
                             'MED_DEG','LWR95_DEG','UPR95_DEG','LWR50_DEG','UPR50_DEG',
                             'MED_EIGEN','LWR95_EIGEN','UPR95_EIGEN','LWR50_EIGEN','UPR50_EIGEN',
                             'MED_BTW','LWR95_BTW','UPR95_BTW','LWR50_BTW','UPR50_BTW',
                             'MED_CC','LWR95_CC','UPR95_CC','LWR50_CC','UPR50_CC'))
dataNodesSummary[,COHORT:=toupper(COHORT)]
dataNodesSummary = dataNodesSummary[order(SAIL, COHORT)]

# plot degree distribution
paneller=function(row = 1,column=1)
{
  
  xlm=c(0,10)
  if(row == 1 & column == 1) ylm=c(0,60) 
  if(row == 2 & column == 1) ylm=c(0,1)
  if(row == 3 & column == 1) ylm=c(0,1) 

  innermargins = c(2,2,1,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  colMedian =  col50CI = c(CONFIG$cols[8],CONFIG$cols[4],CONFIG$cols[3],CONFIG$cols[9])
  col95CI =  c(CONFIG$colsLight3[8],CONFIG$colsLight3[4],CONFIG$colsLight3[3],CONFIG$colsLight3[9])
  
  pchShape = c(15,16,17,18)
  data=dataNodesSummary
  space = c(-0.3,-0.1,0.1,0.3)
  
  data[, COHORT_X_AXIS := c(rep(c(8,1:7,9), times=4) + rep(space, each=9))]
  data[, SHAPE:=rep(pchShape, each=9)]
  data[, COL_MEDIAN:=rep(colMedian, each=9)]
  data[, COL_50CI:=rep(col50CI, each=9)]
  data[, COL_95CI:=rep(col95CI, each=9)]
  
  if(row==1 & column==1){
    for(i in 1:nrow(data)){
      grid.lines(c(data$COHORT_X_AXIS[i],data$COHORT_X_AXIS[i]),c(data$LWR95_DEG[i], data$UPR95_DEG[i]),default.units = 'native',gp=gpar(col=data$COL_95CI[i]))
      grid.lines(c(data$COHORT_X_AXIS[i],data$COHORT_X_AXIS[i]),c(data$LWR50_DEG[i], data$UPR50_DEG[i]),default.units = 'native',gp=gpar(col=data$COL_50CI[i]))
      grid.points(data$COHORT_X_AXIS[i],data$MED_DEG[i],default.units = 'native',pch=data$SHAPE[i],gp=gpar(cex=0.6,col=data$COL_MEDIAN[i],fill=data$COL_MEDIAN[i]))
      
    }
  }
  
  if(row==2 & column==1){
    for(i in 1:nrow(data)){
      grid.lines(c(data$COHORT_X_AXIS[i],data$COHORT_X_AXIS[i]),c(data$LWR95_EIGEN[i], data$UPR95_EIGEN[i]),default.units = 'native',gp=gpar(col=data$COL_95CI[i]))
      grid.lines(c(data$COHORT_X_AXIS[i],data$COHORT_X_AXIS[i]),c(data$LWR50_EIGEN[i], data$UPR50_EIGEN[i]),default.units = 'native',gp=gpar(col=data$COL_50CI[i]))
      grid.points(data$COHORT_X_AXIS[i],data$MED_EIGEN[i],default.units = 'native',pch=data$SHAPE[i],gp=gpar(cex=0.6,col=data$COL_MEDIAN[i],fill=data$COL_MEDIAN[i]))
    }
  }
  
  if(row==3 & column==1){
    for(i in 1:nrow(data)){
      grid.lines(c(data$COHORT_X_AXIS[i],data$COHORT_X_AXIS[i]),c(data$LWR95_CC[i], data$UPR95_CC[i]),default.units = 'native',gp=gpar(col=data$COL_95CI[i]))
      grid.lines(c(data$COHORT_X_AXIS[i],data$COHORT_X_AXIS[i]),c(data$LWR50_CC[i], data$UPR50_CC[i]),default.units = 'native',gp=gpar(col=data$COL_50CI[i]))
      grid.points(data$COHORT_X_AXIS[i],data$MED_CC[i],default.units = 'native',pch=data$SHAPE[i],gp=gpar(cex=0.6,col=data$COL_MEDIAN[i],fill=data$COL_MEDIAN[i]))
    }
  }
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  

  if(row == 1 & column == 1) {
    
    grid.xaxis(at=seq(1,9,1),
               label=c('Entertain', 'F&B', 'Galley', 'Gaming', 'Hotel', 'Housekeep', 'Marine', 'Security', 'Passenger'),
               gp=gpar(fontsize=unit(8,'pt')))
    grid.yaxis(at=seq(0,60,10),label=seq(0,60,10))
    grid.text('a',x=unit(-2.5,'lines'),y=unit(12,'lines'),gp=gpar(fontsize=unit(12,'pt')))
    
      
    # legend
      grid.points(0.5,55,default.units = 'native',pch=15,gp=gpar(cex=0.6,col=CONFIG$cols[8]))
      grid.lines(c(0.25,0.75),c(55,55),default.units = 'native',gp=gpar(col=CONFIG$cols[8]))
      grid.text('sail 1',x=0.8,y=55,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
      
      grid.points(0.5,52,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=CONFIG$cols[4]))
      grid.lines(c(0.25,0.75),c(52,52),default.units = 'native',gp=gpar(col=CONFIG$cols[4]))
      grid.text('sail 2',x=0.8,y=52,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
      
      grid.points(0.5,49,default.units = 'native',pch=17,gp=gpar(cex=0.6,col=CONFIG$cols[3]))
      grid.lines(c(0.25,0.75),c(49,49),default.units = 'native',gp=gpar(col=CONFIG$cols[3]))
      grid.text('sail 3',x=0.8,y=49,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
      
      grid.points(0.5,46,default.units = 'native',pch=18,gp=gpar(cex=0.6,col=CONFIG$cols[9]))
      grid.lines(c(0.25,0.75),c(46,46),default.units = 'native',gp=gpar(col=CONFIG$cols[9]))
      grid.text('sail 4',x=0.8,y=46,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
    
    }
  
  if(row == 2 & column == 1) {grid.xaxis(at=seq(1,9,1),
                                         label=c('Entertain', 'F&B', 'Galley', 'Gaming', 'Hotel', 'Housekeep', 'Marine', 'Security', 'Passenger'),
                                         gp=gpar(fontsize=unit(8,'pt')))
    grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
    grid.text('b',x=unit(-2.5,'lines'),y=unit(12,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  
  if(row == 3 & column == 1) {grid.xaxis(at=seq(1,9,1),
                                         label=c('Entertain', 'F&B', 'Galley', 'Gaming', 'Hotel', 'Housekeep', 'Marine', 'Security', 'Passenger'),
                                         gp=gpar(fontsize=unit(8,'pt')))
    grid.yaxis(at=seq(0,1,0.25),label=seq(0,1,0.25))
    grid.text('c',x=unit(-2.5,'lines'),y=unit(12,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  
  
  if(row == 1) grid.text('Weighted degree',x=unit(-3.5,'lines'),rot=90)
  if(row == 2) grid.text('Eigen centrality',x=unit(-3.5,'lines'),rot=90)
  if(row == 3) grid.text('Clustering coefficient',x=unit(-3.5,'lines'),rot=90)
  if(row ==3 & column == 1) grid.text('Cohort',y=unit(-3,'lines'))
  
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  popViewport()
  popViewport()
  
}

png('figure/graph.png',height=24,width=16,units='cm',res=300,pointsize=10)

pushViewport(plotViewport(c(2,2,2,1)))
pushViewport(viewport(layout=grid.layout(nrow=3,ncol=1)))

paneller(1,1)
paneller(2,1)
paneller(3,1)

popViewport()
popViewport()
dev.off()

rm(paneller)



