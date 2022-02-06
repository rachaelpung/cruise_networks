source('github/code/cruise_load_library.R')
source('github/code/cruise_plot_colour.R')
load('github/data/dataNodes.Rdata')

# load results
listFile = dir('github/data/outputs/', pattern = 'results')
listFile
for(f in 1:length(listFile)){
  load(paste('github/data/outputs/', listFile[f], sep = ''))
}
rm(f,listFile)


paneller=function(row = 1,column=1)
{
  # if(row == 1 & column == 1) {xlm=c(0,7); ylm=log(c(1,50))} # plot cumulative incidence
  # if(row == 1 & column == 2) {xlm=c(1,7); ylm=c(0,15)} # plot cases by generation
  
  # if(row == 1 & column == 1) {xlm=c(0,1); ylm=c(0,1)}
  if(row == 1 & column == 1) {xlm=c(0.5,5.5); ylm=c(0,1)}
  if(row == 1 & column == 2) {xlm=c(0,100); ylm=c(0,20)}
  if(row == 1 & column == 3) {xlm=c(0,100); ylm=c(0,50)}

  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # data
  data = resultsStatic_set_1

  # set colour
  # if(row == 1 & column == 1){colMedian =  CONFIG$cols[4]; colCI = CONFIG$colsLight3[4]; colLine = 'gray90'}
  # if(row == 1 & column == 2){colMedian =  CONFIG$cols[3]; colCI = CONFIG$colsLight3[3]; colLine = 'gray90'}
  
  if(row == 1 & column == 1){colCategory =  c(CONFIG$colsLight1[3],CONFIG$colsLight1[7],CONFIG$colsLight1[6],CONFIG$colsLight1[5],CONFIG$colsLight1[1])}
  if(row == 1 & column == 2){colCategory =  c(CONFIG$cols[8],CONFIG$cols[4],CONFIG$cols[3],CONFIG$colsLight2[8],CONFIG$colsLight2[4],CONFIG$colsLight2[3])}
  if(row == 1 & column == 3){colCategory =  c(CONFIG$cols[8],CONFIG$cols[4],CONFIG$cols[3],CONFIG$colsLight2[8],CONFIG$colsLight2[4],CONFIG$colsLight2[3])}
  
  # # plot cumulative incidence
  # if(row == 1 & column == 1){
  #   
  #   resultsExp = data.table(sim = rep(1:data[,max(sim)], each = 8), exposure = rep(0:7, times = data[,max(sim)]))
  #   resultsExp[data[scenario=='nothing_none_vac_0_mask_0',.N, by=.(sim,exposure)], N:=i.N, on=c(sim='sim',exposure='exposure')]
  #   resultsExp = resultsExp[order(sim,exposure)]
  #   resultsExp[is.na(N), N:=0]
  #   resultsExp[, CUM_N := cumsum(N), by=.(sim)]
  #   data = resultsExp
  #   
  #   # set trajectory to plot
  #   for(s in c(8, 77, 103, 172, 535, 669, 806, 972, 933, 996)){
  #     grid.lines(data[sim == s, exposure],log(data[sim == s, CUM_N]),default.units = 'native',gp=gpar(lwd=0.75,col=colLine))
  #   }
  #  
  #   data = data[, .(quantile(CUM_N, probs = 0.5),
  #                   quantile(CUM_N, probs = 0.25),
  #                   quantile(CUM_N, probs = 0.75)), by=.(exposure)]
  # 
  #   data[V2==0, V2:=0.5]
  #   
  #   t=0:7
  #   grid.points(t[3:7],log(data$V1[3:7]),default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colMedian))
  #   grid.lines(t,log(data$V1),default.units = 'native',gp=gpar(col=colMedian))
  #   grid.polygon(c(t, rev(t)),
  #                c(log(data$V2), rev(log(data$V3))), default.units = 'native', gp=gpar(col=NA, fill=colCI))
  # 
  # }
  # 
  # # plot cases by generation
  # if(row == 1 & column == 2){
  # 
  #   resultsGen_7day = data.table(sim = rep(1:data[,max(sim)], each = 10), generation = rep(1:10, times = data[,max(sim)]))
  #   resultsGen_7day[data[scenario=='nothing_none_vac_0_mask_0' & exposure<=7,.N, by=.(sim,generation)],
  #                   N:=i.N, on=c(sim='sim',generation='generation')]
  #   resultsGen_7day = resultsGen_7day[order(sim,generation)]
  #   resultsGen_7day[is.na(N), N:=0]
  #   data = resultsGen_7day
  #   
  #   gen=1:7
  #   
  #   for(s in c(8, 77, 103, 172, 535, 669, 806, 972, 933, 996)){
  #     grid.lines(data[sim == s & generation %in% gen, generation],data[sim == s & generation %in% gen, N],default.units = 'native',gp=gpar(lwd=0.75,col=colLine))
  #   }
  # 
  #   data = data[, .(quantile(N, probs = 0.5),
  #                   quantile(N, probs = 0.25),
  #                   quantile(N, probs = 0.75)), by=.(generation)]
  # 
  #   grid.points(gen[2:4],data$V1[2:4],default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colMedian))
  #   grid.lines(gen,data[generation %in% gen]$V1,default.units = 'native',gp=gpar(col=colMedian))
  #   grid.polygon(c(gen, rev(gen)),
  #                c(data[generation %in% gen]$V2, rev(data[generation %in% gen]$V3)), default.units = 'native', gp=gpar(col=NA, fill=colCI))
  # 
  # }
  
  # plot cluster size
  if(row == 1 & column == 1){
    
    resultsSize = data.table(scenario=rep(c('nothing_none_vac_0_mask_0','isolation_PCR_once_vac_0_mask_0', 
                                            'isolation_Ag_twice_vac_0_mask_0','nothing_none_vac_0_mask_1',
                                            'nothing_none_vac_1_mask_0'), each=1000), sim=rep(1:1000,times=5))
    resultsSize[data[exposure<=7,.N,by=.(scenario,sim)], N:=i.N, on=c(scenario='scenario', sim='sim')]
    resultsSize[,N:=N-1] # no. of secondary cases
    resultsSize[is.na(N), N:=0]
    
    # by absolute size
    resultsSize[,bins:=cut(N, breaks = c(-1,0,5,10,15), labels = 0:3)]
    resultsSize[,bins:=as.numeric(bins)]
    resultsSize[is.na(bins),bins:=5]
    
    # by prop
    # resultsSize[,N.prop:=N/2181]
    # resultsSize[,bins:=cut(N.prop, breaks = c(-1,0,0.0025,0.005,0.0075), labels = 0:3)]
    # resultsSize[,bins:=as.numeric(bins)]
    # resultsSize[is.na(bins),bins:=5]
    
    
    
    resultsSize = resultsSize[,.N,by=.(scenario,bins)]
    resultsSize = merge(resultsSize, data.table(scenario=rep(c('nothing_none_vac_0_mask_0','isolation_PCR_once_vac_0_mask_0', 
                                                               'isolation_Ag_twice_vac_0_mask_0','nothing_none_vac_0_mask_1',
                                                               'nothing_none_vac_1_mask_0'), each=5), bins=rep(1:5,times=5)),
                        by=c('scenario','bins'), all.y=T)
    resultsSize[is.na(resultsSize)] = 0
    
    resultsSize = rbind(resultsSize[scenario=='nothing_none_vac_0_mask_0'],
                    resultsSize[scenario=='isolation_PCR_once_vac_0_mask_0'],
                    resultsSize[scenario=='isolation_Ag_twice_vac_0_mask_0'],
                    resultsSize[scenario=='nothing_none_vac_0_mask_1'],
                    resultsSize[scenario=='nothing_none_vac_1_mask_0'])
    
    resultsSize[,CUM_N:=cumsum(N),by=.(scenario)]
    resultsSize[,PMF_N:=N/max(CUM_N)]
    resultsSize[,CMF_N:=cumsum(PMF_N),by=.(scenario)]
  
    
    resultsSize[,scenario_xv:=rep(1:5, each=5)]
    
    for(i in 1:resultsSize[,.N]){
      
      if(resultsSize$bins[i]==1){
         grid.polygon(resultsSize$scenario_xv[i]+c(-0.25,-0.25,0.25,0.25),
                   c(0, resultsSize$CMF_N[i], resultsSize$CMF_N[i], 0), default.units = 'native', gp=gpar(col=NA,fill=colCategory[resultsSize$bins[i]]))
      } else{
        grid.polygon(resultsSize$scenario_xv[i]+c(-0.25,-0.25,0.25,0.25),
                     c(resultsSize$CMF_N[i-1], resultsSize$CMF_N[i], resultsSize$CMF_N[i], resultsSize$CMF_N[i-1]), default.units = 'native', gp=gpar(col=NA,fill=colCategory[resultsSize$bins[i]]))
        
      }
    }
    
  }
  
  # plot expected attack rate by scenario
  if(row == 1 & column == 2){
    
    
    resultsSize = data.table(scenario = rep(unique(data$scenario),  each=1000),
                             paramset = rep(1:1000, times = uniqueN(data$scenario)))
    
    resultsSize[data[exposure<=7,.N,by=.(scenario,paramset)], N:=i.N, 
                      on=c(scenario='scenario',paramset='paramset')]
    resultsSize[,N:=N-1]
    resultsSize[is.na(N),N:=0]
   
    data = resultsSize[, .(mean(N)), by=.(scenario)]
    
    data[,coverage:=rep(seq(0,100,25),each=3,len=.N)]
    data[,mask:=c(rep(0,times=15),rep(1,times=15))]
    
    data[mask==0,category:=rep(1:3, times=5)]
    data[mask==1,category:=rep(4:6, times=5)]
    
    
 
    for(c in 1:6){
     
      grid.points(data[category==c & coverage %in% c(25,50,75)]$coverage,data[category==c & coverage %in% c(25,50,75)]$V1,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[c])) 
      grid.lines(data[category==c]$coverage,data[category==c]$V1,default.units = 'native',gp=gpar(col=colCategory[c]))
    }
    
    grid.lines(seq(0,100,25),rep(1,times=5),default.units = 'native',gp=gpar(col='gray60', lty='dashed'))
    
    
  }
  
  
  # plot max attack rate by scenario
  if(row == 1 & column == 3){
    
    
    resultsSize = data.table(scenario = rep(unique(data$scenario),  each=1000),
                             paramset = rep(1:1000, times = uniqueN(data$scenario)))
    
    resultsSize[data[exposure<=7,.N,by=.(scenario,paramset)], N:=i.N, 
                on=c(scenario='scenario',paramset='paramset')]
    resultsSize[,N:=N-1]
    resultsSize[is.na(N),N:=0]
    
    data = resultsSize[, .(quantile(N,0.95)), by=.(scenario)]
    
    data[,coverage:=rep(seq(0,100,25),each=3,len=.N)]
    data[,mask:=c(rep(0,times=15),rep(1,times=15))]
    
    data[mask==0,category:=rep(1:3, times=5)]
    data[mask==1,category:=rep(4:6, times=5)]
    
    
    
    for(c in 1:6){
      
      grid.points(data[category==c & coverage %in% c(25,50,75)]$coverage,data[category==c & coverage %in% c(25,50,75)]$V1,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[c])) 
      grid.lines(data[category==c]$coverage,data[category==c]$V1,default.units = 'native',gp=gpar(col=colCategory[c]))
    }
    
    # grid.lines(seq(0,100,25),rep(1,times=5),default.units = 'native',gp=gpar(col='gray60', lty='dashed'))
    
    
  }
  
  
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  # if(row == 1 & column == 1) {grid.xaxis(at=seq(0,7,1),label=seq(0,7,1))
  #   grid.yaxis(at=log(c(1,2,5,10,20,50)),label=c(1,2,5,10,20,50))
  #   grid.text('a',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  # 
  # if(row == 1 & column == 2) {grid.xaxis(at=seq(1,7,1),label=seq(1,7,1))
  #   grid.yaxis(at=seq(0,15,5),label=seq(0,15,5))
  #   grid.text('b',x=unit(-2.5,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  
  if(row == 1 & column == 1) {
    grid.xaxis(at=seq(1,5,1),label=c('None','PCR','Antigen','Mask','Vaccine'),gp=gpar(fontsize=unit(7,'pt')))
    grid.yaxis(at=seq(0,1,0.25),label=seq(0,100,25))
    grid.text('b',x=unit(-2.5,'lines'),y=unit(9,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  
  if(row == 1 & column == 2) {grid.xaxis(at=seq(0,100,25),label=seq(0,100,25))
    grid.yaxis(at=seq(0,20,5),label=seq(0,20,5))
    grid.text('c',x=unit(-2.5,'lines'),y=unit(9,'lines'),gp=gpar(fontsize=unit(12,'pt')))
    
    # legend
    colCategory =  c(CONFIG$cols[8],CONFIG$cols[4],CONFIG$cols[3],CONFIG$colsLight2[8],CONFIG$colsLight2[4],CONFIG$colsLight2[3])
    
    
    grid.points(7,19,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[1]))
    grid.lines(c(4,10),c(19,19),default.units = 'native',gp=gpar(col=colCategory[1]))
    grid.text('No test',x=11,y=19,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
    
    grid.points(42,19,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[4]))
    grid.lines(c(39,45),c(19,19),default.units = 'native',gp=gpar(col=colCategory[4]))
    grid.text('No test, mask-on',x=46,y=19,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
    
    grid.points(7,17,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[2]))
    grid.lines(c(4,10),c(17,17),default.units = 'native',gp=gpar(col=colCategory[2]))
    grid.text('PCR test',x=11,y=17,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
    
    grid.points(42,17,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[5]))
    grid.lines(c(39,45),c(17,17),default.units = 'native',gp=gpar(col=colCategory[5]))
    grid.text('PCR test, mask-on',x=46,y=17,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
    
    grid.points(7,15,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[3]))
    grid.lines(c(4,10),c(15,15),default.units = 'native',gp=gpar(col=colCategory[3]))
    grid.text('Antigen test',x=11,y=15,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
    
    grid.points(42,15,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[6]))
    grid.lines(c(39,45),c(15,15),default.units = 'native',gp=gpar(col=colCategory[6]))
    grid.text('Antigen test, mask-on',x=46,y=15,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
    
  }

  if(row == 1 & column == 3) {grid.xaxis(at=seq(0,100,25),label=seq(0,100,25))
    grid.yaxis(at=seq(0,50,10),label=seq(0,50,10))
    grid.text('d',x=unit(-2.5,'lines'),y=unit(9,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  
  
  
  
  # if(row == 1 & column == 1) grid.text('Day',y=unit(-2.5,'lines'))
  # if(row == 1 & column == 1) grid.text('Cumulative no. of cases',x=unit(-3.5,'lines'),rot=90)
  # if(row == 1 & column == 2) grid.text('Generation',y=unit(-2.5,'lines'))
  # if(row == 1 & column == 2) grid.text('Cases',x=unit(-3.5,'lines'),rot=90)
  if(row == 1 & column == 1) grid.text('Scenario',y=unit(-3,'lines'))
  if(row == 1 & column == 1) grid.text('Proportion of simulations (%)',x=unit(-3,'lines'),rot=90)
  if(row == 1 & column == 2) grid.text('Vaccine coverage (%)',y=unit(-3,'lines'))
  if(row == 1 & column == 2) grid.text('Expected outbreak size',x=unit(-3,'lines'),rot=90)
  if(row == 1 & column == 3) grid.text('Vaccine coverage (%)',y=unit(-3,'lines'))
  if(row == 1 & column == 3) grid.text('95th percentile outbreak size',x=unit(-3,'lines'),rot=90)
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
  if(row ==1 & column ==1){
    
    colourbar(x_bottom_left = 1.2, y_bottom_left = -0.23, y_length = 0.035, x_width = 0.2)
  }
  
  
  popViewport()
  popViewport()
  
}


colourbar <- function(x_bottom_left = 1.1, y_bottom_left = 0.4, y_length = 0.2, x_width = 0.2){
  
  space=c(0.7,0.5,0.6,0.7,0.8)
  space=cumsum(space)
  text=c('0', '1-5', '6-10', '11-15', '>15')
  colCategory = c(CONFIG$colsLight1[3],CONFIG$colsLight1[7],CONFIG$colsLight1[6],CONFIG$colsLight1[5],CONFIG$colsLight1[1])
  
  grid.text('Cases',x=unit(x_bottom_left,'native'),y=unit(y_bottom_left+0.02,'native'),just='left',
            gp=gpar(fontsize=unit(6,'pt')))
  
  for(i in 1:5){
    
    grid.polygon(c(x_bottom_left+space[i],x_bottom_left+space[i],x_bottom_left+space[i]+x_width,x_bottom_left+space[i]+x_width),
                 c(y_bottom_left,y_bottom_left+y_length,y_bottom_left+y_length,y_bottom_left),
                 default.units = 'native',gp=gpar(col=colCategory[i], fill=colCategory[i]))
    
    grid.text(text[i],x=unit(x_bottom_left+space[i]+x_width+0.05,'native'),y=unit(y_bottom_left+0.02,'native'),just='left',
              gp=gpar(fontsize=unit(6,'pt')))
    
  }
  
}

inches = 0.393701
pdf('figure/nature comms pdf/m4b-d_simulation.pdf',height=7*inches,width=24*inches,pointsize=10)
# png('figure/simulation_main_v2.png',height=8,width=24,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=3)))

# paneller(1,1)
paneller(1,1)
paneller(1,2)
paneller(1,3)

popViewport()
popViewport()
dev.off()

rm(paneller)


paneller=function(row = 1,column=1)
{
  if(row == 1 & column == 1) {xlm=c(0,7); ylm=log(c(1,50))} # plot cumulative incidence
  if(row == 1 & column == 2) {xlm=c(1,7); ylm=c(0,15)} # plot cases by generation
  
 
  
  innermargins = c(2,2,2,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # data
  data = resultsStatic_set_1
  
  # set colour
  if(row == 1 & column == 1){colMedian =  CONFIG$cols[4]; colCI = CONFIG$colsLight3[4]; colLine = 'gray90'}
  if(row == 1 & column == 2){colMedian =  CONFIG$cols[3]; colCI = CONFIG$colsLight3[3]; colLine = 'gray90'}
  
 
  # plot cumulative incidence
  if(row == 1 & column == 1){

    resultsExp = data.table(sim = rep(1:data[,max(sim)], each = 8), exposure = rep(0:7, times = data[,max(sim)]))
    resultsExp[data[scenario=='nothing_none_vac_0_mask_0',.N, by=.(sim,exposure)], N:=i.N, on=c(sim='sim',exposure='exposure')]
    resultsExp = resultsExp[order(sim,exposure)]
    resultsExp[is.na(N), N:=0]
    resultsExp[, CUM_N := cumsum(N), by=.(sim)]
    data = resultsExp

    # set trajectory to plot
    for(s in c(8, 77, 103, 172, 535, 669, 806, 972, 933, 996)){
      grid.lines(data[sim == s, exposure],log(data[sim == s, CUM_N]),default.units = 'native',gp=gpar(lwd=0.75,col=colLine))
    }

    data = data[, .(quantile(CUM_N, probs = 0.5),
                    quantile(CUM_N, probs = 0.25),
                    quantile(CUM_N, probs = 0.75)), by=.(exposure)]

    data[V2==0, V2:=0.5]

    t=0:7
    grid.points(t[3:7],log(data$V1[3:7]),default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colMedian))
    grid.lines(t,log(data$V1),default.units = 'native',gp=gpar(col=colMedian))
    grid.polygon(c(t, rev(t)),
                 c(log(data$V2), rev(log(data$V3))), default.units = 'native', gp=gpar(col=NA, fill=colCI))

  }

  # plot cases by generation
  if(row == 1 & column == 2){

    resultsGen_7day = data.table(sim = rep(1:data[,max(sim)], each = 10), generation = rep(1:10, times = data[,max(sim)]))
    resultsGen_7day[data[scenario=='nothing_none_vac_0_mask_0' & exposure<=7,.N, by=.(sim,generation)],
                    N:=i.N, on=c(sim='sim',generation='generation')]
    resultsGen_7day = resultsGen_7day[order(sim,generation)]
    resultsGen_7day[is.na(N), N:=0]
    data = resultsGen_7day

    gen=1:7

    for(s in c(8, 77, 103, 172, 535, 669, 806, 972, 933, 996)){
      grid.lines(data[sim == s & generation %in% gen, generation],data[sim == s & generation %in% gen, N],default.units = 'native',gp=gpar(lwd=0.75,col=colLine))
    }

    data = data[, .(quantile(N, probs = 0.5),
                    quantile(N, probs = 0.25),
                    quantile(N, probs = 0.75)), by=.(generation)]

    grid.points(gen[2:4],data$V1[2:4],default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colMedian))
    grid.lines(gen,data[generation %in% gen]$V1,default.units = 'native',gp=gpar(col=colMedian))
    grid.polygon(c(gen, rev(gen)),
                 c(data[generation %in% gen]$V2, rev(data[generation %in% gen]$V3)), default.units = 'native', gp=gpar(col=NA, fill=colCI))

  }
  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  
  if(row == 1 & column == 1) {grid.xaxis(at=seq(0,7,1),label=seq(0,7,1))
    grid.yaxis(at=log(c(1,2,5,10,20,50)),label=c(1,2,5,10,20,50))
    grid.text('a',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))}

  if(row == 1 & column == 2) {grid.xaxis(at=seq(1,7,1),label=seq(1,7,1))
    grid.yaxis(at=seq(0,15,5),label=seq(0,15,5))
    grid.text('b',x=unit(-2.5,'lines'),y=unit(10,'lines'),gp=gpar(fontsize=unit(12,'pt')))}

  
  

  if(row == 1 & column == 1) grid.text('Day',y=unit(-2.5,'lines'))
  if(row == 1 & column == 1) grid.text('Cumulative no. of cases',x=unit(-3.5,'lines'),rot=90)
  if(row == 1 & column == 2) grid.text('Generation',y=unit(-2.5,'lines'))
  if(row == 1 & column == 2) grid.text('Cases',x=unit(-3.5,'lines'),rot=90)

  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
 
  
  
  popViewport()
  popViewport()
  
}




png('figure/simulation_main_supp.png',height=8,width=16,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,1,1)))
pushViewport(viewport(layout=grid.layout(nrow=1,ncol=2)))

paneller(1,1)
paneller(1,2)


popViewport()
popViewport()
dev.off()

rm(paneller)


paneller=function(row = 1,column=1)
{
  xlm=c(0,100)
  
  # results 1-3 ve_50_inf_50_presymp_25
  if(row == 1 & column==1){ylm=c(0,20)}
  if(row == 1 & column==2){ylm=c(0,100)}
  if(row == 1 & column==3){ylm=c(0,1000)}
  if(row == 2 & column==1){ylm=c(0,50)}
  if(row == 2 & column==2){ylm=c(0,250)}
  if(row == 2 & column==3){ylm=c(0,2000)}

  # results 4-6 ve_50_inf_100_presymp_25
  # if(row == 1 & column==1){ylm=c(0,20)}
  # if(row == 1 & column==2){ylm=c(0,100)}
  # if(row == 1 & column==3){ylm=c(0,1000)}
  # if(row == 2 & column==1){ylm=c(0,60)}
  # if(row == 2 & column==2){ylm=c(0,250)}
  # if(row == 2 & column==3){ylm=c(0,2000)}
  
  # results 7-9 ve_50_inf_50_presymp_50
  # if(row == 1 & column==1){ylm=c(0,50)}
  # if(row == 1 & column==2){ylm=c(0,350)}
  # if(row == 1 & column==3){ylm=c(0,2000)}
  # if(row == 2 & column==1){ylm=c(0,150)}
  # if(row == 2 & column==2){ylm=c(0,750)}
  # if(row == 2 & column==3){ylm=c(0,2500)}
  
  # results 10-12 3_network
  # if(row == 1 & column==1){ylm=c(0,20)}
  # if(row == 1 & column==2){ylm=c(0,10)}
  # if(row == 1 & column==3){ylm=c(0,10)}
  # if(row == 2 & column==1){ylm=c(0,60)}
  # if(row == 2 & column==2){ylm=c(0,25)}
  # if(row == 2 & column==3){ylm=c(0,25)}
  
  # results 13-15 mask 70% effective
  # if(row == 1 & column==1){ylm=c(0,20)}
  # if(row == 1 & column==2){ylm=c(0,100)}
  # if(row == 1 & column==3){ylm=c(0,1000)}
  # if(row == 2 & column==1){ylm=c(0,60)}
  # if(row == 2 & column==2){ylm=c(0,250)}
  # if(row == 2 & column==3){ylm=c(0,2000)}
  
  # results 16-18 temporal
  # if(row == 1 & column==1){ylm=c(0,5)}
  # if(row == 1 & column==2){ylm=c(0,10)}
  # if(row == 1 & column==3){ylm=c(0,50)}
  # if(row == 2 & column==1){ylm=c(0,10)}
  # if(row == 2 & column==2){ylm=c(0,20)}
  # if(row == 2 & column==3){ylm=c(0,100)}
  
  # results 19-21 infection param
  # if(row == 1 & column==1){ylm=c(0,20)}
  # if(row == 1 & column==2){ylm=c(0,100)}
  # if(row == 1 & column==3){ylm=c(0,1000)}
  # if(row == 2 & column==1){ylm=c(0,60)}
  # if(row == 2 & column==2){ylm=c(0,300)}
  # if(row == 2 & column==3){ylm=c(0,2000)}

  
  innermargins = c(2,2,1,2)
  
  pushViewport(viewport(layout.pos.col=column,layout.pos.row=row))
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # data
  if(column==1){data = resultsStatic_set_1} #resultsStatic_set_1
  if(column==2){data = resultsStatic_set_2} #resultsStatic_set_2
  if(column==3){data = resultsStatic_set_3} #resultsStatic_set_3
  
  # set colour
  colCategory =  c(CONFIG$cols[8],CONFIG$cols[4],CONFIG$cols[3],CONFIG$colsLight2[8],CONFIG$colsLight2[4],CONFIG$colsLight2[3])
  

  if(row==1){
    # plot expected attack rate by incidence
    resultsSize = data.table(scenario = rep(unique(data$scenario),  each=1000),
                             paramset = rep(1:1000, times = uniqueN(data$scenario)))
    
    resultsSize[data[exposure<=7,.N,by=.(scenario,paramset)], N:=i.N, 
                on=c(scenario='scenario',paramset='paramset')]
    resultsSize[,N:=N-1]
    resultsSize[is.na(N),N:=0]
    
    data = resultsSize[, .(mean(N)), by=.(scenario)]
  }
  
  if(row==2){
    # plot max attack rate by incidence
    resultsSize = data.table(scenario = rep(unique(data$scenario),  each=1000),
                             paramset = rep(1:1000, times = uniqueN(data$scenario)))
    
    resultsSize[data[exposure<=7,.N,by=.(scenario,paramset)], N:=i.N, 
                on=c(scenario='scenario',paramset='paramset')]
    resultsSize[,N:=N-1]
    resultsSize[is.na(N),N:=0]
    
    data = resultsSize[, .(quantile(N, 0.95)), by=.(scenario)]
  }
  
  
  data[,coverage:=rep(seq(0,100,25),each=3,len=.N)]
  data[,mask:=c(rep(0,times=15),rep(1,times=15))]
  
  data[mask==0,category:=rep(1:3, times=5)]
  data[mask==1,category:=rep(4:6, times=5)]
  
  
  for(c in 1:6){
    grid.points(data[category==c & coverage %in% c(25,50,75)]$coverage,data[category==c & coverage %in% c(25,50,75)]$V1,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[c])) 
    grid.lines(data[category==c]$coverage,data[category==c]$V1,default.units = 'native',gp=gpar(col=colCategory[c]))
  }
  
  # grid.lines(seq(0,100,25),rep(1,times=5),default.units = 'native',gp=gpar(col='gray60', lty='dashed'))
    
  # legend
  if(row==1 & column==1){legend()}
  

  
  
  popViewport()
  pushViewport(plotViewport(innermargins,xscale=xlm,yscale=ylm))
  
  # labels
  if(row == 1 & column == 1) {
    grid.text('a',x=unit(-2.7,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
    grid.text('Expected outbreak size',x=unit(-3.5,'lines'),rot=90)}
  if(row == 1 & column == 2) {
    grid.text('b',x=unit(-2.7,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  if(row == 1 & column == 3) {
    grid.text('c',x=unit(-2.7,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  if(row == 2 & column == 1) {
    grid.text('d',x=unit(-2.7,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))
    grid.text('95th percentile outbreak size',x=unit(-3.5,'lines'),rot=90)}
  if(row == 2 & column == 2) {
    grid.text('e',x=unit(-2.7,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  if(row == 2 & column == 3) {
    grid.text('f',x=unit(-2.7,'lines'),y=unit(11.5,'lines'),gp=gpar(fontsize=unit(12,'pt')))}
  
  # axis
  grid.xaxis(at=seq(0,100,25),label=seq(0,100,25))
  
  
  # results 1-3
  if(row == 1 & column == 1) {grid.yaxis(at=seq(0,20,5),label=seq(0,20,5))}
  if(row == 1 & column == 2) {grid.yaxis(at=seq(0,100,25),label=seq(0,100,25))}
  if(row == 1 & column == 3) {grid.yaxis(at=seq(0,1000,250),label=seq(0,1000,250))}
  if(row == 2 & column == 1) {grid.yaxis(at=seq(0,50,10),label=seq(0,50,10))}
  if(row == 2 & column == 2) {grid.yaxis(at=seq(0,250,50),label=seq(0,250,50))}
  if(row == 2 & column == 3) {grid.yaxis(at=seq(0,2000,500),label=seq(0,2000,500))}

  # results 4-6
  # if(row == 1 & column == 1) {grid.yaxis(at=seq(0,20,5),label=seq(0,20,5))}
  # if(row == 1 & column == 2) {grid.yaxis(at=seq(0,100,25),label=seq(0,100,25))}
  # if(row == 1 & column == 3) {grid.yaxis(at=seq(0,1000,250),label=seq(0,1000,250))}
  # if(row == 2 & column == 1) {grid.yaxis(at=seq(0,60,10),label=seq(0,60,10))}
  # if(row == 2 & column == 2) {grid.yaxis(at=seq(0,250,50),label=seq(0,250,50))}
  # if(row == 2 & column == 3) {grid.yaxis(at=seq(0,2000,500),label=seq(0,2000,500))}

  # results 7-9
  # if(row == 1 & column == 1) {grid.yaxis(at=seq(0,50,10),label=seq(0,50,10))}
  # if(row == 1 & column == 2) {grid.yaxis(at=seq(0,350,50),label=seq(0,350,50))}
  # if(row == 1 & column == 3) {grid.yaxis(at=seq(0,2000,500),label=seq(0,2000,500))}
  # if(row == 2 & column == 1) {grid.yaxis(at=seq(0,150,25),label=seq(0,150,25))}
  # if(row == 2 & column == 2) {grid.yaxis(at=seq(0,750,150),label=seq(0,750,150))}
  # if(row == 2 & column == 3) {grid.yaxis(at=seq(0,2500,500),label=seq(0,2500,500))}
  
  # results 10-12
  # if(row == 1 & column == 1) {grid.yaxis(at=seq(0,20,5),label=seq(0,20,5))}
  # if(row == 1 & column == 2) {grid.yaxis(at=seq(0,10,2),label=seq(0,10,2))}
  # if(row == 1 & column == 3) {grid.yaxis(at=seq(0,10,2),label=seq(0,10,2))}
  # if(row == 2 & column == 1) {grid.yaxis(at=seq(0,60,10),label=seq(0,60,10))}
  # if(row == 2 & column == 2) {grid.yaxis(at=seq(0,25,5),label=seq(0,25,5))}
  # if(row == 2 & column == 3) {grid.yaxis(at=seq(0,25,5),label=seq(0,25,5))}
  
  # results 13-15
  # if(row == 1 & column == 1) {grid.yaxis(at=seq(0,20,5),label=seq(0,20,5))}
  # if(row == 1 & column == 2) {grid.yaxis(at=seq(0,100,25),label=seq(0,100,25))}
  # if(row == 1 & column == 3) {grid.yaxis(at=seq(0,1000,250),label=seq(0,1000,250))}
  # if(row == 2 & column == 1) {grid.yaxis(at=seq(0,60,10),label=seq(0,60,10))}
  # if(row == 2 & column == 2) {grid.yaxis(at=seq(0,250,50),label=seq(0,250,50))}
  # if(row == 2 & column == 3) {grid.yaxis(at=seq(0,2000,500),label=seq(0,2000,500))}

  # results 16-18
  # if(row == 1 & column == 1) {grid.yaxis(at=seq(0,5,1),label=seq(0,5,1))}
  # if(row == 1 & column == 2) {grid.yaxis(at=seq(0,10,2),label=seq(0,10,2))}
  # if(row == 1 & column == 3) {grid.yaxis(at=seq(0,50,10),label=seq(0,50,10))}
  # if(row == 2 & column == 1) {grid.yaxis(at=seq(0,10,2),label=seq(0,10,2))}
  # if(row == 2 & column == 2) {grid.yaxis(at=seq(0,20,5),label=seq(0,20,5))}
  # if(row == 2 & column == 3) {grid.yaxis(at=seq(0,100,20),label=seq(0,100,20))}

  # results 19-21
  # if(row == 1 & column == 1) {grid.yaxis(at=seq(0,20,5),label=seq(0,20,5))}
  # if(row == 1 & column == 2) {grid.yaxis(at=seq(0,100,25),label=seq(0,100,25))}
  # if(row == 1 & column == 3) {grid.yaxis(at=seq(0,1000,250),label=seq(0,1000,250))}
  # if(row == 2 & column == 1) {grid.yaxis(at=seq(0,60,10),label=seq(0,60,10))}
  # if(row == 2 & column == 2) {grid.yaxis(at=seq(0,300,50),label=seq(0,300,50))}
  # if(row == 2 & column == 3) {grid.yaxis(at=seq(0,2000,500),label=seq(0,2000,500))}
  
  grid.lines(c(0,1,1,0,0),c(0,0,1,1,0))
  
 
  
  
  popViewport()
  popViewport()
  
}


legend <- function(){
  
  # legend
  colCategory =  c(CONFIG$cols[8],CONFIG$cols[4],CONFIG$cols[3],CONFIG$colsLight2[8],CONFIG$colsLight2[4],CONFIG$colsLight2[3])

  # results 1-3 
  y.max = 19; space = 1.5
  
  # results 4-6 
  # y.max = 19; space = 1.5
  
  # results 7-9 
  # y.max = 48.5; space = 4
  
  # results 10-12
  # y.max = 19; space = 1.5
  
  # results 13-15
  # y.max = 19; space = 1.5
  
  # results 16-18
  # y.max = 4.7; space = 0.25
  
  # results 19-21 
  # y.max = 19; space = 1.5
  
  grid.points(12,y.max,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[1]))
  grid.lines(c(9,15),c(y.max,y.max),default.units = 'native',gp=gpar(col=colCategory[1]))
  grid.text('No test',x=16,y=y.max,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
  
  grid.points(47,y.max,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[4]))
  grid.lines(c(44,50),c(y.max,y.max),default.units = 'native',gp=gpar(col=colCategory[4]))
  grid.text('No test, mask-on',x=51,y=y.max,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
  
  grid.points(12,y.max-space,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[2]))
  grid.lines(c(9,15),c(y.max-space,y.max-space),default.units = 'native',gp=gpar(col=colCategory[2]))
  grid.text('PCR test',x=16,y=y.max-space,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
  
  grid.points(47,y.max-space,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[5]))
  grid.lines(c(44,49),c(y.max-space,y.max-space),default.units = 'native',gp=gpar(col=colCategory[5]))
  grid.text('PCR test, mask-on',x=51,y=y.max-space,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
  
  grid.points(12,y.max-2*space,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[3]))
  grid.lines(c(9,15),c(y.max-2*space,y.max-2*space),default.units = 'native',gp=gpar(col=colCategory[3]))
  grid.text('Antigen test',x=16,y=y.max-2*space,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
  
  grid.points(47,y.max-2*space,default.units = 'native',pch=16,gp=gpar(cex=0.6,col=colCategory[6]))
  grid.lines(c(44,50),c(y.max-2*space,y.max-2*space),default.units = 'native',gp=gpar(col=colCategory[6]))
  grid.text('Antigen test, mask-on',x=51,y=y.max-2*space,default.units = 'native',just='left',gp=gpar(fontsize=unit(8,'pt')))
  
}



# simulation_sensitivity_analysis_ve_50_inf_50_presymp_25
# simulation_sensitivity_analysis_ve_50_inf_100_presymp_25
# simulation_sensitivity_analysis_ve_50_inf_50_presymp_50
# simulation_sensitivity_analysis_3_network
# simulation_sensitivity_analysis_mask_effectiveness_70
# simulation_sensitivity_analysis_static_3day / simulation_sensitivity_analysis_temporal_3day 
# simulation_sensitivity_analysis_infection_param

png('figure/simulation_sensitivity_analysis_ve_50_inf_50_presymp_25.png',height=16,width=24,units='cm',res=300,pointsize=10)
pushViewport(plotViewport(c(2,2,2,1)))
pushViewport(viewport(layout=grid.layout(nrow=2,ncol=3)))

paneller(1,1)
paneller(1,2)
paneller(1,3)

paneller(2,1)
paneller(2,2)
paneller(2,3)

grid.text('Vaccine coverage (%)',y=unit(-1,'lines'))

popViewport()
popViewport()
dev.off()


# analyse simulation outcomes
# baseline
data = resultsStatic_set_1[scenario=='nothing_none_vac_0_mask_0']
summary(data[,.N,by=.(paramset)]$N-1)
length(which(data[,.N,by=.(paramset)]$N-1 >=11))

data[onset<=7,event_onset:='before']
data[onset>7,event_onset:='after']
onsetTab = data[generation!=1, .N, by=.(paramset,event_onset)]
onsetTab = dcast(onsetTab, paramset~event_onset, value.var = 'N')
onsetTab[is.na(onsetTab)] = 0
onsetTab[,N:=before+after]
onsetTab[,after_prop:=after*100/N]
summary(onsetTab$after_prop)

# once-off PCR
data = resultsStatic_set_1[scenario=='isolation_PCR_once_vac_0_mask_0']

data[generation==1 & isolated_time==1,.N]
data[generation==1 & isolated_time==1,paramset]

length(which(data[!(paramset %in% data[generation==1 & isolated_time==1,paramset]),.N, by=.(paramset)]$N-1 == 0))
length(which(data[!(paramset %in% data[generation==1 & isolated_time==1,paramset]),.N, by=.(paramset)]$N-1 >= 11))

# daily antigen
data = resultsStatic_set_1[scenario=='isolation_Ag_twice_vac_0_mask_0']

data[generation==1 & isolated_time==1,.N]
data[generation==1 & isolated_time==1,paramset]

length(which(data[!(paramset %in% data[generation==1 & isolated_time==1,paramset]),.N, by=.(paramset)]$N-1 == 0))
length(which(data[!(paramset %in% data[generation==1 & isolated_time==1,paramset]),.N, by=.(paramset)]$N-1 >= 11))

# mask
data = resultsStatic_set_1[scenario=='nothing_none_vac_0_mask_1']
summary(data[,.N,by=.(paramset)]$N-1)
length(which(data[,.N,by=.(paramset)]$N-1 >=11))

# vaccinate
data = resultsStatic_set_1[scenario=='nothing_none_vac_1_mask_0']
summary(data[,.N,by=.(paramset)]$N-1)
length(which(data[,.N,by=.(paramset)]$N-1 <=5))


# spillover generation
data = resultsStatic_set_1[scenario=='nothing_none_vac_0_mask_0']
data[dataNodes, infector_cohort:=i.COHORT, on=c(infector='ID')]
data[dataNodes, case_cohort:=i.COHORT, on=c(caseid='ID')]

# find passenger to crew 
data[infector_cohort == 'PASSENGER' & case_cohort != 'PASSENGER', pass_to_crew:=1]
data[pass_to_crew==1,uniqueN(paramset)]

summary(data[pass_to_crew==1, min(generation), by=.(paramset)]$V1)


# find crew to crew different cohort transmission
data[infector_cohort != 'PASSENGER' & case_cohort != 'PASSENGER' & infector_cohort!= case_cohort, crew_to_crew_cross_cohort:=1]
data[crew_to_crew_cross_cohort==1,uniqueN(paramset)]

summary(data[crew_to_crew_cross_cohort==1, min(generation), by=.(paramset)]$V1)


# combination
data = resultsStatic_set_1

resultsSize_set_1 = data.table(scenario = rep(unique(data$scenario),  each=1000),
                         paramset = rep(1:1000, times = uniqueN(data$scenario)))

resultsSize_set_1[data[exposure<=7,.N,by=.(scenario,paramset)], N:=i.N, 
            on=c(scenario='scenario',paramset='paramset')]
resultsSize_set_1[,N:=N-1]
resultsSize_set_1[is.na(N),N:=0]
resultsSize_set_1 = resultsSize_set_1[,.(mean(N)), by=.(scenario)]

resultsSize_set_1[,mask:=gsub('.*_(mask_\\d)', '\\1', scenario)]
resultsSize_set_1[,scenario:=gsub('(.*)_mask_\\d', '\\1', scenario)]
resultsSize_set_1 = dcast(resultsSize_set_1, scenario~mask, value.var = 'V1')
resultsSize_set_1[,diff:=(mask_0-mask_1)*100/mask_0]
summary(resultsSize_set_1$diff)


data = resultsStatic_set_2

resultsSize_set_2 = data.table(scenario = rep(unique(data$scenario),  each=1000),
                               paramset = rep(1:1000, times = uniqueN(data$scenario)))

resultsSize_set_2[data[exposure<=7,.N,by=.(scenario,paramset)], N:=i.N, 
                  on=c(scenario='scenario',paramset='paramset')]
resultsSize_set_2[,N:=N-1]
resultsSize_set_2[is.na(N),N:=0]
resultsSize_set_2 = resultsSize_set_2[,.(mean(N)), by=.(scenario)]

resultsSize_set_2[,mask:=gsub('.*_(mask_\\d)', '\\1', scenario)]
resultsSize_set_2[,scenario:=gsub('(.*)_mask_\\d', '\\1', scenario)]
resultsSize_set_2 = dcast(resultsSize_set_2, scenario~mask, value.var = 'V1')
resultsSize_set_2[,diff:=(mask_0-mask_1)*100/mask_0]
summary(resultsSize_set_2$diff)



data = resultsStatic_set_3

resultsSize_set_3 = data.table(scenario = rep(unique(data$scenario),  each=1000),
                               paramset = rep(1:1000, times = uniqueN(data$scenario)))

resultsSize_set_3[data[exposure<=7,.N,by=.(scenario,paramset)], N:=i.N, 
                  on=c(scenario='scenario',paramset='paramset')]
resultsSize_set_3[,N:=N-1]
resultsSize_set_3[is.na(N),N:=0]
resultsSize_set_3 = resultsSize_set_3[,.(mean(N)), by=.(scenario)]

resultsSize_set_3[,mask:=gsub('.*_(mask_\\d)', '\\1', scenario)]
resultsSize_set_3[,scenario:=gsub('(.*)_mask_\\d', '\\1', scenario)]
resultsSize_set_3 = dcast(resultsSize_set_3, scenario~mask, value.var = 'V1')
resultsSize_set_3[,diff:=(mask_0-mask_1)*100/mask_0]
summary(resultsSize_set_3$diff)

summary(c(resultsSize_set_1$diff,resultsSize_set_2$diff,resultsSize_set_3$diff))

# mask off once pcr
resultsSize_set_3[6:10]$mask_0

# mask on no testing
resultsSize_set_3[11:15]$mask_1

(resultsSize_set_3[11:15]$mask_1-resultsSize_set_3[6:10]$mask_0)/resultsSize_set_3[11:15]$mask_1


# mask on pcr
resultsSize_set_3[6:10]$mask_1

# mask off antigen
resultsSize_set_3[1:5]$mask_0


(resultsSize_set_3[6:10]$mask_1-resultsSize_set_3[1:5]$mask_0)/resultsSize_set_3[6:10]$mask_1