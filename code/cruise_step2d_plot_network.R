source('github/code/cruise_load_library.R')

load('github/data/dataNodes.RData')
load('github/data/edgelist weighted/edgeListStatic_sail_1.RData')

dataNodes = dataNodes[SAIL==1]

edgeListStatic.trans.long = edgeListStatic[['edgeList.trans.long']]
edgeListStatic.trans.long[dataNodes, COHORT.x:=i.COHORT, on=c(ID.x='ID')]
edgeListStatic.trans.long[dataNodes, COHORT.y:=i.COHORT, on=c(ID.y='ID')]
edgeListStatic.trans.long[,unique(COHORT.x)]

draw.network<-function(edgelist,nodelist,cohort.x=NULL,cohort.y=NULL, main.label=NULL){
  
  range.use<-function(x,min.use,max.use){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (max.use - min.use) + min.use } #define for later
  
  if(!is.null(cohort.x) & !is.null(cohort.y)){
    if(cohort.x == 'PASSENGER' & cohort.y == 'PASSENGER'){
      # edgelist=edgelist[CONTACT_GROUP %in% c(0,1)]
      edgelist=edgelist[COHORT.x == cohort.x & COHORT.y == cohort.y]
    } else{
      edgelist=edgelist[COHORT.x == cohort.x & COHORT.y == cohort.y]
    }
  } else{
    edgelist=edgelist
  }
  
  if(!is.null(cohort.x) & !is.null(cohort.y)){
    nodelist=nodelist[COHORT %in% c(cohort.x, cohort.y)]
  } else{
    nodelist=nodelist
  }
    
  nodelist[,v:=seq_len(.N)]
  nodelist=nodelist[,c(14,1:13)]
  
  #arrange data:
  edgelist[nodelist, v.x:=i.v, on=c(ID.x='ID')]
  edgelist[nodelist, v.y:=i.v, on=c(ID.y='ID')]
  
  col.rearrange = which(!(colnames(edgelist) %in% c('v.x','v.y','WEIGHT')))
  edgelist=cbind(edgelist[,.(v.x,v.y,WEIGHT)], edgelist[,col.rearrange,with=FALSE])
  

  #make the needed igraph object from the edgelist
  graph = graph_from_data_frame(edgelist, directed = TRUE, vertices = nodelist)
  graph = as.undirected(graph, mode ='collapse', edge.attr.comb="first")
  
  graph.lay<-layout_nicely(graph)
  
  # make circular
  ps<-nrow(graph.lay)
  dim.tl<-ceiling(sqrt(ps))*2 #needs *2 just to make sure we have enough points
  #grid.lay<-expand.grid(floor(-dim.tl/2):ceiling(dim.tl/2),((floor(-dim.tl/2))-0.5):(ceiling((dim.tl/2))+0.5))
  xpoints1<-floor(-dim.tl/2):ceiling(dim.tl/2)
  xpoints2<-xpoints1+0.5
  ypoints1<-xpoints1
  ypoints2<-xpoints2
  grid.lay1<-expand.grid(xpoints2,ypoints1)
  grid.lay2<-expand.grid(xpoints1,ypoints2)
  grid.lay3<-expand.grid(xpoints1,ypoints1)
  grid.lay4<-expand.grid(xpoints2,ypoints2)
  grid.lay<-rbind(grid.lay1,grid.lay2,grid.lay3,grid.lay4)
  grid.lay[,1]<-jitter(grid.lay[,1],1)
  grid.lay[,2]<-jitter(grid.lay[,2],1)
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  grid.lay[,3]<-apply(grid.lay,1,function(a)euc.dist(matrix(c(mean(grid.lay[,1]),mean(grid.lay[,2])),1,2),a))
  max.dist<-grid.lay[,3][((1:length(unique(grid.lay[,3])))[match(grid.lay[,3],sort(unique(grid.lay[,3])))])==ps]
  g.lay.c<-grid.lay[grid.lay[,3]<=max.dist,]
  g.lay.c[,1]<-scale(g.lay.c[,1])[,1]
  g.lay.c[,2]<-scale(g.lay.c[,2])[,1]
  graph.lay[,1]<-scale(graph.lay[,1])[,1]
  graph.lay[,2]<-scale(graph.lay[,2])[,1]
  distances <- cdist(graph.lay[,1:2],g.lay.c[,1:2])
  sol <- solve_LSAP(t(distances))
  graph.lay[as.numeric(sol),1:2]<-as.matrix(g.lay.c[,1:2])
  
  #standardise graph.lay into desired range
  range.scale<-c(0,2)
  graph.lay<-apply(graph.lay,2,function(a)range.use(a,min(range.scale),max(range.scale)))
  
  if(edgelist[,.N]==2){
    
    graph.lay[2,2]<-1.5
    graph.lay[1,1]<-1.75
  }
  
  #Plotting information from here
  makeTrans<-function(..., alpha=0.5) {
    if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
    alpha = floor(255*alpha)
    newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
    .makeTrans = function(col, alpha) {
      rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)}
    newColor = apply(newColor, 2, .makeTrans, alpha=alpha)
    return(newColor)}
  
  #edge info
  edgew<-E(graph)$WEIGHT
  edge.cols<- sapply(edgew, function(x){
    makeTrans("azure3",alpha=x)  # cornsilk3  
  })
  # edgew<-range.use(edgew,0.5,1.5)
  edgew<-range.use(edgew,1,1.5)
  
  #make size of nodes
  if(!is.null(cohort.x) & !is.null(cohort.y)){
    vert.sizes<-3
    if(cohort.x == 'PASSENGER' & cohort.y == 'PASSENGER'){
      vert.sizes<-1.5
    }
  } else{
    vert.sizes<-1.5
  }
  
  #make vert colours
  vert.bg<-makeTrans(rep('darkgrey',gorder(graph)),alpha=0)
  vert.bg[nodelist$COHORT=='CRUISE & ENT']<-"#4E79A7"
  vert.bg[nodelist$COHORT=='F&B']<-"#F28E2B"
  vert.bg[nodelist$COHORT=='GALLEY']<-"#E15759"
  vert.bg[nodelist$COHORT=='GAMING']<-"#76B7B2"
  vert.bg[nodelist$COHORT=='HOTEL SMALL']<-"#59A14F"
  vert.bg[nodelist$COHORT=='HSKP & LAUNDRY']<-"#EDC948"
  vert.bg[nodelist$COHORT=='MARINE']<-"#B07AA1"
  vert.bg[nodelist$COHORT=='CAGE & SURV']<-"#FF9DA7"
  vert.bg[nodelist$COHORT=='PASSENGER']<-"#9C7557"
  # vert.bg<-makeTrans(vert.bg,alpha=1)
  
  vert.outline<-makeTrans(rep('darkgrey',gorder(graph)),alpha=0)
  vert.outline[nodelist$COHORT=='CRUISE & ENT']<-"#4E79A7"
  vert.outline[nodelist$COHORT=='F&B']<-"#F28E2B"
  vert.outline[nodelist$COHORT=='GALLEY']<-"#E15759"
  vert.outline[nodelist$COHORT=='GAMING']<-"#76B7B2"
  vert.outline[nodelist$COHORT=='HOTEL SMALL']<-"#59A14F"
  vert.outline[nodelist$COHORT=='HSKP & LAUNDRY']<-"#EDC948"
  vert.outline[nodelist$COHORT=='MARINE']<-"#B07AA1"
  vert.outline[nodelist$COHORT=='CAGE & SURV']<-"#FF9DA7"
  vert.outline[nodelist$COHORT=='PASSENGER']<-"#9C7557"
  
  # make PASSENGER nodes transparent based on time spend in location
  if('TIME_SPEND_BIN' %in% colnames(edgelist)){
    
    vert.time.spend<-E(graph)$TIME_SPEND_BIN
    vert.time.spend<-range.use(vert.time.spend,0.2,1)
    
    # debug plotting of shops contacts
    if(length(vert.time.spend) == 1) vert.time.spend<-0.2
    
    vert.bg <- sapply(vert.time.spend, function(x){
      makeTrans("#9C7557",alpha=x)
    })
    
    vert.outline <- sapply(vert.time.spend, function(x){
      makeTrans("#9C7557",alpha=x)
    })
    
    vert.sizes<-5
  }
  

  vert.shapes<-rep("circle",gorder(graph))
  
  vert.labels<-rep(NA,gorder(graph))
  vert.labels.cols<-"deepskyblue1";vert.labels.sizes<-1.5
  
  # if(!is.null(cohort.x)){
  #   main.label = c('A','B','C','D','E','F','G','H','I')
  #   main.label.cohort = c('CRUISE & ENT', 'F&B', 'GALLEY', 'GAMING', 'HOTEL SMALL', 
  #                         'HSKP & LAUNDRY', 'MARINE','CAGE & SURV', 'PASSENGER')
  #   main.label[match(cohort.x, main.label.cohort)]
  # }
  
  
  #Plotting
  plot.igraph(graph,layout=graph.lay,rescale=F,edge.width=edgew,edge.curved=T,
              edge.color=edge.cols,vertex.label=vert.labels,
              vertex.label.color=vert.labels.cols,
              vertex.label.cex=vert.labels.sizes,
              vertex.size=vert.sizes,vertex.color=vert.bg,
              vertex.frame.color=vert.outline,vertex.shape=vert.shapes,
              xlim=range.scale,ylim=range.scale)

  title(main=main.label,
        cex.main=2, adj=0, font.main=1, line=-2.5)
}

inches = 0.393701
set.seed(123)
pdf('figure/nature comms pdf/s2_networks_seed321_all_pass.pdf',height=24*inches,width=24*inches,pointsize=10)
# png('figure/networks/networks_seed321_all_pass.png',height=24,width=24,units='cm',res=300,pointsize=10)
par(mfrow=c(3,3),mar=c(3,1,0,0),mgp=c(2,0.6,0),las=0)

draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='CRUISE & ENT',cohort.y='CRUISE & ENT',main.label='a')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='F&B',cohort.y='F&B',main.label='b')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='GALLEY',cohort.y='GALLEY',main.label='c')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='GAMING',cohort.y='GAMING',main.label='d')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='HOTEL SMALL',cohort.y='HOTEL SMALL',main.label='e')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='HSKP & LAUNDRY',cohort.y='HSKP & LAUNDRY',main.label='f')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='MARINE',cohort.y='MARINE',main.label='g')

draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='CAGE & SURV',cohort.y='CAGE & SURV',main.label='h')
legend(x=0.2, y=0, 
       legend = c('Entertainment', 'Gaming', 'Marine', 
                  'F&B', 'Hotel', 'Security',
                  'Galley', 'Housekeep','Passenger'), 
       col = c("#4E79A7","#76B7B2","#B07AA1",
               "#F28E2B","#59A14F","#FF9DA7",
               "#E15759","#EDC948","#9C7557"), 
       pch = c(19,19,19,19,19,19,19,19,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       inset = 0,
       ncol=4,
       text.width=0.45)

draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='PASSENGER',cohort.y='PASSENGER',main.label='i')
dev.off()




set.seed(321)
svg('figure/networks/networks_seed321.svg',height=24,width=24,pointsize=10)
par(mfrow=c(3,3),mar=c(3,1,0,0),mgp=c(2,0.6,0),las=0)

draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='CRUISE & ENT',cohort.y='CRUISE & ENT',main.label='A')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='F&B',cohort.y='F&B',main.label='B')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='GALLEY',cohort.y='GALLEY',main.label='C')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='GAMING',cohort.y='GAMING',main.label='D')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='HOTEL SMALL',cohort.y='HOTEL SMALL',main.label='E')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='HSKP & LAUNDRY',cohort.y='HSKP & LAUNDRY',main.label='F')
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='MARINE',cohort.y='MARINE',main.label='G')

draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='CAGE & SURV',cohort.y='CAGE & SURV',main.label='H')
legend(x=0.45, y=0.03, 
       legend = c('Entertainment', 'Gaming', 'Marine', 
                  'F&B', 'Hotel', 'Security',
                  'Galley', 'Housekeep','Passenger'), 
       col = c("#4E79A7","#76B7B2","#B07AA1",
               "#F28E2B","#59A14F","#FF9DA7",
               "#E15759","#EDC948","#9C7557"), 
       pch = c(19,19,19,19,19,19,19,19,19), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.5, 
       text.col = "black", 
       inset = 0,
       ncol=4,
       text.width=0.35)

draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='PASSENGER',cohort.y='PASSENGER',main.label='I')
dev.off()



# png('figure/networks/networks_legend.png',height=8,width=8,units='cm',res=300,pointsize=10)
par(mfrow=c(1,1),mar=c(3,0,0,0),mgp=c(2,0.6,0),las=0)
# plot(NULL,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
draw.network(edgeListStatic.trans.long,dataNodes,cohort.x='CAGE & SURV',cohort.y='CAGE & SURV',main.label='H')

legend(x=0.2, y=0, 
       legend = c('Entertainment', 'Gaming', 'Marine', 
                  'F&B', 'Hotel', 'Security',
                  'Galley', 'Housekeep','Passenger'), 
       col = c("#4E79A7","#76B7B2","#B07AA1",
               "#F28E2B","#59A14F","#FF9DA7",
               "#E15759","#EDC948","#9C7557"), 
       pch = c(19,19,19,19,19,19,19,19,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       inset = 0,
       ncol=4,
       text.width=0.45)
# dev.off()


