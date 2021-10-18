source('code/cruise_load_library.R')

load('github/data/dataNodes.RData')
load('github/data/edgelist weighted/edgeListStatic_sail_1.RData')
load('github/data/outputs/resultsStatic_set_1.RData')

# select sailing
dataNodes = dataNodes[SAIL==1]

# select edgelist
outbreak.edge = edgeListStatic$edgeListStatic.trans.long

# select outbreak to plot
unique(resultsStatic_set_1$scenario)
resultsStatic_set_1[scenario == 'nothing_none_vac_0_mask_0',.N, by=.(paramset)]
outbreak.result = resultsStatic_set_1[scenario == 'nothing_none_vac_0_mask_0' & paramset == 27]

# identify nodes of cases and contacts
outbreak.nodes.cases = outbreak.result$caseid
outbreak.nodes.contacts = outbreak.edge[ID.x %in% outbreak.nodes.cases]$ID.y


outbreak.nodes = unique(c(outbreak.nodes.cases, outbreak.nodes.contacts))
outbreak.nodes = dataNodes[ID %in% outbreak.nodes]

# merge with outbreak results
outbreak.nodes = merge(outbreak.nodes, outbreak.result, by.x = 'ID', by.y = 'caseid', all.x = T)

edgelist=outbreak.edge[ID.x %in% outbreak.nodes$ID & ID.y %in% outbreak.nodes$ID ]
nodelist=outbreak.nodes


draw.outbreak<-function(edgelist,nodelist,cohort.x=NULL,cohort.y=NULL, main.label=NULL){
  
  range.use<-function(x,min.use,max.use){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (max.use - min.use) + min.use } #define for later
  
  if(!is.null(cohort.x) & !is.null(cohort.y)){
    if(cohort.x == 'PASSENGER' & cohort.y == 'PASSENGER'){
      edgelist=edgelist[CONTACT_GROUP %in% c(0,1)]
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
  
  col.rearrange = which(!(colnames(nodelist) %in% c('v')))
  nodelist=cbind(nodelist[,.(v)], nodelist[,col.rearrange,with=FALSE])
  
  
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
  edgew<-range.use(edgew,0.5,1.5)
  
  #make size of nodes
  if(!is.null(cohort.x) & !is.null(cohort.y)){
    vert.sizes<-3 # rep(3,gorder(graph))
    if(cohort.x == 'PASSENGER' & cohort.y == 'PASSENGER'){
      vert.sizes<-1.5 # rep(1.5,gorder(graph))
    }
  } else{
    vert.sizes<-2 # rep(2,gorder(graph))
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
    
    vert.sizes<-5 # rep(5,gorder(graph))
   
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
  
  # assign infected nodes and edges red colours
  vert.outline[!is.na(nodelist$generation)]<-makeTrans("indianred1",alpha=0.7)
  # vert.bg[!is.na(nodelist$generation)]<-makeTrans("indianred1",alpha=0.7)
  
  vert.sizes<-rep(1.5,gorder(graph))
  vert.sizes[!is.na(nodelist$generation)]<-3
  
  #infection arrows
  vert.cases = nodelist[!is.na(generation)]
  vert.cases = merge(vert.cases, edgelist, by.x = c('ID', 'infector'), by.y = c('ID.x','ID.y') , all.x =T)
  edgelist.cases = vert.cases[,.(v.y,v.x,WEIGHT)] # y is infector, x is infected contact
  setnames(edgelist.cases, c("infector","v","weight"))
  
  edgelist.cases<-edgelist.cases[!is.na(infector)]
  edgelist.cases$x0<-graph.lay[edgelist.cases$infector,1]
  edgelist.cases$y0<-graph.lay[edgelist.cases$infector,2]
  edgelist.cases$x1<-graph.lay[edgelist.cases$v,1]
  edgelist.cases$y1<-graph.lay[edgelist.cases$v,2]
  

  
  
  #Plotting
  plot.igraph(graph,layout=graph.lay,rescale=F,edge.width=edgew,edge.curved=T,
              edge.color=edge.cols,vertex.label=vert.labels,
              vertex.label.color=vert.labels.cols,
              vertex.label.cex=vert.labels.sizes,
              vertex.size=vert.sizes,vertex.color=vert.bg,
              vertex.frame.color=vert.outline,vertex.shape=vert.shapes,
              xlim=range.scale,ylim=range.scale)
  
  apply(edgelist.cases,1,function(a){curvedarrow(a[c("x0","y0")],a[c("x1","y1")],lwd=0.8,lty=1,lcol="firebrick3",arr.col="firebrick3",arr.pos=0.96,curve=0.3,dr=0.01,endhead = T, segment = c(0, 1),arr.type="curved",arr.length=0.25)})
  
  
  title(main=main.label,
        cex.main=3, adj=0, font.main=1, line=-2.5)
}


png('figure/networks/outbreak_arr.pos_0.96.png',height=24,width=24,units='cm',res=300,pointsize=10)
par(mfrow=c(1,1),mar=c(3,1,0,0),mgp=c(2,0.6,0),las=0)
draw.outbreak(outbreak.edge[ID.x %in% outbreak.nodes$ID & ID.y %in% outbreak.nodes$ID ],
              outbreak.nodes,main.label='A')

legend(x=0.5, y=-0.05, 
       legend = c('Entertainment', 'Gaming', 'Marine', 
                  'F&B', 'Hotel', 'Security',
                  'Galley', 'Housekeep','Passenger',
                  '','Edge',''), 
       col = c("#4E79A7","#76B7B2","#B07AA1",
               "#F28E2B","#59A14F","#FF9DA7",
               "#E15759","#EDC948","#9C7557",
               'white','azure3','white'), 
       pch = c(19,19,19,19,19,19,19,19,19,15,15,15), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       inset = 0,
       ncol=4,
       text.width=0.25)
dev.off()


set.seed(123)
svg('figure/networks/outbreak_arr.pos_0.96_seed123.svg',height=24,width=24,pointsize=10)
par(mfrow=c(1,1),mar=c(3,1,0,0),mgp=c(2,0.6,0),las=0)
              outbreak.nodes,main.label='A')

legend(x=0.5, y=-0.05, 
       legend = c('Entertainment', 'Gaming', 'Marine', 
                  'F&B', 'Hotel', 'Security',
                  'Galley', 'Housekeep','Passenger',
                  '','Edge',''), 
       col = c("#4E79A7","#76B7B2","#B07AA1",
               "#F28E2B","#59A14F","#FF9DA7",
               "#E15759","#EDC948","#9C7557",
               'white','azure3','white'), 
       pch = c(19,19,19,19,19,19,19,19,19,15,15,15), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       inset = 0,
       ncol=4,
       text.width=0.2)
dev.off()

