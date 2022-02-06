source('github/code/cruise_load_library.R')

# load nodes
load('github/data/dataNodes.RData')

# list edgelist files
listFile = dir('github/data/edgelist weighted/')

# analyse network properties
nodeListAll = data.table()

for(sail in 1:4){
  
  load(paste('github/data/edgelist weighted/', listFile[sail], sep = ''))
  
  # create graph from nodes and edgelist
  nodeList = copy(dataNodes[SAIL==sail])
  edgeList = copy(edgeListStatic[['edgeListStatic.trans.long']])
  edgeList = edgeList[duplicated(edgeList) == FALSE]
  graph = graph_from_data_frame(edgeList, directed = TRUE, vertices = nodeList)
  graph = as.undirected(graph, mode ='collapse', edge.attr.comb="first")
  
  edge_attr_names(graph)
  print(graph, e=TRUE, v=TRUE)
  
  nodeList[, DEGREE:=degree(graph, v=ID)]
  nodeList[, STRENGTH:=strength(graph, v=ID, weights=E(graph)$WEIGHT)]
  nodeList[, EIGEN:=eigen_centrality(graph)$vector]
  nodeList[, BETWEEN:=betweenness(graph, v=ID, directed = FALSE)]
  nodeList[, CC:=transitivity(graph,"weighted", weights=E(graph)$WEIGHT)]
  
  # graph component
  component = decompose(graph)
  component = sapply(1:length(component), function(x){
    ID = V(component[[x]])$name
    cbind(ID, COMPONENT = rep(x, length(ID)))
  }, simplify = FALSE)
  component = do.call('rbind',component)
  component = data.table(component)
  component = component[!duplicated(component)]
  
  nodeList[component, COMPONENT:=i.COMPONENT, on=c(ID='ID')]
  
  nodeListAll = rbind(nodeListAll,nodeList)
  
}




dataNodes = copy(nodeListAll)
save(dataNodes, file = 'github/data/dataNodes_test.RData')

