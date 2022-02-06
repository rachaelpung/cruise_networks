source('github/code/cruise_load_library.R')

load('github/data/dataNodes.RData')
colnames(dataNodes)

# mean and range no. of passenger and crew
dataNodes[TYPE=='P',.N,by=.(SAIL)][,.(mean(N),min(N),max(N))]
dataNodes[TYPE=='C',.N,by=.(SAIL)][,.(mean(N),min(N),max(N))]


# median and IQR no. of passenger age
dataNodes[TYPE=='P',.(quantile(AGE, probs=0.5, na.rm = T),
                         quantile(AGE, probs=0.25, na.rm = T),
                         quantile(AGE, probs=0.75, na.rm = T))]


dataNodes[TYPE=='P' & AGE<12,.N,by=.(SAIL)][,.(mean(N),min(N),max(N))] 
dataNodes[TYPE=='P' & AGE>=12 & AGE<=29,.N,by=.(SAIL)][,.(mean(N),min(N),max(N))] 
dataNodes[TYPE=='P' & AGE>=30 & AGE<=39,.N,by=.(SAIL)][,.(mean(N),min(N),max(N))] 
dataNodes[TYPE=='P' & AGE>=40 & AGE<=49,.N,by=.(SAIL)][,.(mean(N),min(N),max(N))] 
dataNodes[TYPE=='P' & AGE>=50 & AGE<=59,.N,by=.(SAIL)][,.(mean(N),min(N),max(N))] 
dataNodes[TYPE=='P' & AGE>=60 & AGE<=69,.N,by=.(SAIL)][,.(mean(N),min(N),max(N))] 
dataNodes[TYPE=='P' & AGE>=70,.N,by=.(SAIL)][,.(mean(N),min(N),max(N))] 


# median and IQR no. of passenger gender
dataNodes[TYPE=='P',.N,by=.(SAIL,GENDER)][,.(mean(N),min(N),max(N)), by=.(GENDER)] 


# median and IQR no. of crew by department
dataNodes[TYPE=='C',.N,by=.(SAIL, COHORT)][,.(mean(N),min(N),max(N)), by=.(COHORT)] 


# median and IQR weighted degree of passenger and crew
dataNodes[TYPE=='P',.(quantile(STRENGTH, probs=0.5, na.rm = T),
                      quantile(STRENGTH, probs=0.25, na.rm = T),
                      quantile(STRENGTH, probs=0.75, na.rm = T))]

dataNodes[TYPE=='C',.(quantile(STRENGTH, probs=0.5, na.rm = T),
                      quantile(STRENGTH, probs=0.25, na.rm = T),
                      quantile(STRENGTH, probs=0.75, na.rm = T))]

t.test(dataNodes[TYPE=='P', STRENGTH], dataNodes[TYPE=='C', STRENGTH], alternative = "two.sided", var.equal = FALSE)

# median and IQR eigenvector centrality of passenger and crew
dataNodes[TYPE=='P',.(quantile(EIGEN, probs=0.5, na.rm = T),
                      quantile(EIGEN, probs=0.25, na.rm = T),
                      quantile(EIGEN, probs=0.75, na.rm = T))]

dataNodes[TYPE=='C',.(quantile(EIGEN, probs=0.5, na.rm = T),
                      quantile(EIGEN, probs=0.25, na.rm = T),
                      quantile(EIGEN, probs=0.75, na.rm = T))]

t.test(dataNodes[TYPE=='P', EIGEN], dataNodes[TYPE=='C', EIGEN], alternative = "two.sided", var.equal = FALSE)


# median and IQR clustering coefficient of passenger and crew
dataNodes[TYPE=='P',.(quantile(CC, probs=0.5, na.rm = T),
                      quantile(CC, probs=0.25, na.rm = T),
                      quantile(CC, probs=0.75, na.rm = T))]

dataNodes[TYPE=='C',.(quantile(CC, probs=0.5, na.rm = T),
                      quantile(CC, probs=0.25, na.rm = T),
                      quantile(CC, probs=0.75, na.rm = T))]

t.test(dataNodes[TYPE=='P', CC], dataNodes[TYPE=='C', CC], alternative = "two.sided", var.equal = FALSE)
