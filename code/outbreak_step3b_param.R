# load nodelist and edgelist
load('github/data/dataNodes.RData')
load('github/data/edgelist weighted/edgeListStatic_sail_1.RData')

# load onset distributions and FOI scale
load('github/data/onset/distOnset.3.RData')
load('github/data/onset/distOnset.7.RData')
load('github/data/foi/foi_scale_set_1_sail_1.RData')
# load('github/data/foi/foi_scale_set_2_sail_1.RData') # vaccine efficacy against transmitting infection is 0

dataNodes = dataNodes[SAIL==1]

# generate list of parameters  
set.seed(1)
param = data.table(param.set = 1:1000, n.sim=1, num.initial.cases = 1,
                   initial.cases = sample(dataNodes[TYPE == "P", ID], 1000, replace = TRUE),
                   prop.asym=0, prop.ascertain = 0.9999999, delay_shape = 1, delay_scale = 0.000001, 
                   null.net = "none", outside = 0, distancing = 0)

# parameters for static network transmission
paramStatic = copy(param)
paramStatic[,initial.cases.onset:=sample(distOnset.7$DAY_ONSET, num.initial.cases, prob = distOnset.7$PDF, replace = T), by = .(param.set)]
paramStatic[,cap_max_days:=7]

paramStatic=paramStatic[rep(seq_len(nrow(paramStatic)), times=3*2*5),]
paramStatic[,intervention:=rep(c("nothing", "isolation", "isolation"), each=1000, len = .N)]
paramStatic[,testing.regime:=rep(c("none", "PCR_once", "Ag_twice"), each=1000, len = .N)]
paramStatic[,vac.coverage:=rep(seq(0,1,0.25), each =3*1000, len = .N)]
paramStatic[,mask:=rep(c(0,1), each = .N/2)]
paramStatic[,net.type:='static']

paramStatic[,scenario:=paste(intervention,testing.regime,'vac',vac.coverage,'mask',mask, sep = '_')]
paramStatic[,.N,by=.(scenario)]

rm(param, distOnset.3, distOnset.7)