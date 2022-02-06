source('github/code/cruise_load_library.R')
source('github/code/outbreak_step3b_param.R')
source('github/code/outbreak_step3c_functions.R')
source('github/code/outbreak_step3d_setup.R')
source('github/code/outbreak_step3e_step.R')
source('github/code/outbreak_step3f_model.R')
source('github/code/outbreak_step3g_sim_parallel.R')

# outbreak by static/temporal network
paramList=paramTemporal # paramStatic, paramTemporal
edgeList=edgeListTemporal[['edgeList.trans.long']] # edgeListStatic[['edgeList.trans.long']]

# set the presymtomatic rate and scales for FOI based on type of edges
paramList[,presymrate:=0.25] # 0.25, 0.5
paramList[,edge:='edgeList.trans.long'] # edgeList.trans.long, edgeList.trans.short, edgeList.prob
paramList=merge(paramList, foi_scale, by=c('edge'), all.x = T)

set.seed(123)
## set up clusters
cl = makeCluster(detectCores())
clusterExport(cl, as.vector(lsf.str()))
registerDoParallel(cl)

results = foreach(p = 1:paramList[,.N], .packages = c('sn', 'dplyr', 'purrr', 'data.table'), .combine = 'rbind') %dopar%  {
  
  paramSet = paramList[p,]
  
  sim.output <- scenario_sim_parallel(param.set = paramSet$param.set, n.sim = paramSet$n.sim,
                                      net = edgeList, net.type = paramSet$net.type, 
                                      num.initial.cases = paramSet$num.initial.cases, initial.cases = paramSet$initial.cases,
                                      initial.cases.onset = paramSet$initial.cases.onset,
                                      prop.asym = paramSet$prop.asym, presymrate = paramSet$presymrate,
                                      prop.ascertain = paramSet$prop.ascertain, cap_max_days = paramSet$cap_max_days,
                                      delay_shape = paramSet$delay_shape, delay_scale = paramSet$delay_scale, 
                                      intervention = paramSet$intervention, testing.regime = paramSet$testing.regime,
                                      null.net = paramSet$null.net, outside = paramSet$outside,  
                                      distancing = paramSet$distancing, 
                                      edge_scale = paramSet[,.(R_scaling_factor, vac.coverage, mask,
                                                               inf_mask, inf_vac_infector, inf_vac_infectee, 
                                                               inf_vac_both, inf_mask_vac_infector,
                                                               inf_mask_vac_infectee, inf_mask_vac_both)],
                                      scenario = paramSet$scenario)
  
 

}

# stop clusters
stopCluster(cl) 

# save results
results = data.table(results)
resultsTemporal_set_19 = copy(results)
save(resultsTemporal_set_19, file = 'github/data/outputs/resultsTemporal_set_19.RData')





