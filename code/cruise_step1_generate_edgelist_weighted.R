source('github/code/cruise_load_library.R')

# load data
load('github/data/dataNodes.RData')
load('github/data/dataContact.RData')

# extract sail of interest
sail=1
edgeList = copy(dataContact[SAIL==sail])
dataNodes = dataNodes[SAIL==sail]

# determine cumulative duration of contact by respective day
edgeList = edgeList[,sum(DURATION), by = .(ID.x, ID.y, DAY_INTERACT)]
edgeList = edgeList[order(ID.x, ID.y, DAY_INTERACT)]
setnames(edgeList, old = 'V1', new = 'CUM_DURATION')

# determine cumulative duration of contact over entire sail
edgeList[,CUM_DURATION_SAIL:=sum(CUM_DURATION), by = .(ID.x, ID.y)]

# categorise into different contact groups
# 1: P-P same travelling group
# 2: P-P different travelling group
# 3: C-C close working/social colleagues
# 4: C-C non-close working/social colleagues
# 5: P-C contacts (not stratified by department)
edgeList[dataNodes, CABIN_NO.y:=i.CABIN_NO, on=c(ID.y='ID')]
edgeList[dataNodes, CABIN_NO.x:=i.CABIN_NO, on=c(ID.x='ID')]
edgeList[,SAME_CABIN:=CABIN_NO.x-CABIN_NO.y]

edgeList[CUM_DURATION_SAIL>18000,SAME_GROUP:=1]
edgeList[SAME_CABIN==0,SAME_GROUP:=1]
edgeList[dataNodes, TYPE.y:=i.TYPE, on=c(ID.y='ID')]
edgeList[dataNodes, TYPE.x:=i.TYPE, on=c(ID.x='ID')]

edgeList[SAME_GROUP==1 & TYPE.x == 'P' & TYPE.y=='P' & SAME_CABIN==0,CONTACT_GROUP:=0]
edgeList[SAME_GROUP==1 & TYPE.x == 'P' & TYPE.y=='P' & SAME_CABIN!=0,CONTACT_GROUP:=1]
edgeList[is.na(SAME_GROUP) & TYPE.x == 'P' & TYPE.y=='P',CONTACT_GROUP:=2]
edgeList[SAME_GROUP==1 & TYPE.x == 'C' & TYPE.y=='C',CONTACT_GROUP:=3]
edgeList[is.na(SAME_GROUP) & TYPE.x == 'C' & TYPE.y=='C',CONTACT_GROUP:=4]
edgeList[(TYPE.x == 'P' & TYPE.y=='C') | (TYPE.x == 'C' & TYPE.y=='P'),CONTACT_GROUP:=5]
edgeList[is.na(CONTACT_GROUP),.N]


# set up storage for temporal network
edgeListTemporal = list(edgeList.trans.long=data.table(),
                        edgeList.trans.short=data.table(),
                        edgeList.prob=data.table())

# generate temporal network
edgeList[,CUM_DURATION_HALF_HR:=CUM_DURATION/1800]

edgeList[,WEIGHT:=(1-exp(-CUM_DURATION_HALF_HR))] # reach 95% saturation after 3 hour cumulative contact
edgeListTemporal[['edgeList.trans.long']] = edgeList[,.(ID.x,ID.y,CONTACT_GROUP,DAY_INTERACT,WEIGHT)]

edgeList[,WEIGHT:=1-exp(-3*CUM_DURATION_HALF_HR)] # reach 95% saturation after 1 hour cumulative contact
edgeListTemporal[['edgeList.trans.short']] = edgeList[,.(ID.x,ID.y,CONTACT_GROUP,DAY_INTERACT,WEIGHT)]

edgeList[,WEIGHT:=1]
edgeListTemporal[['edgeList.prob']] = edgeList[,.(ID.x,ID.y,CONTACT_GROUP,DAY_INTERACT,WEIGHT)]


save(edgeListTemporal, file = paste('github/data/edgelist weighted/edgeListTemporal_sail_', sail,'.Rdata', sep = ''))



# set up storage for static network
edgeListStatic = list(edgeList.trans.long=data.table(),
                      edgeList.trans.short=data.table(),
                      edgeList.prob=data.table())

# generate static network
edgeList = unique(edgeList, by=c('ID.x', 'ID.y', 'DAY_INTERACT', 'CONTACT_GROUP'))
edgeList = edgeList[,.(ID.x, ID.y, DAY_INTERACT, CONTACT_GROUP,CUM_DURATION)]

edgeList = edgeList[,.(uniqueN(DAY_INTERACT), mean(CUM_DURATION)), by=.(ID.x,ID.y,CONTACT_GROUP)]
setnames(edgeList, old = c('V1','V2'), new = c('DAY_INTERACT', 'MEAN_CUM_DURATION'))


edgeList[,MEAN_CUM_DURATION_HALF_HR:=MEAN_CUM_DURATION/1800]
edgeList[,PROB_DAY_INTERACT:=DAY_INTERACT/3]

edgeList[,WEIGHT:=PROB_DAY_INTERACT*(1-exp(-MEAN_CUM_DURATION_HALF_HR))] # reach 95% saturation in 3 hour
edgeListStatic[['edgeList.trans.long']] = edgeList[,.(ID.x,ID.y,CONTACT_GROUP,WEIGHT)]

edgeList[,WEIGHT:=PROB_DAY_INTERACT*(1-exp(-3*MEAN_CUM_DURATION_HALF_HR))] # reach 95% saturation in 1 hour
edgeListStatic[['edgeList.trans.short']] = edgeList[,.(ID.x,ID.y,CONTACT_GROUP,WEIGHT)]

edgeList[,WEIGHT:=PROB_DAY_INTERACT]
edgeListStatic[['edgeList.prob']] = edgeList[,.(ID.x,ID.y,CONTACT_GROUP,WEIGHT)]

save(edgeListStatic, file = paste('github/data/edgelist weighted/edgeListStatic_sail_', sail,'.Rdata', sep = ''))
rm(dataNodes, dataContact, edgeList)



# set up foi scale
foi_scale = data.table(edge = c('edgeList.trans.long', 'edgeList.trans.short', 'edgeList.prob'),
                       R_scaling_factor = NA,
                       inf_mask = NA, # both mask and unvaccinated
                       inf_vac_infector = NA, # both unmasked, vaccinated infector, unvaccinated infectee
                       inf_vac_infectee = NA, # both unmasked, unvaccinated infector, vaccinated infectee
                       inf_vac_both = NA, # both unmasked, both vaccinated 
                       inf_mask_vac_infector = NA, # both masked, vaccinated infector, unvaccinated infectee
                       inf_mask_vac_infectee = NA, # both masked, unvaccinated infector, vaccinated infectee
                       inf_mask_vac_both = NA)  # both masked, both vaccinated 

# household attack rate of delta
# SARS-CoV-2 variants of concern and variants under investigation in England: technical briefing 13
# Probability of infection with no mask https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)31142-9/fulltext
# NCID study
hh.ar = 0.2

for(i in 1:3){
  
  edgeList = copy(edgeListStatic[[foi_scale[i,edge]]])
  
  # tune R scaling factor such that 
  # prob inf for persons in same cabin is 20% over entire duration of infectiousness
  find_R_scaling_factor <- function(r, prob_inf, list){
    
    list[,FOI:=WEIGHT*r]
    list[,PROB_INF := 1-exp(-FOI)]
    diff = mean(list[CONTACT_GROUP==0,PROB_INF]) - prob_inf
    
    return(diff)
  }
  
  foi_scale$R_scaling_factor[i] = uniroot(find_R_scaling_factor, c(0, 1), tol = 0.001, prob_inf = hh.ar, list = edgeList)$root
  
  
  # tune inf_mask such that prob inf for persons in same cabin is reduced
  # by about 80% over entire duration of infectiousness due to mask wearing alone
  # https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)31142-9/fulltext
  find_inf_mask <- function(inf_mask, r, prob_inf, list){
    
    list[,FOI:=WEIGHT*inf_mask*r]
    list[,PROB_INF := 1-exp(-FOI)]
    diff = mean(list[CONTACT_GROUP==0,PROB_INF]) - prob_inf
    
    return(diff)
  }
  
  foi_scale$inf_mask[i] = uniroot(find_inf_mask, c(0, 1), tol = 0.001, 
                                  r = foi_scale$R_scaling_factor[i], prob_inf = (0.031/0.174)*hh.ar, list = edgeList)$root
                                                                                # (0.031/0.174) or 0.3
  
  # tune inf_vac_infector such that prob inf for persons in same cabin is reduced
  # by about 0 or 50% over entire duration of infectiousness due to vaccination of infector alone
  # https://www.gov.uk/government/news/one-dose-of-covid-19-vaccine-can-cut-household-transmission-by-up-to-half
  find_inf_vac_infector <- function(inf_vac_infector, r, prob_inf, list){

    list[,FOI:=WEIGHT*inf_vac_infector*r]
    list[,PROB_INF := 1-exp(-FOI)]
    diff = mean(list[CONTACT_GROUP==0,PROB_INF]) - prob_inf

    return(diff)

  }

  foi_scale$inf_vac_infector[i] = uniroot(find_inf_vac_infector, c(0, 1), tol = 0.001,
                                  r = foi_scale$R_scaling_factor[i], prob_inf = 0.5*hh.ar, list = edgeList)$root
                                                                                # 0.5 or 1
  
  # foi_scale$inf_vac_infector[i] = 1
  
  # tune inf_vac_infector such that prob inf for persons in same cabin is reduced
  # by about 50% over entire duration of infectiousness due to vaccination of infectee alone
  # https://spiral.imperial.ac.uk/bitstream/10044/1/90800/2/react1_r13_final_preprint_final.pdf
  find_inf_vac_infectee <- function(inf_vac_infectee, r, prob_inf, list){
    
    list[,FOI:=WEIGHT*inf_vac_infectee*r]
    list[,PROB_INF := 1-exp(-FOI)]
    diff = mean(list[CONTACT_GROUP==0,PROB_INF]) - prob_inf

    return(diff)
    
  }
  
  foi_scale$inf_vac_infectee[i] = uniroot(find_inf_vac_infectee, c(0, 1), tol = 0.001, 
                                          r = foi_scale$R_scaling_factor[i], prob_inf = 0.5*hh.ar, list = edgeList)$root
  
  
  # tune inf_vac_both such that prob inf for persons in same cabin is reduced
  # by about 1-(1-0.5)*(1-0.5)% over entire duration of infectiousness due to vaccination of infector and infectee alone
  find_inf_vac_both <- function(inf_vac_both, r, prob_inf, list){
    
    list[,FOI:=WEIGHT*inf_vac_both*r]
    list[,PROB_INF := 1-exp(-FOI)]
    diff = mean(list[CONTACT_GROUP==0,PROB_INF]) - prob_inf
    
    return(diff)
    
  }
  
  foi_scale$inf_vac_both[i] = uniroot(find_inf_vac_both, c(0, 1), tol = 0.001, 
                                          r = foi_scale$R_scaling_factor[i], prob_inf = (1-0.5)*(1-0.5)*hh.ar, list = edgeList)$root
                                                                                        # (1-0.5)*(1-0.5) or (1-0.5)*(1-0)
 
  # tune inf_mask_vac_infector such that prob inf for persons in same cabin is reduced
  # by about 1-(0.031/0.174)*(1-0.5)% over entire duration of infectiousness due to vaccination of infector and mask wearing for both
  find_inf_mask_vac_infector <- function(inf_mask_vac_infector, r, prob_inf, list){
    
    list[,FOI:=WEIGHT*inf_mask_vac_infector*r]
    list[,PROB_INF := 1-exp(-FOI)]
    diff = mean(list[CONTACT_GROUP==0,PROB_INF]) - prob_inf
    
    return(diff)
    
  }
  
  foi_scale$inf_mask_vac_infector[i] = uniroot(find_inf_mask_vac_infector, c(0, 1), tol = 0.001, 
                                      r = foi_scale$R_scaling_factor[i], prob_inf = (0.031/0.174)*(1-0.5)*hh.ar, list = edgeList)$root
                                                                       # (0.031/0.174)*(1-0.5) or (0.031/0.174)*(1-0)
  
  # tune inf_mask_vac_infectee such that prob inf for persons in same cabin is reduced
  # by about 1-(0.031/0.174)*(1-0.5)% over entire duration of infectiousness due to vaccination of infectee and mask wearing for both
  find_inf_mask_vac_infectee <- function(inf_mask_vac_infectee, r, prob_inf, list){
    
    list[,FOI:=WEIGHT*inf_mask_vac_infectee*r]
    list[,PROB_INF := 1-exp(-FOI)]
    diff = mean(list[CONTACT_GROUP==0,PROB_INF]) - prob_inf
    
    return(diff)
    
  }
  
  foi_scale$inf_mask_vac_infectee[i] = uniroot(find_inf_mask_vac_infectee, c(0, 1), tol = 0.001, 
                                               r = foi_scale$R_scaling_factor[i], prob_inf = (0.031/0.174)*(1-0.5)*hh.ar, list = edgeList)$root
                                                                                  # (0.031/0.174)*(1-0.5)       
  
  # tune inf_mask_vac_both such that prob inf for persons in same cabin is reduced
  # by about 1-(0.031/0.174)*(1-0.5)*(1-0.5)% over entire duration of infectiousness due to vaccination of infector and infectee and mask wearing for both
  find_inf_mask_vac_both <- function(find_inf_mask_vac_both, r, prob_inf, list){
    
    list[,FOI:=WEIGHT*find_inf_mask_vac_both*r]
    list[,PROB_INF := 1-exp(-FOI)]
    diff = mean(list[CONTACT_GROUP==0,PROB_INF]) - prob_inf

    return(diff)
    
  }
  
  foi_scale$inf_mask_vac_both[i] = uniroot(find_inf_mask_vac_both, c(0, 1), tol = 0.001, 
                                           r = foi_scale$R_scaling_factor[i], prob_inf = (0.031/0.174)*(1-0.5)*(1-0.5)*hh.ar, list = edgeList)$root
                                                  # (0.031/0.174)*(1-0.5)*(1-0.5) or (0.031/0.174)*(1-0.5)*(1-0)
  
}

save(foi_scale, file=paste('github/data/foi/foi_scale_set_1_sail_', sail,'.Rdata', sep = ''))
