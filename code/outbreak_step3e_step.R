#' Infections on one day in the network model
#' @author Lewis Spurgin
#' @author Joel Hellewell
#' @author Rachael Pung
#' @param case_data data.frame of cases in outbreak so far; initially generated by outbreak_setup
#' @param day number of days from start of the inection - must be > 0
#' @param interval number of half hourly intervals since the start of outbreak
#' @param net network from which to sample cases, in pairwise list format.
#' @param net.type 'static', 'temporal'                                      
#' @param prop.asym proportion of asymptomtaic cases (must be 0<=x<=1)
#' @param presymrate the proportion of cases with pre-onset transmission (must be 0<=x<=1)
#' @param prop.ascertain numeric proportion of infectious contacts ascertained by contact tracing (must be 0<=x<=1)
#' @param incfn function samples from incubation period; generated by dist_setup
#' @param delayfn function samples from the onset-to-hospitalisation delay; generated by dist_setup
#' @param isolation logical whether isolation is in effect
#' @param tracing logical whether tracing is in effect
#' @param secondary logical whether secondary contact tracing is in effect
#' @param quarantine logical whether quarantine is in effect, if TRUE then traced contacts are isolated before symptom onset
#' @param testing logical whether testing is in effect              
#' @param testing.regime none, PCR_once, Ag_daily               
#' @param outside daily probability of infection from outside the network
#' @param edge_scale includes R_scaling_factor and various infectiousness factors to scale the infection probability
#' @param cap_max_tests integer - maximum number of tests
#' @param test_neg false negative rate for testing
#' 
#' @rawNamespace import(dplyr, except = c(union,as_data_frame,groups))
#' @importFrom tibble tibble
#' @importFrom purrr map2 map2_dbl map_lgl rbernoulli
#' @importFrom magrittr %>% %<>%
#'
#' @return
#' @export

outbreak_step <- function(case_data, day, interval, net, net.type, incfn, delayfn, 
                          prop.asym, presymrate, prop.ascertain,
                          isolation, tracing, secondary,quarantine,testing, testing.regime,
                          outside, edge_scale, cap_max_tests = NULL, test_neg) {

  # rename network
  newnet <- copy(net)
  
  # setup scale factors
  R_scaling_factor <- edge_scale$R_scaling_factor
  inf_mask <- edge_scale$inf_mask
  inf_vac_infector <- edge_scale$inf_vac_infector
  inf_vac_infectee <- edge_scale$inf_vac_infectee
  inf_vac_both <- edge_scale$inf_vac_both
  inf_mask_vac_infector <- edge_scale$inf_mask_vac_infector
  inf_mask_vac_infectee <- edge_scale$inf_mask_vac_infectee
  inf_mask_vac_both <- edge_scale$inf_mask_vac_both
  
  vac.coverage <- edge_scale$vac.coverage
  mask <- edge_scale$mask
  

  # implement testing and update isolation times if test positive
  if(day == 4 & testing.regime == 'Ag_twice'){
    
    non_isolated_cases <- which(case_data$status == "I" & case_data$isolated == FALSE)

    if(length(non_isolated_cases) != 0){
      isolated_cases <- rbernoulli(length(non_isolated_cases),
                                   p = Ag_prob(day = rep(day,length(non_isolated_cases)),
                                               onset = case_data$onset[non_isolated_cases],
                                               ct.traj = ct.traj))

      isolated_cases <- non_isolated_cases[isolated_cases]
      case_data$isolated_time[isolated_cases] <- day
      case_data$release_time[isolated_cases] <- case_data$isolated_time[isolated_cases] + 14
      case_data$isolated[isolated_cases] <- TRUE

    }
  }
  

  # update isolation, quarantine and infection status -----------------------------------
  if(isolation) {

    new_isolations <- which(day > case_data$isolated_time)
    if(quarantine){
      new_quarantines <- which(day > case_data$quarantine_time)
    }
    new_releases <- which(day > case_data$release_time)


    case_data$isolated[new_isolations] <- TRUE
    if(quarantine){case_data$quarantined[new_quarantines] <- TRUE}
    case_data$isolated[new_releases] <- FALSE
    case_data$quarantined[new_releases] <- FALSE



    # reset isolation, release and test time for individuals who didn't undergo full isolation or quarantine
    early_releases <- c(new_releases[case_data$release_time[new_releases] <
                                       (case_data$isolated_time[new_releases] + 14)],
                        new_releases[case_data$release_time[new_releases] <
                                       (case_data$quarantine_time[new_releases] + 14)])
    case_data$isolated_time[early_releases] <- Inf
    case_data$test_time[early_releases] <- Inf
    case_data$release_time[early_releases] <- Inf
    case_data$quarantine_time[early_releases] <- Inf
  }

  # assign recovered status to recovered individuals
  recovered <- which(day > case_data$recovery_time)
  case_data$status[recovered] <- "R"






  # add infections from outside ---------------------------------------------

  if(outside > 0) {
    potential_new_inf<- which(case_data$status == "S" & !case_data$isolated & !case_data$quarantined)
    new_infections <- potential_new_inf[rbernoulli(length(potential_new_inf),
                                                   p = outside)]

    case_data$exposure[new_infections] <- day
    case_data$onset[new_infections] <- day + incfn(length(new_infections))
    case_data$recovery_time[new_infections] <- case_data$onset[new_infections] + 7
    case_data$status[new_infections] <- "I"

    # isolation times for symtpomatic new infections
    if(isolation)
    {
      # sym_cases <- new_infections[!case_data$asym[new_infections]]
      # case_data$isolated_time[sym_cases] <- case_data$onset[sym_cases] +
      #   delayfn(length(sym_cases))
      # case_data$release_time[sym_cases] <- case_data$isolated_time[sym_cases] + 14
    }
  }


  # pull out infectious inds who are not isolated
  infectors <- dplyr::filter(case_data, status == "I", !isolated)






  # new cases  ----------------------------

  # get contacts of infectious inds from network
  if(net.type == "static"){
    new_cases <- dplyr::filter(newnet,
                               caseid %in% infectors$caseid,            
                               weight > 0)          ##### RP edit
  }
  
  if(net.type == "temporal"){
    new_cases <- dplyr::filter(newnet,
                               caseid %in% infectors$caseid,            
                               weight > 0,
                               day_interact == day)          ##### RP edit
  }
  
  new_inf_rows <- match(new_cases$contact,
                        case_data$caseid)

  # only keep susceptible contacts who are not isolated
  new_cases <- new_cases[case_data$status[new_inf_rows] == "S" &
                           !case_data$isolated[new_inf_rows] &
                           !case_data$quarantined[new_inf_rows],]

  # modify weights based on vaccination and mask wearing interventions
  new_cases <- left_join(new_cases, case_data[,c('caseid', 'vaccinated')], by = 'caseid') %>%
                  rename('vac.caseid'=vaccinated)  
  
  new_cases <- left_join(new_cases, case_data[,c('caseid', 'vaccinated')], by = c('contact' = 'caseid')) %>%
    rename('vac.contact'=vaccinated)  
  
  if(mask == 1){

    # assume 60% duration of P-P in different travelling group occur in mask off setting
    new_cases[contact_group==2 & vac.caseid == 0 & vac.contact == 0,weight:=0.6*weight + 0.4*inf_mask*weight]
    new_cases[contact_group==2 & vac.caseid == 0 & vac.contact == 1,weight:=0.6*inf_vac_infectee*weight + 0.4*inf_mask_vac_infectee*weight]
    new_cases[contact_group==2 & vac.caseid == 1 & vac.contact == 0,weight:=0.6*inf_vac_infector*weight + 0.4*inf_mask_vac_infector*weight]
    new_cases[contact_group==2 & vac.caseid == 1 & vac.contact == 1,weight:=0.6*inf_vac_both*weight + 0.4*inf_mask_vac_both*weight]

    # assume 100% duration of P-C occur in mask on setting
    new_cases[contact_group==5 & vac.caseid == 0 & vac.contact == 0,weight:=inf_mask*weight]
    new_cases[contact_group==5 & vac.caseid == 0 & vac.contact == 1,weight:=inf_mask_vac_infectee*weight]
    new_cases[contact_group==5 & vac.caseid == 1 & vac.contact == 0,weight:=inf_mask_vac_infector*weight]
    new_cases[contact_group==5 & vac.caseid == 1 & vac.contact == 1,weight:=inf_mask_vac_both*weight]

    # assume 30% duration of C-C occur in mask off setting
    new_cases[contact_group%in%c(3,4) & vac.caseid == 0 & vac.contact == 0,weight:=0.3*weight + 0.7*inf_mask*weight]
    new_cases[contact_group%in%c(3,4) & vac.caseid == 0 & vac.contact == 1,weight:=0.3*inf_vac_infectee*weight + 0.7*inf_mask_vac_infectee*weight]
    new_cases[contact_group%in%c(3,4) & vac.caseid == 1 & vac.contact == 0,weight:=0.3*inf_vac_infector*weight + 0.7*inf_mask_vac_infector*weight]
    new_cases[contact_group%in%c(3,4) & vac.caseid == 1 & vac.contact == 1,weight:=0.3*inf_vac_both*weight + 0.7*inf_mask_vac_both*weight]

    # P-P close travelling group
    new_cases[contact_group==1 & vac.caseid == 0 & vac.contact == 1,weight:=inf_vac_infectee*weight]
    new_cases[contact_group==1 & vac.caseid == 1 & vac.contact == 0,weight:=inf_vac_infector*weight]
    new_cases[contact_group==1 & vac.caseid == 1 & vac.contact == 1,weight:=inf_vac_both*weight]


  } else if(mask==0){

    # assume 60% duration of P-P in different travelling group occur in mask off setting
    new_cases[contact_group==2 & vac.caseid == 0 & vac.contact == 1,weight:=inf_vac_infectee*weight]
    new_cases[contact_group==2 & vac.caseid == 1 & vac.contact == 0,weight:=inf_vac_infector*weight]
    new_cases[contact_group==2 & vac.caseid == 1 & vac.contact == 1,weight:=inf_vac_both*weight]

    # assume 100% duration of P-C occur in mask on setting
    new_cases[contact_group==5 & vac.caseid == 0 & vac.contact == 1,weight:=inf_vac_infectee*weight]
    new_cases[contact_group==5 & vac.caseid == 1 & vac.contact == 0,weight:=inf_vac_infector*weight]
    new_cases[contact_group==5 & vac.caseid == 1 & vac.contact == 1,weight:=inf_vac_both*weight]

    # assume 30% duration of C-C occur in mask off setting
    new_cases[contact_group%in%c(3,4) & vac.caseid == 0 & vac.contact == 1,weight:=inf_vac_infectee*weight]
    new_cases[contact_group%in%c(3,4) & vac.caseid == 1 & vac.contact == 0,weight:=inf_vac_infector*weight]
    new_cases[contact_group%in%c(3,4) & vac.caseid == 1 & vac.contact == 1,weight:=inf_vac_both*weight]

    # P-P close travelling group
    new_cases[contact_group==1 & vac.caseid == 0 & vac.contact == 1,weight:=inf_vac_infectee*weight]
    new_cases[contact_group==1 & vac.caseid == 1 & vac.contact == 0,weight:=inf_vac_infector*weight]
    new_cases[contact_group==1 & vac.caseid == 1 & vac.contact == 1,weight:=inf_vac_both*weight]
  }
  
  
 

  # generate new infections -------------------------------------------------

  if(nrow(new_cases) > 0) {

    # filter based on probability that each contact is infected
    infector_rows <- match(new_cases$caseid,
                           case_data$caseid)

    asymrate <- ifelse(case_data$asym[infector_rows],0.5,1)
    
    infected <- rbernoulli(nrow(new_cases),
                           p = inf_prob(day = rep(day,length(infector_rows)),
                                        inc_samp = case_data$onset[infector_rows],
                                        contactrate = new_cases$weight,
                                        theta = presymrate,
                                        infasym = asymrate,
                                        R = R_scaling_factor))

    # each contact can only be infected once
    new_cases <- new_cases[infected,] %>%
      group_by(contact) %>%
      slice_sample(n = 1)

  }






  # compile data for all new infections -------------------------------------


  # compile a data frame for all new cases, new_cases is the amount of people that each infector has infected

  if(nrow(new_cases) > 0){

    prob_samples <- match(new_cases$contact,case_data$caseid)
    case_data$infector[prob_samples] <- case_data$caseid[match(new_cases$caseid,
                                                               case_data$caseid)]
    case_data$exposure[prob_samples] <- day
    case_data$generation[prob_samples] <- case_data$generation[match(new_cases$caseid,                 
                                                                     case_data$caseid)] + 1            
    case_data$status[prob_samples] <- "I"
    case_data$onset[prob_samples] <- day + incfn(length(prob_samples))
    case_data$recovery_time[prob_samples] <- case_data$onset[prob_samples] + 7
    
    # isolation times for symtpomatic cases
    if(isolation) {               
      # sym_cases <- prob_samples[!case_data$asym[prob_samples]]
      # case_data$isolated_time[sym_cases] <- case_data$onset[sym_cases] +
      #   delayfn(length(sym_cases))
      # case_data$release_time[sym_cases] <- case_data$isolated_time[sym_cases] + 14

    }

  }






  # contact tracing ---------------------------------------------------------

  # empty vector of contacts
  traced_contacts <- c()


  if(tracing) {
    # get contacts of symptomatic infectors who onset yesterday
    new_ill <- filter(case_data,
                      status == "I",
                      !asym,
                      onset < day,
                      onset > (day - 1))

    if(nrow(new_ill) > 0){

      # get contacts of infectious inds from network
      case_contacts <- filter(newnet,
                              caseid %in% new_ill$caseid,
                              weight > 0)         

      new_contact_rows <- match(case_contacts$contact,
                                case_data$caseid)

      # only keep contacts who are not isolated and not recovered
      traced_contacts <- case_contacts[case_data$status[new_contact_rows] != "R" &
                                         !case_data$isolated[new_contact_rows] &
                                         !case_data$quarantined[new_contact_rows],]
      traced_contacts <- traced_contacts$contact[rbernoulli(length(traced_contacts$contact),
                                                            p = prop.ascertain)]
    }

  }


  # secondary contact tracing ----------------



  # estimate whether secondary contacts are traced
  if(secondary) {
    if(nrow(new_ill) > 0) {
      if(nrow(case_contacts) > 0) {

        sec_contacts <- filter(newnet,
                               caseid %in% case_contacts$contact,             
                               weight > 0)         

        new_contact_rows <- match(sec_contacts$contact,
                                  case_data$caseid)


        # only keep secondary contacts who are not isolated and not recovered
        traced_sec_contacts <- sec_contacts[case_data$status[new_contact_rows] != "R" &
                                              !case_data$isolated[new_contact_rows] &
                                              !case_data$quarantined[new_contact_rows],]

        # filter based on ascertainment rate
        traced_sec_contacts <- traced_sec_contacts$contact[rbernoulli(length(traced_sec_contacts$contact),
                                                                      p = prop.ascertain)]


        # add to list of traced contacts
        traced_contacts <- unique(c(traced_contacts,
                                    traced_sec_contacts))
      }
    }
  }







# set quarantine times based on tracing -----------------------------------

  if(isolation & tracing & length(traced_contacts) > 0) {

    if(quarantine) {
      # if you are recovered and asymptomatic and traced, you isolate
      recovered_traced <- which(case_data$caseid %in% traced_contacts &
                                  case_data$status == "R" &
                                  case_data$asym)
      case_data$quarantine_time[recovered_traced] <- day+delayfn(length(recovered_traced))

      # if you are susceptible you quarantine on being traced
      susceptible_traced <- which(case_data$caseid %in% traced_contacts &
                                    case_data$status == "S")

      case_data$quarantine_time[susceptible_traced] <- day+delayfn(length(susceptible_traced))

      # if you are infectious you isolate if the delay is shorter than your current iso time
      infectious_traced <- which(case_data$caseid %in% traced_contacts &
                                   case_data$status == "I")            
      new_iso_time <- day + delayfn(length(infectious_traced))
      case_data$quarantine_time[infectious_traced] <-  day + delayfn(length(infectious_traced))
      
    } else {

      # if you're susceptible and there's no quarantine and you're traced
      # you isolate immediately at symptom onset
      susceptible_traced <- which(case_data$caseid %in% traced_contacts &
                                    case_data$status == "S" &
                                    !case_data$asym)

      case_data$isolated_time[susceptible_traced] <- case_data$onset[susceptible_traced]

      # if you're infectious and traced and no quarantine, you also isolate at onset
      infectious_traced <- which(case_data$caseid %in% traced_contacts &
                                   case_data$status == "I" &
                                   !case_data$asym)

      case_data$isolated_time[infectious_traced] <- case_data$onset[infectious_traced]

    }

    case_data$release_time <- ifelse(case_data$isolated_time < case_data$quarantine_time,
                                     case_data$isolated_time + 14,
                                     case_data$quarantine_time + 14)

  }



  # test and release quarantined and isolated cases ------------------------------------

  if(testing)
  {
    #Iidentify untested inds and assign a test time based on their isolation and quarantine time
    untested <- which(case_data$test_time == Inf)
    case_data$test_time[untested] <- ifelse(case_data$isolated_time[untested] < case_data$quarantine_time[untested],
                                            case_data$isolated_time[untested],
                                            case_data$quarantine_time[untested]) +
      delayfn(length(untested))

    # today's tests
    new_tests <- which(day < case_data$test_time &
                         (day+1) > case_data$test_time)

    # if there's a test cap randomly sample tests to be excluded and reset their test time
    if(length(new_tests) > cap_max_tests) {
      not_tested <- new_tests[sample(1:length(new_tests),length(new_tests)-cap_max_tests)]
      case_data$test_time[not_tested] <- Inf
      new_tests <- new_tests[!(new_tests %in% not_tested)]
    }


    # run tests
    # 90% chance of testing positive
    # 2% false positive rate

    test_results <- (case_data$status[new_tests] == "I" &
                       rbernoulli(length(new_tests),
                                  1-test_neg)) |
      rbernoulli(length(new_tests), 0.02)


    negative_tests <- new_tests[!test_results]

    case_data$release_time[negative_tests] <- case_data$test_time[negative_tests]

  }




  return(case_data)
}



