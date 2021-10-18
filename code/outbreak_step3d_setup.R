#' Set up initial cases for network model
#' @author Lewis Spurgin
#' @author Joel Hellewell
#' @author Rachael Pung
#' @param net network in pairwise list format.
#' @param day day of the simulation
#' @param num.initial.cases integer number of initial cases
#' @param initial.cases specify the individual to infect     
#' @param initial.cases.onset symptoms onset date of the initial cases   
#' @param incfn function that samples from incubation period Weibull distribution; generated using dist_setup
#' @param delayfn function that samples from the onset-to-hospitalisation delay Weibull distribution; generated using dist_setup
#' @param prop.asym numeric proportion of cases that are sublinical (between 0 and 1)
#' @param isolation logical - do you want indiviuals to self-isolate on infection?
#' @param testing.regime none, PCR_once, Ag_daily    
#' @param vac.coverage vaccination coverage           
#'
#' @return data.frame of cases in outbreak so far
#' @export
#' @importFrom tibble tibble
#'

outbreak_setup <- function(net, day, num.initial.cases, initial.cases, initial.cases.onset,
                           incfn, delayfn, prop.asym, isolation, testing.regime, vac.coverage) {

  # set up table of population
  pop <- unique(c(net$caseid,net$contact))
  popsize <- length(pop)    
  case_data <- tibble(exposure = NA, 
                      generation = NA,                
                      asym = purrr::rbernoulli(popsize, p = prop.asym),
                      caseid = unique(c(net$caseid,net$contact)),         
                      infector = NA,
                      onset = NA,
                      isolated_time = Inf,
                      quarantine_time = Inf,
                      test_time = Inf,
                      release_time = NA,
                      recovery_time = NA,
                      status = "S",
                      isolated = FALSE,
                      quarantined = FALSE,
                      vaccinated = as.numeric(purrr::rbernoulli(popsize, p = vac.coverage)))

  # set up initial cases 
  if(num.initial.cases == 0) return(case_data)

  
  if(all(initial.cases %in% pop)){             
    initial_cases <- which(case_data$caseid %in% initial.cases)        
  } else{
    initial_cases <- sample(1:popsize,num.initial.cases)           
  }
  
  case_data$exposure[initial_cases] <- 0
  case_data$generation[initial_cases] <- 1                  
  case_data$onset[initial_cases] <- initial.cases.onset    # incfn(num.initial.cases)           
  case_data$recovery_time[initial_cases] <- case_data$onset[initial_cases] + 7
  case_data$status[initial_cases] <- "I"

  if(isolation){
    # isolation times for symptomatic cases: onset + delay
    # sym_cases <- initial_cases[!case_data$asym[initial_cases]]
    # case_data$isolated_time[sym_cases] <- case_data$onset[sym_cases] + delayfn(length(sym_cases))
    # case_data$release_time[sym_cases] <- case_data$isolated_time[sym_cases] + 14
    
    
    isolated_initial_cases <- 0
    if(testing.regime == 'PCR_once' & num.initial.cases !=0){
      isolated_initial_cases <- rbernoulli(num.initial.cases,
                                           p = PCR_prob(day = rep(day,num.initial.cases),
                                                        onset = case_data$onset[initial_cases],
                                                        ct.traj = ct.traj))
    } else if(testing.regime == 'Ag_twice' & num.initial.cases !=0){
      isolated_initial_cases <- rbernoulli(num.initial.cases,
                                           p = Ag_prob(day = rep(day,num.initial.cases),
                                                       onset = case_data$onset[initial_cases],
                                                       ct.traj = ct.traj))
    }
    isolated_initial_cases <- initial_cases[isolated_initial_cases]
    case_data$isolated_time[isolated_initial_cases] <- day
    case_data$release_time[isolated_initial_cases] <- case_data$isolated_time[isolated_initial_cases] + 14
    case_data$isolated[isolated_initial_cases] <- TRUE
    
  } 

  

  # return
  return(case_data)
}
