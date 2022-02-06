#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @author Lewis Spurgin
#' @author Rachael Pung
#' @param param.set parameter set
#' @param n.sim number of simulations to run
#' @param net network from which to sample cases - edgelist
#' @param net.type 'static', 'temporal'   
#' @param num.initial.cases initial number of cases
#' @param initial.cases specify the individual to infect     
#' @param initial.cases.onset symptoms onset day of the initial cases   
#' @param prop.asym proportion of asymptomatic cases (must be 0 <= x <= 1)
#' @param presymrate rate of presymptomatic transmission (must be 0 <= x <= 1)
#' @param prop.ascertain probability that cases are ascertained by contact tracing
#' @param cap_max_days maximum number of days to run process for
#' @param delay_shape shape of distribution for delay between symptom onset and isolation
#' @param delay_scale scale of distribution for delay between symptom onset and isolation
#' @param intervention character - must be either ("nothing","isolation","primary_quarantine", or "secondary_quarantine")
#' @param testing.regime none, PCR_once, Ag_daily 
#' @param null.net do you want to simulate on a null network? Must be 'none', 'edge', 'deg', 'latt' or 'clust'
#' @param outside infection rate from outside the network (must be 0 <= x <= 1)
#' @param distancing proportion of rare contacts to remove (must be 0 <= x <= 1)
#' @param edge_scale includes R_scaling_factor and various infectiousness factors to scale the infection probability
#' @param dist_func how do you want to do social distancing? (must be dist_all or dist_no_reall - see aux functions)
#' @param test_neg false negative rate for testing
#' @param cap_max_tests integer - max number of daily tests. Only use if testing == TRUE
#' @param scenario naming of the modelled scenario
#'
#' @importFrom purrr safely map
#' @importFrom dplyr bind_rows mutate
#' @return data.frame of cases, isolations, quarantines and tests for each simulation 


scenario_sim_parallel <- function(param.set, n.sim, net, net.type, num.initial.cases, initial.cases,  initial.cases.onset,
                                  prop.asym, presymrate, prop.ascertain, cap_max_days, delay_shape, delay_scale,
                                  intervention, testing.regime, null.net = "none", outside, distancing, 
                                  edge_scale, dist_func = NULL, cap_max_tests = NULL, test_neg = 0.1, scenario){
  
  
  # check input parameters --------------------------------------------------
  
  if(floor(prop.ascertain) != 0) stop("prop.ascertain must between 0 and 1")
  if(floor(presymrate) != 0) stop("presymrate must between 0 and 1")
  if(floor(prop.asym) != 0) stop("prop.asym must between 0 and 1")
  if(floor(outside) != 0) stop("outside must between 0 and 1")
  
  
  
  # set up networks ---------------------------------------------------------
  
  # message("Setting up networks...")
  
  if(null.net == "none")
  {
    amlist <- rep(list(net),n.sim)
  } else
  {
    amlist <- replicate(n.sim, network_null(net,returns = "matrix",null = null.net), simplify = FALSE)
  }
  
  if(distancing > 0)
  {
    amlist <- purrr::map(amlist, ~ dist_reall(., "matrix", distancing))  
  }
  
  
  netlist <- amlist
  
  
  # set up interventions --------------------------------------------------------
  
  if(intervention == "nothing") {
    isolation <- FALSE
    tracing <- FALSE
    quarantine <- FALSE
    secondary <- FALSE
    testing <- FALSE            
  } else {
    if(intervention == "isolation") {
      isolation <- TRUE
      tracing <- FALSE
      quarantine <- FALSE
      secondary <- FALSE
      testing <- FALSE            
    } else {
      if(intervention == "primary_quarantine") {
        isolation <- TRUE
        tracing <- TRUE
        quarantine <- TRUE
        secondary <- FALSE
        testing <- FALSE            
      } else {
        if(intervention == "secondary_quarantine") {
          isolation <- TRUE
          tracing <- TRUE
          quarantine <- TRUE
          secondary <- TRUE
          testing <- FALSE            
        } else {
          stop('Intervention must be either "nothing","isolation","primary_quarantine", or "secondary_quarantine"')
        }
      }
    }
  }
  
  
  # message("Running epidemic model...")
  
  # run n.sim number of model runs and put them all together in a big data.frame
  res <- 1:n.sim %>% 
            purrr::map(function(i) outbreak_model(net = netlist[[i]], net.type = net.type, 
                                                  num.initial.cases = num.initial.cases,
                                                  initial.cases = initial.cases,
                                                  initial.cases.onset = initial.cases.onset,
                                                  prop.asym = prop.asym,
                                                  presymrate = presymrate,
                                                  prop.ascertain = prop.ascertain,
                                                  cap_max_days = cap_max_days,
                                                  delay_shape = delay_shape,
                                                  delay_scale = delay_scale,
                                                  isolation = isolation,
                                                  tracing = tracing,
                                                  secondary = secondary,
                                                  quarantine = quarantine,
                                                  testing = testing,
                                                  testing.regime = testing.regime,
                                                  outside = outside,
                                                  edge_scale = edge_scale,
                                                  cap_max_tests = cap_max_tests,
                                                  test_neg = test_neg,
                                                  data.return = 'case data'))
      
  res.row <- nrow(res[[1]])
  n.start <- (param.set-1)*n.sim + 1
  n.end <- (param.set-1)*n.sim + n.sim
  
  # bind output together and add simulation index
  res <- dplyr::bind_rows(res) %>%
    dplyr::mutate(sim = rep(n.start:n.end, rep(res.row, n.sim)),
                  paramset = param.set,
                  scenario = scenario)   
  
  res <- res[which(!is.na(res$generation)),]
  
  # message("Done!")
  return(res)
}
