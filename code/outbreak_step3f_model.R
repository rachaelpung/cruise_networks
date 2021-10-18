#' Run a single instance of the branching process model
#' @author Joel Hellewell
#' @author Lewis Spurgin
#' @author Rachael Pung
#' @inheritParams outbreak_setup
#' @inheritParams outbreak_step
#' @param data.return returns summarised weekly cases or daily cases or the raw case data        
#' @param s seed - optional for reproducing the same output. Useful for `plot_network()`
#'
#' @return data.frame of weekly cases or daily cases or the raw case data
#' @export
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate group_by summarise arrange
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na

outbreak_model <- function(net, net.type, num.initial.cases, initial.cases, initial.cases.onset,  
                           prop.asym, presymrate, prop.ascertain, cap_max_days, delay_shape, delay_scale,
                           isolation, tracing, secondary, quarantine, testing, testing.regime,
                           outside, edge_scale, cap_max_tests = NULL, test_neg, data.return = 'case data',
                           s = NULL) {

  # set up functions to sample from distributions
  # incubation period sampling function in days
  
  # wild type
  # incfn <- weibull_setup(dist_shape = 2.322737,
  #                        dist_scale = 6.492272)
  
  # delta
  incfn   <- lnorm_setup(dist_meanlog = log(4),
                         dist_sdlog = sqrt(2*(log(4.4)-log(4))))

  # onset to isolation delay sampling function
  delayfn <- weibull_setup(delay_shape,
                           delay_scale)


  # set initial values for loop indices
  total.cases <- num.initial.cases
  latest.onset <- 0
  extinct <- FALSE
  popsize <- nrow(net)
  cday <- 1
  daily_isolated <- 0 #none isolated on day 0
  daily_quarantined <- 0
  daily_tests <- 0


  # change column names ##### RP edit, extract only close contacts
  if(net.type == 'static'){
    colnames(net) <- c("caseid","contact","contact_group","weight") 
  } else if(net.type == 'temporal'){
    colnames(net) <- c("caseid","contact","contact_day","weight") 
  }


  # initial setup
  if(exists("s")){set.seed(s)}
  case_data <- outbreak_setup(net = net,
                              day = cday,
                              num.initial.cases = num.initial.cases,
                              initial.cases = initial.cases,      
                              initial.cases.onset = initial.cases.onset,  
                              incfn = incfn,
                              delayfn = delayfn,
                              prop.asym = prop.asym,
                              isolation = isolation,
                              testing.regime = testing.regime,
                              vac.coverage = edge_scale$vac.coverage)



  # model loop
  if(exists("s")){set.seed(s)}
  while (num.initial.cases >0 & cday <= cap_max_days & total.cases < popsize & !extinct) {
    case_data <- outbreak_step(case_data = case_data,
                               day = cday,
                               net = net,
                               net.type = net.type,        
                               incfn = incfn,
                               delayfn = delayfn,
                               prop.asym = prop.asym,
                               presymrate = presymrate,
                               prop.ascertain = prop.ascertain,
                               isolation = isolation,
                               tracing = tracing,
                               secondary = secondary,
                               quarantine = quarantine,
                               testing = testing,
                               testing.regime = testing.regime,
                               outside = outside,
                               edge_scale = edge_scale,
                               cap_max_tests = cap_max_tests,
                               test_neg = test_neg)


    total.cases <- sum(!is.na(case_data$exposure))
    extinct <- all(case_data$isolated,na.rm = TRUE)
    daily_isolated <- c(daily_isolated,sum(case_data$isolated))
    daily_quarantined <- c(daily_quarantined,sum(case_data$quarantined))
    daily_tests <- c(daily_tests,sum(floor(case_data$test_time) == cday))
    cday <- cday + 1

  }
 
  # prepare output, group into days
  if(data.return == 'daily'){
    daily_cases <- tibble(day = as.double(unique(1:cap_max_days)),         
                         daily_isolated,
                         daily_tests,
                         daily_quarantined) %>%
    left_join(case_data %>%                                   
                dplyr::group_by(floor(exposure)) %>%                   
                dplyr::summarise(tibble(daily_cases = n(),
                                 max_gen = max(generation))),         
              by = c("day" = "floor(exposure)"))  %>%
    mutate(daily_cases = tidyr::replace_na(daily_cases,0),
           max_gen = tidyr::replace_na(max_gen,0))                  
    
  }
  
  # prepare output, group into weeks
  if(data.return == 'weekly'){
    
    weekly_isolation <- c()
    weekly_quarantine <- c()
    weekly_tests <- c()
  
    for(i in seq(1,cap_max_days+1,7)){
      weekly_isolation <- c(weekly_isolation,
                            mean(daily_isolated[i:(i+6)],na.rm = TRUE))
  
      weekly_quarantine <- c(weekly_quarantine,
                             mean(daily_quarantined[i:(i+6)],na.rm = TRUE))
  
      weekly_tests <- c(weekly_tests,
                        sum(daily_tests[i:(i+6)],na.rm = TRUE))
    }
  
    weekly_cases <- tibble(week = unique(floor((1:cap_max_days)/7)),
                           weekly_isolation,
                           weekly_tests,
                           weekly_quarantine) %>%
      left_join(case_data %>%
                  dplyr::mutate(week = floor(exposure/7)) %>%              
                  dplyr::group_by(week) %>%
                  dplyr::summarise(weekly_cases = n()),
                by = "week") %>%
      mutate(weekly_cases = tidyr::replace_na(weekly_cases,0),
             weekly_isolation = tidyr::replace_na(weekly_isolation,0))
  
  
    # order and sum up, cut at max_week and add effective R0
    weekly_cases %<>%
      dplyr::arrange(week) %>%
      dplyr::mutate(cumcases = cumsum(weekly_cases),
                    cumiso = cumsum(weekly_isolation))
    
  }
  

  # return
  if(data.return == 'weekly') {
    return(weekly_cases)
  } else if (data.return == 'daily') {
    return(daily_cases)
  } else if (data.return == 'case data') {
    return(case_data)
  }
}
