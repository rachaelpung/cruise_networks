#' Create partial function to sample from weibull and log-normal distributions
#' @author Joel Hellewell
#' @author Rachael Pung
#' @param dist_shape numeric shape parameter of Weibull distribution
#' @param dist_scale numeric scale parameter of Weibull distribution
#' @param dist_meanlog numeric meanlog parameter of log normal distribution
#' @param dist_sdlog numeric sdlog parameter of log normal distribution
#'
#' @return partial function that takes a numeric argument for number of samples
#' @export
#' @importFrom purrr partial
#' @examples
#'
weibull_setup <- function(dist_shape = NULL, dist_scale = NULL) {
  
  out <- purrr::partial(rweibull,
                        shape = dist_shape,
                        scale = dist_scale)  
  
  return(out)
}


lnorm_setup <- function(dist_meanlog = NULL, dist_sdlog = NULL) {
  out <- purrr::partial(rlnorm,
                        meanlog  = dist_meanlog,
                        sdlog = dist_sdlog)
  return(out)
}


#' Samples the serial interval for given incubation period samples - gives an infection prob for a given day
#' @author Joel Hellewell
#' @author Lewis Spurgin
#' @author Rachael Pung
#' @param day day of simulation
#' @param inc_samp vector of samples from the incubation period distribution     
#' @param theta proportion of presymptomatic transmission            
#' @param R scaling factor
#' @param contactrate vector of network edge weights
#' @param infasym vector of weights based on whether inds are asymptomatic
#'
#' @return vector of probabilities
#' @export
#' @importFrom sn dsn
#' @importFrom purrr rbernoulli
#' @examples
#'
#'
#'

inf_prob <- function(day = NULL, inc_samp = NULL, theta = NULL, R = NULL, contactrate = NULL, infasym = NULL) {
  
  # default assume 25% presymptomatic transmission
  serial_int <- sn::dsn(x = day,    # x is the sum of day of exposure and time since infection
                        xi = inc_samp-2,    
                        omega = 5,
                        alpha = 4)
    
  if(theta == 0.5){
    serial_int <- sn::dsn(x = day,
                          xi = inc_samp,    
                          omega = 2,
                          alpha = -0.2)
  }
  
  
  totalFOI <- R*contactrate*infasym*serial_int
  out2 <- 1 - exp(1)^(-totalFOI)
  
  return(out2)
}



# peak in Ct values 18.5 from https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3861566
# incubation period 4.4 days from http://weekly.chinacdc.cn/en/article/doi/10.46234/ccdcw2021.148
# duration to CT > 30 since onset 18 days from https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3861566
ct.traj = splinefunH(c(-4,0,35), c(40,18.5,40), c(0,0,0))
ct.traj = data.table(DAY_FROM_ONSET = -30:35, 
                     CT = ct.traj(-30:35))
ct.traj[,CT_DROP:=40-CT]


PCR_prob <- function(day = NULL, onset = NULL, ct.traj = NULL){
  
  ct = ct.traj[match(day-1-floor(onset), DAY_FROM_ONSET),CT]
  dist_PCR_sensitivity <- data.table(ct_threshold = c(35,40), prob = c(1,0))
  
  # test one day before sail
  sapply(ct,function(x){
    dist_PCR_sensitivity$prob[min(which((dist_PCR_sensitivity$ct_threshold>=x) == T))]
  })
  
  
}


Ag_prob <- function(day = NULL, onset = NULL, ct.traj = NULL){
  
  # https://www.cochranelibrary.com/cdsr/doi/10.1002/14651858.CD013705.pub2/full
  ct = ct.traj[match(day-floor(onset), DAY_FROM_ONSET),CT]
  dist_Ag_sensitivity <- data.table(ct_threshold = c(25, 32, 39.999, 40), prob = c(0.945, 0.825, 0.089, 0))
  
  # print(c(ct, day, onset))
  # as.numeric(sapply(ct,function(x){
  #   predict(binom.ct.antigen, newdata = data.frame(ct=x), type="response")
  # }))
  
  
  sapply(ct,function(x){
    dist_Ag_sensitivity$prob[min(which((dist_Ag_sensitivity$ct_threshold>=x) == T))]
  })
  
}

