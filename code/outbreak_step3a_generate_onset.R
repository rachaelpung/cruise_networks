source('github/code/cruise_load_library.R')


# determine probability of onset by day of event (day 1-7)
# http://weekly.chinacdc.cn/en/article/doi/10.46234/ccdcw2021.148
distIncub = data.table(DAY=1:14, PDF=dlnorm(1:14, meanlog = log(4), sdlog =sqrt(2*log(4.4/4))))
distIncub[,PDF:=PDF/sum(PDF)]

# assume that index cases could be infected up to 14 days before event
# determine probability symptoms onset by respective days before/during event
distOnset = data.table(DAY_INFECTION=-13:0,PROB_DAY_INFECTION=1/14)
distOnset = distOnset[rep(seq_len(distOnset[,.N]), each=14)]
distOnset[,DAY_ONSET:=rep(1:14, len=.N)]
distOnset[distIncub,INC_PDF:=i.PDF, on=c(DAY_ONSET='DAY')]
distOnset[,DAY_ONSET:=DAY_ONSET+DAY_INFECTION]
distOnset[,INC_PDF:=INC_PDF*PROB_DAY_INFECTION]
distOnset = distOnset[,sum(INC_PDF), by=.(DAY_ONSET)]

# determine probability of symptoms onset by respective days during event
# conditional on symptoms onset during event (persons who developed symptoms prior to event will be barred)
distOnset.7 = distOnset[DAY_ONSET %in% c(1:7)]
setnames(distOnset.7, old='V1', new='PDF')
distOnset.7[,PDF:=PDF/sum(PDF)]

distOnset.3 = distOnset[DAY_ONSET %in% c(1:3)]
setnames(distOnset.3, old='V1', new='PDF')
distOnset.3[,PDF:=PDF/sum(PDF)]

save(distOnset.3, file = 'github/data/onset/distOnset.3.RData')
save(distOnset.7, file = 'github/data/onset/distOnset.7.RData')
