## load.fun(x) will load library 'x' if it is installed. If 'x' has
## not been installed, it will install it. Understanding this code is
## not necessary
## source: http://r.789695.n4.nabble.com/Install-package-automatically-if-not-there-tp2267532p2267659.html
load.fun <- function(x) {
  x <- as.character(substitute(x))
  if(isTRUE(x %in% .packages(all.available=TRUE))) {
    eval(parse(text=paste("require(", x, ")", sep="")))
  } else {
    #update.packages()  ## good idea, but may take some time. can
    ## usually be safely skipped
    eval(parse(text=paste("install.packages('", x, "')", sep="")))
    eval(parse(text=paste("require(", x, ")", sep="")))
  }
}

################################################################################
## Download the data if necessary
file.name <- function(year) {
  return(sprintf("DFR_Data_FY%d.csv",year))
}
url.base <- "https://dialysisdata.org/sites/default/files/content/public-data/"
for (year in 2008:2016) {
  if (!file.exists(file.name(year))) {
    ## Long-term, we should not rely on the data always
    ## being available at the same url, but writing code to automatically
    ## download the data at least makes sure we document where the data
    ## originally came from.
    url <- paste(url.base,file.name(year),sep="")
    download.file(url,file.name(year),mode="wb")
  }
}
################################################################################

################################################################################
# Load and combine the data
vars.to.keep <- c("year","provfs","provcity","provname","state","network", ## identifying info
                  "chainnam","npi","modal_f",
                  "owner_f",   ## profit status,
                  "totstas_f", ## number of hemo stations
                  "staffy4_f", ## total staff
                  "dietFTy4_f", # renal dieticians full time
                  "dietPTy4_f",
                  "nurseFTy4_f", # nurses
                  "nursePTy4_f",
                  "ptcareFTy4_f", # patient care technicians
                  "ptcarePTy4_f",
                  "socwkFTy4_f", # social workers
                  "socwkPTy4_f",
                  "phdy4_f", # HD patient-months total
                  "ppavfy4_f", # % fistula
                  "sexy4_f", # % female
                  "agey4_f", # average age
                  "viny4_f", # average years of esrd
                  "clmcntcomy4_f", # average comorbidities
                  "hgmy4_f", # average hemoglobin
                  "smry4_f", # standardized mortality rate
                  "shrdy4_f", # standardized hospital days
                  "shrty4_f", # standardized hospital admissions
                  "sepiy4_f", # % patient hospitalized for septicimia
                  "htay4_f",  # number hospitalizations
                  "clmcntcomy3_f", # average comorbidities
                  "hgmy3_f", # average hemoglobin
                  "smry3_f", # standardized mortality rate
                  "shrdy3_f", # standardized hospital days
                  "shrty3_f", # standardized hospital admissions
                  "sepiy3_f", # % patient hospitalized for septicimia
                  "htay3_f",  # number hospitalizations
                  "surveydt", # last survey
                  "compl_cond", # compliance with survey
                  "cfc_f", # CfC citations
                  "std_f") # standard citations
df <- data.frame()
numeric.vars <-  c("totstas_f", "staffy4_f", "dietFTy4_f",
                   "dietPTy4_f", "nurseFTy4_f", "nursePTy4_f",
                   "ptcareFTy4_f", "ptcarePTy4_f", "socwkPTy4_f",
                   "phdy4_f", "ppavfy4_f", "sexy4_f", "agey4_f",
                   "viny4_f",
                   "clmcntcomy4_f", "hgmy4_f", "smry4_f",
                   "shrdy4_f", "shrty4_f", "sepiy4_f", "htay4_f",
                   "clmcntcomy3_f", "hgmy3_f", "smry3_f",
                   "shrdy3_f", "shrty3_f", "sepiy3_f", "htay3_f",
                   "cfc_f", "std_f")

for (year in 2008:2016) {
  tmp <- read.csv(file.name(year),as.is=TRUE)
  tmp$year <- year
  v <- vars.to.keep
  if ("surveydt_o_f" %in% names(tmp)) {
    ## survey regulations changed in 2008, merge _n_ ones after 2008
    ## and _o_  before 2008
    miss.char <- function(x) {
      is.na(x) | x=="." | x==""
    }
    tmp$surveydt <- tmp$surveydt_n_f
    tmp$surveydt[miss.char(tmp$surveydt_n_f)] <-
      tmp$surveydt_o_f[miss.char(tmp$surveydt_n_f)]
    tmp$cfc_f <- tmp$surveycfc_n_f
    tmp$cfc_f[miss.char(tmp$surveycfc_n_f)] <-
      tmp$surveycfc_o_f[miss.char(tmp$surveycfc_n_f)]
    tmp$std_f <- tmp$surveystd_n_f
    tmp$std_f[miss.char(tmp$surveystd_n_f)] <-
      tmp$surveystd_o_f[miss.char(tmp$surveystd_n_f)]
    tmp <- tmp[,!(names(tmp) %in% c("surveydt_o_f","surveydt_n_f"))]
    tmp$surveyact_f <- tmp$surveyact_n_f
    tmp$surveyact_f[miss.char(tmp$surveyact_n_f)] <-
      tmp$surveyact_o_f[miss.char(tmp$surveyact_n_f)]

  }
  for (n in c("provfs","surveydt","cfc_f","std_f","compl_cond")) {
    ## fix for variables whose names change slightly across years
    if (n=="surveydt") { p <- "survey(|_)dt" }
    else if (n=="compl_cond") { p <- ("(compl_cond|surveyact_f)") }
    else { p <- n }
    if (names(tmp)[grep(p,names(tmp))] != n) {
      if (length(grep(p,names(tmp)))==1) {
        names(tmp)[grep(p,names(tmp))] <- n
      } else {
        stop(sprintf("problem with %s",n))
      }
    }
  }

  if (!(all(v %in% names(tmp)))) {
    miss <- v[!(v %in% names(tmp))]
    warning(paste(c("Variables ", miss,
                    "not found in year ", year),collapse=" "))
    v <- v[v %in% names(tmp)]
  }
  foo <- tmp[,v]
  foo[,miss] <- ""
  df <- rbind(df,foo)
}

for(v in numeric.vars) {
  df[,v] <- as.numeric(df[,v])
}
################################################################################
## some extra functions

#' Create lags for panel data.
#'
#' This function creates lags (or leads) of panel data variables.
#' Input data should be sorted by i, t --- e.g.
#' df <- df[order(i,t),]
#' @param x Vector or matrix to get lags of.
#' @param i unit index
#' @param t time index
#' @param lag How many time periods to lag. Can be negative if leading
#' values are desired.
#' @return Lagged copy of x.
panel.lag <- function(x, i, t, lag=1) {
  if (!identical(order(i,t),1:length(i))) {
    stop("inputs not sorted.")
  }
  if (is.matrix(x)) {
    return(apply(x,MARGIN=2,FUN=function(c) { panel.lag(c,i,t,lag) }))
  }
  if (length(i) != length(x) || length(i) != length(t) ) {
    stop("Inputs not same length")
  }
  if (lag>0) {
    x.lag <- x[1:(length(x)-lag)]
    x.lag[i[1:(length(i)-lag)]!=i[(1+lag):length(i)] ] <- NA
    x.lag[t[1:(length(i)-lag)]+lag!=t[(1+lag):length(i)] ] <- NA
    val <- (c(rep(NA,lag),x.lag))
  } else if (lag<0) {
    lag <- abs(lag)
    x.lag <- x[(1+lag):length(x)]
    x.lag[i[1:(length(i)-lag)]!=i[(1+lag):length(i)] ] <- NA
    x.lag[t[1:(length(i)-lag)]+lag!=t[(1+lag):length(i)] ] <- NA
    val <- (c(x.lag,rep(NA,lag)))
  } else { # lag=0
    return (x)
  }
  if (class(x)=="Date" & class(val)=="numeric") {
    stopifnot(0==as.numeric(as.Date("1970-01-01")))
    val <- as.Date(val, origin="1970-01-01")
  }
  return(val)
}

################################################################################
## Clean the data.
## Rename variables, reconcile timing.
##
## xxxx4_f and xxxx3_f are variable "xxxx" in report year minus 2
## and report year minus 3 respectively. Some variables, namely the
## infection and hospitalization ones are only available in 3_f
## versions early on.
##
## Number of stations is recorded in an early month of report year
## minus 1. Number of workers is as of December 31st of report year
## minus 2 (for 4_f, minus 3 for 3_f). It  doesn't match up exactly,
## but we'll assume that this was the number of stations and staffing
## for report year minus 2.
##
## To merge things, we'll always prefer the 4_f version of variables,
## but use the 3_f when necessary.

names(df)[names(df)=="year"] <- "report_year"
df$year <- df$report_year - 2
df <- df[order(df$provfs,df$year),]
dialysis <- df[,c("provfs","year")]
## renaming and combining of variables
keys <- ## list of pairs - old abbreviation, new name
  list(c("clmcntcomy","comorbidities"),
       c("hgmy","hemoglobin"),
       c("smry","std.mortality"),
       c("shrdy","std.hosp.days"),
       c("shrty","std.hosp.admit"),
       c("sepiy","pct.septic"),
       c("htay","n.hosp.admit"))
for(pair in keys) {
  v4f <- paste(pair[1],"4_f",sep="")
  dialysis[,pair[2]] <- df[,v4f]
  v3f <- paste(pair[1],"3_f",sep="")
  lead.3f <- panel.lag(df[,v3f],i=df$provfs,t=df$year,lag=-1)
  dialysis[is.na(dialysis[,pair[2]]),pair[2]] <- lead.3f[is.na(dialysis[,pair[2]])]
}

names <-## list of pairs - old name, new name
  list(c("provcity","city"),
       c("provname","name"),
       c("state","state"),
       c("network","network"),
       c("chainnam","chain.name"),
       c("owner_f","profit.status"),
       c("totstas_f","stations"),
       c("staffy4_f","total.staff"),
       c("dietFTy4_f","dieticiansFT"),
       c("dietPTy4_f","dieticiansPT"),
       c("nurseFTy4_f", "nurseFT"),
       c("nursePTy4_f", "nursePT"),
       c("ptcareFTy4_f", "ptcareFT"),
       c("ptcarePTy4_f", "ptcarePT"),
       c("socwkFTy4_f", "social.workerFT"),
       c("socwkPTy4_f", "social.workerPT"),
       c("phdy4_f", "patient.months"),
       c("ppavfy4_f", "pct.fistula"),
       c("sexy4_f", "pct.female"),
       c("agey4_f", "patient.age"),
       c("viny4_f", "patient.esrd.years"))
for (pair in names) {
  dialysis[,pair[2]] <- df[,pair[1]]
}
## clean up some factors
treatment.type <- as.factor(df$modal_f)
treatment.type[treatment.type=="1"] <- "Hemodialysis"
treatment.type[treatment.type=="2"] <- "Hemodialysis and Peritoneal Dialysis"
treatment.type[treatment.type=="3"] <- "Peritoneal Dialysis"
treatment.type[treatment.type=="4"] <- "Unavailable"
dialysis$treatment.type <- droplevels(treatment.type)
dialysis$profit.status <- as.factor(dialysis$profit.status)

## inspection variables
##  report_year has info about most recent inspection as of mid
##  report_year - 1. This could be after end of most recent data
##  collected (which is report_yr - 2)
inspect.date <- as.Date(df$surveydt,"%d%b%Y")
inspect.date[is.na(inspect.date)] <-
  as.Date(df$surveydt,"%m/%d/%Y")[is.na(inspect.date)]
dialysis$inspect.date <- inspect.date
dialysis$inspect.result <- as.factor(df$compl_cond)
dialysis$inspect.cfc.cites <- df$cfc_f
dialysis$inspect.std.cites <- df$std_f
eoy <- as.Date(sprintf("31/12/%d",df$year),"%d/%m/%Y")
## replace missing inspection dates with most recent non missing one
cont <- TRUE
while (cont) {
  miss <- is.na(dialysis$inspect.date)
  dialysis$inspect.date[miss] <-
    panel.lag(dialysis$inspect.date,  dialysis$provfs,
              dialysis$year)[miss]
  dialysis$inspect.result[miss] <-
    panel.lag(dialysis$inspect.result,  dialysis$provfs,
              dialysis$year)[miss]
  dialysis$inspect.cfc.cites[miss] <-
    panel.lag(dialysis$inspect.cfc.cites,  dialysis$provfs,
              dialysis$year)[miss]
  dialysis$inspect.std.cites[miss] <-
    panel.lag(dialysis$inspect.std.cites,  dialysis$provfs,
              dialysis$year)[miss]
  cont <- sum(miss) > sum(is.na(dialysis$inspect.date))
}
dialysis$days.since.inspection <-
  as.numeric(difftime(eoy,dialysis$inspect.date,units="days"))

save(dialysis, file="dialysisFacilityReports.Rdata")
