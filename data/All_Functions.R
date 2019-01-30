######################################################################
#Midriff's Watch Project Functions 
#December 10, 2018
######################################################################

######################################################################
#CMSY2 Function
#
######################################################################

Species_CMSY2<-function(data, priors){
  library(dplyr)
  require(datalimited2)
  
  variables <- data.frame(Name=NA,
                          Adjusted=NA,
                          r= NA, 
                          r.low = NA, 
                          r.hi = NA,
                          k= NA, 
                          k.low = NA, 
                          k.hi = NA,
                          msy= NA, 
                          msy.low = NA, 
                          msy.hi = NA,
                          bmsy= NA, 
                          bmsy.low = NA,
                          bmsy.hi = NA,
                          year=NA,	
                          catch=NA,	
                          catch_ma=NA,	
                          b=NA,	
                          b_lo=NA,	
                          b_hi=NA,	
                          bbmsy=NA,	
                          bbmsy_lo=NA,	
                          bbmsy_hi=NA,	
                          s=NA,	
                          s_lo=NA,	
                          s_hi=NA,	
                          f=NA,	
                          f_lo=NA,	
                          f_hi=NA,	
                          fmsy=NA,	
                          fmsy_lo=NA,	
                          fmsy_hi=NA,	
                          ffmsy=NA,	
                          ffmsy_lo=NA,	
                          ffmsy_hi=NA,	
                          er=NA)
  
  ref_ts <- data.frame(Name=NA,
                       Adjusted=NA,
                       year=NA,	
                       catch=NA,	
                       catch_ma=NA,	
                       b=NA,	
                       b_lo=NA,	
                       b_hi=NA,	
                       bbmsy=NA,	
                       bbmsy_lo=NA,	
                       bbmsy_hi=NA,	
                       s=NA,	
                       s_lo=NA,	
                       s_hi=NA,	
                       f=NA,	
                       f_lo=NA,	
                       f_hi=NA,	
                       fmsy=NA,	
                       fmsy_lo=NA,	
                       fmsy_hi=NA,	
                       ffmsy=NA,	
                       ffmsy_lo=NA,	
                       ffmsy_hi=NA,	
                       er=NA)
  
  Genus <- as.vector(unique(data$Genus))
  
  for (s in Genus){
    df <- data%>% filter(Genus == s) 
    
    year<-as.vector(df$Ano, mode='numeric')
    
    catches<-(df[,4:7])
    
    var <-subset(priors, Genus == s)
    
    resilience <- (var$Resilience)
    r.lo<- (var$r_lo)
    r.hi<- (var$r_hi)
    
    for (i in 1:ncol(catches)){
      catch <- catches[,i]
      
      adjusted <- c(colnames(catches[i]))
      
      cmsy <- datalimited2::cmsy2(year=year, catch=catch, resilience = resilience, r.low=r.lo, r.hi=r.hi)
      
      out <- data.frame(Name = s, 
                        Adjusted = adjusted,
                        r = cmsy$ref_pts[1,2], 
                        r.low = cmsy$ref_pts[1,3], 
                        r.hi = cmsy$ref_pts[1,4],
                        k = cmsy$ref_pts[2,2], 
                        k.low = cmsy$ref_pts[2,3], 
                        k.hi = cmsy$ref_pts[2,4],
                        msy = cmsy$ref_pts[3,2], 
                        msy.low = cmsy$ref_pts[3,3], 
                        msy.hi = cmsy$ref_pts[3,4],
                        bmsy = cmsy$ref_pts[5,2], 
                        bmsy.low = cmsy$ref_pts[5,3],
                        bmsy.hi = cmsy$ref_pts[5,4],
                        year = cmsy$ref_ts[tail(),1],	
                        catch = cmsy$ref_ts[tail(),2],	
                        catch_ma = cmsy$ref_ts[tail(),3],	
                        b = cmsy$ref_ts[tail(),4],	
                        b_lo = cmsy$ref_ts[tail(),5],	
                        b_hi = cmsy$ref_ts[tail(),6],	
                        bbmsy = cmsy$ref_ts[tail(),7],	
                        bbmsy_lo = cmsy$ref_ts[tail(),8],	
                        bbmsy_hi = cmsy$ref_ts[tail(),9],	
                        s = cmsy$ref_ts[tail(),10],	
                        s_lo = cmsy$ref_ts[tail(),11],	
                        s_hi = cmsy$ref_ts[tail(),12],	
                        f = cmsy$ref_ts[tail(),13],	
                        f_lo = cmsy$ref_ts[tail(),14],	
                        f_hi = cmsy$ref_ts[tail(),15],	
                        fmsy = cmsy$ref_ts[tail(),16],	
                        fmsy_lo = cmsy$ref_ts[tail(),17],	
                        fmsy_hi = cmsy$ref_ts[tail(),18],	
                        ffmsy = cmsy$ref_ts[tail(),19],	
                        ffmsy_lo = cmsy$ref_ts[tail(),20],	
                        ffmsy_hi = cmsy$ref_ts[tail(),21],	
                        er = cmsy$ref_ts[tail(),22])
      
      variables <- rbind(variables, out)
      
      output<- data.frame(cmsy$ref_ts)
      
      output<- output%>%
        mutate(Name = s, 
               Adjusted = c(adjusted))
      
      ref_ts<- rbind(ref_ts, output)
    }
  }

  return (list(variables[-1,], ref_ts[-1,]))
}

######################################################################
#Harvest function 
#
######################################################################

harvest <- function(fishing, patches, MPA.width)  {
  fishing.vec <- vector(length=patches)
  fishing.vec[] <- fishing 
  if (MPA.width > 0) {  
    MPA.begin <- round((patches-MPA.width)/2)+1
    MPA.end <- MPA.begin + MPA.width -1         
    fishing.vec[MPA.begin:MPA.end] <- 0        
  }
  return(fishing.vec)
}

fishing.matrix <- function(fishing, MPA, MPA.matrix)  {
  
  freq <-table(MPA.matrix)
  displace<- fishing*freq[2]
  new_F <- fishing + displace
  
  if (MPA == 0){
    MPA.matrix[MPA.mat = 0] <- new_F
    MPA.matrix[MPA.mat = 1] <- new_F }
  else {
    MPA.matrix[MPA.mat = 0] <- new_F
    MPA.matrix[MPA.mat = 1] <- 0
  }
  return(MPA.matrix)
}


######################################################################
#Biological Patch Model Function
#
######################################################################

MPA.Model <- function(r, K, fishing, biomass, patches, years, MPA.width, mrate){
  library(dplyr)
  library(magrittr)
  
  f.rate <- harvest(fishing=fishing, patches=patches, MPA.width=MPA.width)
  Biomass <- vector(length=patches)
  Biomass[] <-biomass
  left.patch <- c(patches, 1: (patches-1))
  right.patch <- c(2: patches, 1)
  
  summary<- data.frame(Year=NA,
                       Leave=NA, 
                       Arrive=NA,
                       Surplus=NA,
                       Catch=NA,
                       Biomass=NA)
  
  Years<- as.vector(2015:(2015+years))
  
  for (i in Years){
    
    leaving <- 2*mrate*Biomass
    arriving <- 0.5*leaving[left.patch]+ 0.5*leaving[right.patch]
    surplus <- r*Biomass *(1-Biomass/K)
    catches <- f.rate*Biomass
    Biomass <- Biomass+surplus-catches- leaving+ arriving
    output<- data.frame (Year= i, 
                         Leave = sum(leaving) , 
                         Arrive = sum(arriving) , 
                         Surplus = sum(surplus),
                         Catch = sum(catches),
                         Biomass = sum(Biomass)
    )                      
    
    summary <- rbind (summary, output)
  }
  return(summary[-1,])
}

######################################################################
#Biological Patch Model Function
#looping over average, low and high estimates of variables 
######################################################################

Scenarios <- function(data, patches, years, MPA.width, mrate) {
  library(dplyr)
  out<-data.frame(Name=NA,
                  Adjusted=NA)
  
  R <- as.numeric(data[,3:5])
  k <- as.numeric(data[,6:8])
  Fishing <- as.numeric(data[,9:11])
  Biomass <- as.numeric(data[,12:14])
  
  for (s in 1:length(R)){
    fishing <- Fishing[s]
    r <- R[s]
    K <- (k[s]/patches)
    biomass<- (Biomass[s]/patches)
    
    MPA<-MPA.Model(r=r, K=K, biomass=biomass, fishing=fishing, patches=patches,
                   MPA.width=MPA.width, years=years, mrate=mrate)
    
    out<-(cbind(out, MPA))
  }
  out<- out[,-c(9,15)]
  
  out<-out %>%
    mutate(Name=data$Name,
           Adjusted=data$Adjusted)
  
  names(out) <- c("Name",
                  "Adjusted", 
                  "Year",
                  "Leave_est",
                  "Arrive_est", 
                  "Surplus_est",
                  "Catch_est",
                  "Biomass_est", 
                  "Leave_lo",
                  "Arrive_lo", 
                  "Surplus_lo",
                  "Catch_lo",
                  "Biomass_lo", 
                  "Leave_hi",
                  "Arrive_hi", 
                  "Surplus_hi",
                  "Catch_hi",
                  "Biomass_hi")
  return(out[-1,])
}

######################################################################
#Biological Patch Model Function
#looping over all Fisheries and inflated catch
######################################################################
Biological.Model<- function(df, patches, years, MPA.width, mrate) {
  
  results<- data.frame(Name = NA,
                       Adjusted = NA, 
                       Year = NA,
                       Leave_est = NA,
                       Arrive_est = NA, 
                       Surplus_est = NA,
                       Catch_est = NA,
                       Biomass_est = NA, 
                       Leave_lo = NA,
                       Arrive_lo = NA, 
                       Surplus_lo = NA,
                       Catch_lo = NA,
                       Biomass_lo = NA, 
                       Leave_hi = NA,
                       Arrive_hi = NA, 
                       Surplus_hi = NA,
                       Catch_hi = NA,
                       Biomass_hi = NA)
  for(i in 1:nrow(df)) {
    
    data <- df[i,]
    
    scen <- Scenarios(data=data, patches=patches, years=years, MPA.width=MPA.width, mrate=mrate)  
    
    results <- rbind(results, scen)
  }

  return(results[-1,])
}
