######################################################################
#Harvest function 
#Even redistribution of fising effort 
######################################################################

fishing.matrix<- function (fishing, MPA, MPA.matrix){
  freq<- table(MPA.matrix)
  MPA.patch<- as.numeric(freq[2])
  NoMPA.patch<-as.numeric(freq[1])
  
  displace<- (fishing*MPA.patch)/NoMPA.patch
  new_F<- fishing+displace
  
  if (MPA==1){
    MPA.matrix[MPA.matrix==0]<- new_F
    MPA.matrix[MPA.matrix==1]<-0}
    else {MPA.matrix[,]<- fishing
    }
  return(MPA.matrix)}

######################################################################
#Biological 2-D Patch Model Function
#
######################################################################

MPA.Model <- function(r, K, fishing, biomass, MPA, years, MPA.matrix, mrate){
  library(dplyr)
  library(magrittr)
  
  f.matrix <- fishing.matrix(fishing=fishing, MPA=MPA, MPA.matrix=MPA.matrix)
  Biomass <- MPA.matrix
  Biomass[,] <- biomass
  
  patches <- 106 #dimenssions of the mpa matrix

  l.patch <- c(patches, 1: (patches-1))
  left.patch<-as.matrix(do.call(rbind, replicate(patches, l.patch, simplify=FALSE)))
  
  r.patch <- c(2: patches, 1)
  right.patch<-as.matrix(do.call(rbind, replicate(patches, l.patch, simplify=FALSE)))
  
  u.patch <- c(patches, 1: (patches-1))
  up.patch<-as.matrix(do.call(cbind, replicate(patches, u.patch, simplify=FALSE)))
  
  d.patch <- c(2: patches, 1)
  down.patch<-as.matrix(do.call(cbind, replicate(patches, d.patch, simplify=FALSE)))
  
  summary<- data.frame(Year=NA,
                       Leave=NA, 
                       Arrive=NA,
                       Surplus=NA,
                       Catch=NA,
                       Biomass=NA)
  
  Years<- as.vector(2015:(2015+years))
  
  arriving <- matrix(nrow=nrow(MPA.matrix), ncol= ncol(MPA.matrix), 0)
  
  for (i in Years){
    
    leaving <- mrate*Biomass 
    
    for(row in 1:nrow(arriving)) {
      for(col in 1:ncol(arriving)) {
        
        arriving [row, col] <- 0.25*leaving[left.patch[row, col]]+ 
                               0.25*leaving[right.patch[row, col]] + 
                               0.25*leaving[up.patch[row, col]] + 
                               0.25*leaving[down.patch[row, col]]
      }
      } #close nested forloop for calculating arriving
    
    surplus <- r*Biomass *(1-Biomass/K)
    
    catches <- f.matrix * Biomass
    
    Biomass <- Biomass + surplus - catches - leaving + arriving
    
    output<- data.frame (Year= i, 
                         Leave = sum(leaving) , 
                         Arrive = sum(arriving) , 
                         Surplus = sum(surplus),
                         Catch = sum(catches),
                         Biomass = sum(Biomass))                      
    summary <- rbind (summary, output)
  }
  return(summary[-1,])
}

######################################################################
#Biological 2-D Patch Model Function
#looping over average, low and high estimates of variables 
######################################################################

Scenarios <- function(data, MPA, years, MPA.matrix) {
  library(dplyr)
  out<-data.frame(Name=NA,
                  Adjusted=NA)
  
  R <- as.numeric(data[,3:5])
  k <- as.numeric(data[,6:8])
  Fishing <- as.numeric(data[,9:11])
  Biomass <- as.numeric(data[,12:14])
  mrate<- as.numeric(data[,15])
  
  all.patches<-11236

  for (s in 1:length(R)){
    r <- R[s]
    K <- (k[s]/all.patches)
    fishing <- Fishing[s]
    biomass<- (Biomass[s]/all.patches)
    mrate<- mrate
    
    MPA<-MPA.Model(r=r, K=K, biomass=biomass, fishing=fishing, MPA=MPA,
                   MPA.matrix=MPA.matrix, years=years, mrate=mrate)
    
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
Biological.Model<- function(df, years, MPA, MPA.matrix) {
  
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
  ptm<-proc.time() #time lapse
  
  for(i in 1:nrow(df)) {
    
    data <- df[i,]

    scen <- Scenarios(data=data, years=years, MPA, MPA.matrix=MPA.matrix)  
    
    results <- rbind(results, scen)
  }
  
  print(proc.time() -ptm)
  
  return(results[-1,])
}
