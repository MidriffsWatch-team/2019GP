##########################
#Harvest Model
#
#########################

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

##########################
# MPA Model
#
#########################

Model<- function(r, K, fishing, biomass, patches, years, 
                 MPA.width, mrate){
  library(dplyr)
  
  f.rate <- harvest(fishing=fishing, patches=patches, MPA.width=MPA.width)
  Biomass <- vector(length=patches) 
  Biomass[]<-biomass
  left.patch<- c(patches, 1: (patches-1))
  right.patch<- c(2: patches, 1)
  
  summary<- data.frame(Year=NA,
                         Leave=NA, 
                         Arrive=NA,
                         Surplus=NA,
                         Catch=NA,
                         Biomass=NA)
  
  Years<- as.vector(2015:(2015+years))
  
  for (i in Years){
  
  leaving <-2*mrate*Biomass
  arriving <-0.5*leaving[left.patch]+ 0.5*leaving[right.patch]
  surplus <- r*Biomass *(1-Biomass/K)
  catches <- f.rate*Biomass
  Biomass <-Biomass+surplus-catches- leaving+ arriving
  output<- data.frame (Year= i, 
                          Leave = sum(leaving) , 
                          Arrive = sum(arriving) , 
                          Surplus = sum(surplus),
                          Catch = sum(catches),
                          Biomass = sum(Biomass)
                       )                      
  
  summary <- rbind (summary, output)
  }
  summary%<>%drop_na()
return(summary)
  }

MPA<-Model(r=0.920811437856805, K=280, biomass=112, fishing=0.12, patches=100,
               MPA.width=5, years=20, mrate=0.5)

#######################
# 
#
########################
MPA.model <- function(data, patches, years, MPA.width, mrate) {
  
  fishery<- as.vector(unique(data$Name))

  summary<-as.data.frame(Name=NA,
                         Adjusted=NA, 
                         Year=NA,
                         Leave_est=NA,
                         Arrive_est=NA, 
                         Surplus_est=NA,
                         Catch_est=NA,
                         Biomass_est=NA, 
                         Leave_lo=NA,
                         Arrive_lo=NA, 
                         Surplus_lo=NA,
                         Catch_lo=NA,
                         Biomass_lo=NA, 
                         Leave_hi=NA,
                         Arrive_hi=NA, 
                         Surplus_hi=NA,
                         Catch_hi=NA,
                         Biomass_hi=NA
                         )

  for(f in fishery){
    
    df<- data%>% filter(Name == f)
    IUU<- as.vector(unique(df$Adjusted))
    
    for(U in IUU){
      var<- df %>% filter(Adjusted == U)
    
      R[] <- as.vector(var[,3:5])
      k[]<- as.vector (var[,6:8])
      fishing[]<- as.vector(var[,9:11])
      Biomass[]<-as.vector (var[,12:14])
      scenario[]<- c("est", "lo", "hi")
  
            for (r in 1:length(scenario)){
              left.patch<- c(patches, 1: (patches-1))
              right.patch<- c(2: patches, 1)
              fishing<- fishing[r]
              r<- R[r]
              K <- k[r]
             
              biomass<- Biomass[r]
              scenario<-scenario [r]
              
              MPA<-Model(r=r, K=K, biomass=biomass, fishing=fishing, patches=patches,
                         MPA.width=MPA.width, years=years, mrate=mrate)
              
              summary<- cbind (summary, MPA)
              
            }
      summary<- output%>%
        mutate(Adjusted=U)
      
    }
    summary<-output%>%
      mutate(Name=f)
    }
  
  return(summary)
}
  
  