######################################################################
#Fishing Matrix
#Defining fishing effort in and outside MPA at t=1
######################################################################

fishing.mat<- function (MPA, Fishing, MPA.mat){
  F.mat<-MPA.mat
  F.mat[F.mat==1]<- Fishing
  
  if (MPA==1){
    F.mat[F.mat==0]<-0}
  else {
    F.mat[F.mat==0]<-Fishing
  }
  return (F.mat)
}

######################################################################
#Open Access Fishing Effort dynamic over time 
#Distribution of Fishing effort (profit and cost)
######################################################################

fishing.effort.OA<- function (p, MSY, r, bmsy, fmsy, F.mat, B.mat, c, profit.msy, t){
  F.mat <- F.mat/fmsy 
  b <- B.mat/bmsy
  MSY <- MSY/11236 #MSY evenly distributed among all patches
  
  profit <- p * F.mat * b * MSY - (c * r * F.mat)
  
  PV <- profit/(1 + 0.1)^t
  
  F.mat <- F.mat + 0.05 * (profit/profit.msy)
  F.mat <- F.mat * fmsy
  
  return(list(F.mat, profit, PV))}


######################################################################
#Biological 2-D Patch Model Function
#
######################################################################

MPA.Model <- function(r, K, Fishing, B, MPA, years, MPA.mat, mrate, MSY, bmsy, fmsy, p, c, profit.msy, start.year){
  #start.year<- as.numeric(start.year)
  patches <- 106
  F.mat <- as.matrix(fishing.mat (MPA=MPA, Fishing=Fishing, MPA.mat=MPA.mat))
  K.mat <- matrix(ncol=patches, nrow=patches, K/11236) #K at t=1 is evently distributed among all patches 
  B.mat <- matrix(ncol=patches, nrow=patches, B/11236) #Biomass at t=1 is evenly distributed among all patches 
  arriving <- matrix(nrow=nrow(MPA.mat), ncol= ncol(MPA.mat), 0) 
  Years<- as.vector(2015:(2015+ years))
 
  
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
                       Biomass=NA, 
                       Profit=NA,
                       Fishing=NA,
                       PV=NA)
  
  for (i in Years){
    
    if(i == start.year){F.mat<- F.mat*MPA.mat} else {F.mat<-F.mat}
    
    t <- i - 1
    
    F.out <- fishing.effort.OA (t=t, p=p, MSY=MSY, r=r, bmsy=bmsy, fmsy=fmsy, 
                                F.mat=F.mat, B.mat=B.mat, c=c, profit.msy = profit.msy)
    F.mat<-(F.out[[1]])
    profit<-(F.out[[2]])
    PV <-(F.out[[3]])
    
    leaving <- mrate*B.mat 
    leaving[leaving < 0] <- 0
    for(row in 1:nrow(arriving)) {
      for(col in 1:ncol(arriving)) {
        arriving [row, col] <- 0.25*leaving[left.patch[row, col]]+ 
                                0.25*leaving[right.patch[row, col]] + 
                                0.25*leaving[up.patch[row, col]] + 
                                0.25*leaving[down.patch[row, col]]

      }
      } #close nested forloop for calculating arriving
    
    arriving[arriving < 0] <- 0
    surplus <- r * B.mat *(1 - B.mat/K.mat)
    surplus[surplus < 0] <- 0
    catches <- F.mat * B.mat
    catches[catches < 0] <- 0
    B.mat <- B.mat + surplus - catches - leaving + arriving
    B.mat[B.mat < 0] <- 0
    
    output<- data.frame (Year= i, 
                         Leave = sum(leaving) , 
                         Arrive = sum(arriving) , 
                         Surplus = sum(surplus),
                         Catch = sum(catches),
                         Biomass = sum(B.mat),
                         Profit = sum (profit),
                         Fishing = mean (F.mat), 
                         PV = sum(PV))                      
    summary <- rbind (summary, output)
  }
  return(summary[-1,])
}

######################################################################
#Biological 2-D Patch Model Function
#looping over average, low and high estimates of variables 
######################################################################

Scenarios <- function(data, MPA, years, MPA.mat, start.year) {
  out<-data.frame(Name=NA,Adjusted=NA)

  growth <- as.numeric(data[,c("r", "r.low", "r.hi")])
  car.cap <- as.numeric(data[,c("k", "k.low", "k.hi")])
  harvest <- as.numeric(data[,c("f", "f_lo", "f_hi")])
  Biom <- as.numeric(data[,c("b", "b_lo", "b_hi")])
  price <- as.numeric(data[,c("p", "p.lo", "p.hi")])
  msy <- as.numeric(data[,c("msy", "msy.low", "msy.hi")])
  Bmsy <- as.numeric(data[,c("bmsy", "bmsy.low", "bmsy.hi")])
  Fmsy <- as.numeric(data[,c("fmsy", "fmsy_lo", "fmsy_hi")])
  m.rate<- as.numeric(data[,"m.rate"])
  cost <- as.numeric(data[,c("c", "c.lo", "c.hi")])
  Profit_msy <- as.numeric(data[,c("profit.msy", "profit.msy.lo", "profit.msy.hi")])

  for (s in 1:3){
    r <- growth[s]
    K <- car.cap[s]
    Fishing <- harvest[s]
    B<- Biom[s]
    mrate<- m.rate
    MSY <- msy[s]
    bmsy <- Bmsy[s]
    fmsy <- Fmsy [s]
    p <- price[s]
    c <- cost[s]/11236
    profit.msy <- Profit_msy/11236
    
    MPA<-MPA.Model(r=r, K=K, B=B, Fishing=Fishing, MPA=MPA, years=years, MPA.mat=MPA.mat, 
                   mrate=mrate, MSY=MSY, bmsy=bmsy, fmsy=fmsy, p=p, c=c, profit.msy=profit.msy, start.year=start.year)
    
    out<-(cbind(out, MPA))
  }
  out<- out[,-c(12,21)]
  
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
                  "Profit_est",
                  "Fishing_est",
                  "PV_est",
                  "Leave_lo",
                  "Arrive_lo", 
                  "Surplus_lo",
                  "Catch_lo",
                  "Biomass_lo", 
                  "Profit_lo",
                  "Fishing_lo",
                  "PV_lo",
                  "Leave_hi",
                  "Arrive_hi", 
                  "Surplus_hi",
                  "Catch_hi",
                  "Biomass_hi",
                  "Profit_hi",
                  "Fishing_hi",
                  "PV_hi")
  return(out)
}


######################################################################
#Biological Patch Model Function
#looping over all Fisheries and inflated catch
######################################################################
Biological.Model<- function(df, years, MPA, MPA.mat, start.year) {
  
  results <- data.frame(matrix(ncol = 27, nrow = 0))
  x <- c("Name",
         "Adjusted", 
         "Year",
         "Leave_est",
         "Arrive_est", 
         "Surplus_est",
         "Catch_est",
         "Biomass_est",
         "Profit_est",
         "Fishing_est",
         "PV_est",
         "Leave_lo",
         "Arrive_lo", 
         "Surplus_lo",
         "Catch_lo",
         "Biomass_lo", 
         "Profit_lo",
         "Fishing_lo",
         "PV_lo",
         "Leave_hi",
         "Arrive_hi", 
         "Surplus_hi",
         "Catch_hi",
         "Biomass_hi",
         "Profit_hi",
         "Fishing_hi",
         "PV_hi")
  colnames(results) <- x

  for(i in 1:nrow(df)) {
    
    data <- df[i,]

    scen <- Scenarios(data=data, years=years, MPA, MPA.mat=MPA.mat, start.year=start.year)  
    
    results <- rbind(results, scen)
  }
  
  return(results)
}
