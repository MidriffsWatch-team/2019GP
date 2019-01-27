######################################################################
#Fishing Matrix
#Defining fishing effort in and outside MPA at t=1
######################################################################

fishing.mat<- function (MPA, Fishing, MPA.mat){
  MPA.mat[MPA.mat==0]<- Fishing
  
  if (MPA > 0){
    MPA.mat[MPA.mat==1]<-0}
  else {
    MPA.mat[MPA.mat==1]<-Fishing
  }
  return (MPA.mat)
}

######################################################################
#Open Access Fishing Effort dynamic over time 
#Distribution of Fishing effort (profit and cost)
######################################################################

fishing.effort.OA<- function (p, MSY, r, bmsy, fmsy, F.mat, B.mat){

  F.mat <- F.mat/fmsy 
  b <- B.mat/bmsy
  MSY.mat <- matrix(ncol=106, nrow=106, MSY/11236) #MSY evenly distributed among all patches
 
  c <- (0.3 * p * MSY.mat)/r
  profit <- p * F.mat * b * MSY.mat - (c * r * F.mat)
  profit.msy <- p * MSY.mat - (c * r)
  revenue <- profit - c
  
  F.mat <- F.mat + 0.1 * (profit/profit.msy)
  
  F.mat <- F.mat * MSY
  
  return(list(F.mat, profit, profit.msy, revenue, c))}

#list(F.mat=F.mat, profit=profit, profit.msy=profit.msy, revenue=revenue, c=c)
######################################################################
#Adjuting price (inflation and discounting)
#
######################################################################



######################################################################
#Biological 2-D Patch Model Function
#
######################################################################

MPA.Model <- function(r, K, Fishing, B, MPA, years, MPA.mat, mrate, MSY, bmsy, fmsy, p){
  patches <- 106
  K <- K/11236
  B.mat <- matrix(ncol=patches, nrow=patches, B/11236) #Biomass at t=1 is evenly distributed among all patches 
  arriving <- matrix(nrow=nrow(MPA.matrix), ncol= ncol(MPA.matrix), 0) 
  Years<- as.vector(2015:(2015+ years))
  F.mat <- fishing.mat (MPA=MPA, Fishing=Fishing, MPA.mat=MPA.mat)
  
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
                       Cost=NA, 
                       Profit=NA,
                       Profit.msy=NA, 
                       Fishing=NA,
                       Revenue=NA)
  
  for (i in Years){
    F.mat <- fishing.effort.OA (p=p, MSY=MSY, r=r, bmsy=bmsy, fmsy=fmsy, F.mat=F.mat, B.mat=B.mat)
    F.mat<-as.numeric(F.mat[[1]])
    profit<-as.numeric(F.mat[[2]])
    profit.msy<-as.numeric(F.mat[[3]])
    revenue<-as.numeric(F.mat[[4]])
    c<-as.numeric(F.mat[[5]])
    
    leaving <- mrate*B.mat 
   
    for(row in 1:nrow(arriving)) {
      for(col in 1:ncol(arriving)) {
        arriving [row, col] <- 0.25*leaving[left.patch[row, col]]+ 
                                0.25*leaving[right.patch[row, col]] + 
                                0.25*leaving[up.patch[row, col]] + 
                                0.25*leaving[down.patch[row, col]]

      }
      } #close nested forloop for calculating arriving
    
    surplus <- r*B.mat *(1-B.mat/K)
    catches <- F.mat * B.mat
    B.mat <- B.mat + surplus - catches - leaving + arriving
    
    output<- data.frame (Year= i, 
                         Leave = sum(leaving) , 
                         Arrive = sum(arriving) , 
                         Surplus = sum(surplus),
                         Catch = sum(catches),
                         Biomass = sum(B.mat),
                         Cost = sum(c), 
                         Profit = sum (profit),
                         Profit.msy = sum(profit.msy),
                         Fishing = mean (F.mat), 
                         Revenue = sum(revenue))                      
    summary <- rbind (summary, output)
  }
  return(summary[-1,])
}

######################################################################
#Biological 2-D Patch Model Function
#looping over average, low and high estimates of variables 
######################################################################

Scenarios <- function(data, MPA, years, MPA.mat) {
  out<-data.frame(Name=NA,
                  Adjusted=NA)

  R <- as.numeric(data[,c("r", "r.low", "r.hi")])
  k <- as.numeric(data[,c("k", "k.low", "k.hi")])
  fishing <- as.numeric(data[,c("f", "f_lo", "f_hi")])
  Biomass <- as.numeric(data[,c("b", "b_lo", "b_hi")])
  P <- as.numeric(data[,c("p", "p.lo", "p.hi")])
  msy <- as.numeric(data[,c("msy", "msy.low", "msy.hi")])
  Bmsy <- as.numeric(data[,c("bmsy", "bmsy.low", "bmsy.hi")])
  Fmsy <- as.numeric(data[,c("fmsy", "fmsy_lo", "fmsy_hi")])
  mrate<- as.numeric(data[,"m.rate"])
  all.patches<-11236

  for (s in 1:3){
    r <- R[s]
    K <- k[s]
    Fishing <- fishing[s]
    B<- Biomass[s]
    mrate<- mrate
    MSY <- msy[s]
    bmsy <- Bmsy[s]
    fmsy <- Fmsy [s]
    p <- P[s]
    
    MPA<-MPA.Model(r=r, K=K, B=B, Fishing=Fishing, MPA=MPA, years=years, MPA.mat=MPA.mat, mrate=mrate, MSY=MSY, bmsy=bmsy, fmsy=fmsy, p=p)
    
    out<-(cbind(out, MPA))
  }
  out<- out[,-c(14,20)]
  
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
                  "Cost_est",
                  "Profit_est",
                  "Profit.msy_est",
                  "Fishing_est",
                  "Revenue_est",
                  "Leave_lo",
                  "Arrive_lo", 
                  "Surplus_lo",
                  "Catch_lo",
                  "Biomass_lo", 
                  "Cost_lo",
                  "Profit_lo",
                  "Profit.msy_lo",
                  "Fishing_lo",
                  "Revenue_lo",
                  "Leave_hi",
                  "Arrive_hi", 
                  "Surplus_hi",
                  "Catch_hi",
                  "Biomass_hi",
                  "Cost_hi",
                  "Profit_hi",
                  "Profit.msy_hi",
                  "Fishing_hi",
                  "Revenue_hi")
  return(out)
}


######################################################################
#Biological Patch Model Function
#looping over all Fisheries and inflated catch
######################################################################
Biological.Model<- function(df, years, MPA, MPA.mat) {
  
  results <- data.frame(matrix(ncol = 33, nrow = 0))
  x <- c("Name",
         "Adjusted", 
         "Year",
         "Leave_est",
         "Arrive_est", 
         "Surplus_est",
         "Catch_est",
         "Biomass_est",
         "Cost_est",
         "Profit_est",
         "Profit.msy_est",
         "Fishing_est",
         "Revenue_est",
         "Leave_lo",
         "Arrive_lo", 
         "Surplus_lo",
         "Catch_lo",
         "Biomass_lo", 
         "Cost_lo",
         "Profit_lo",
         "Profit.msy_lo",
         "Fishing_lo",
         "Revenue_lo",
         "Leave_hi",
         "Arrive_hi", 
         "Surplus_hi",
         "Catch_hi",
         "Biomass_hi",
         "Cost_hi",
         "Profit_hi",
         "Profit.msy_hi",
         "Fishing_hi",
         "Revenue_hi")
  colnames(results) <- x

  for(i in 1:nrow(df)) {
    
    data <- df[i,]

    scen <- Scenarios(data=data, years=years, MPA, MPA.mat=MPA.mat)  
    
    results <- rbind(results, scen)
  }
  
  return(results)
}
