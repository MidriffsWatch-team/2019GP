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
                          r= NA, r.low = NA, r.hi = NA,
                          k= NA, k.low = NA,k.hi = NA,
                          msy= NA,msy.low = NA, msy.hi = NA,
                          bmsy= NA, bmsy.low = NA, bmsy.hi = NA,
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
    resilience <- var$Resilience
    r.low<- var$r_lo
    r.hi<- var$r_hi
    stb.low<- var$stb.low
    stb.hi<- var$stb.hi
    endb.low<- var$endb.low
    endb.hi<- var$endb.hi
    
    for (i in 1:ncol(catches)){
      catch <- catches[,i]
      x_last<- nrow(catch)
      adjusted <- colnames(catches[i])
      
      cmsy <- datalimited2::cmsy2(year=year, catch=catch, resilience = resilience, 
                                  r.low=r.low, r.hi=r.hi,
                                  stb.low =stb.low, stb.hi=stb.hi,
                                  endb.low=endb.low, endb.hi=endb.hi)
      output<- cmsy$ref_ts
      
      output<- output%>%
        mutate(Name = s, 
               Adjusted = c(adjusted))
      Name <- s
      Adjusted <- adjusted
      R <- cmsy$ref_pts[1,2:4]
      K <- cmsy$ref_pts[2,2:4]
      MSY <- cmsy$ref_pts[3,2:4]
      BMSY <- cmsy$ref_pts[5,2:4]
      tail <- cmsy$ref_ts[tail(nrow(cmsy$ref_pts)),]
      out <- cbind(Name, Adjusted, R, K, MSY, BMSY, tail)
      names(out)[1:14] <- c("Name", "Adjusted",
                            "r", "r.low", "r.hi", 
                            "k", "k.low", "k.hi",
                            "msy", "msy.low", "msy.hi",
                            "bmsy", "bmsy.low", "bmsy.hi")
      
      variables <- rbind(variables, out)
      
      ref_ts<- rbind(ref_ts, output)
    }
  }
  return (list(variables[-1,], ref_ts[-1,]))
}


######################################################################
#Open Access Fishing Effort dynamic over time 
#Distribution of Fishing effort (profit and cost)
######################################################################

fishing.effort.OA<- function (p, MSY, r, bmsy, fmsy, F.mat, B.mat, c, profit.msy, t){
  f.mat <- F.mat/fmsy 
  b.mat <- B.mat/bmsy
  MSY <- MSY/11236 
  c <- c/11236 
  profit.msy <- profit.msy/11236 
  
  revenue <- p * F.mat * B.mat
  profit <-  revenue - (c * fmsy * f.mat)
  
  PV <- profit/((1 + 0.1)^t) #10% discount rate
  
  f.mat <- f.mat + (0.1 * (profit/profit.msy)) #lambda = 0.1
  F.mat <- f.mat * fmsy
  
  return(list(F.mat, profit, PV, revenue))}

######################################################################
#Biological 2-D Patch Model Function
#
######################################################################

MPA.Model <- function(r, K, Fishing, B, years, MPA.mat, mrate, MSY, bmsy, fmsy, p, c, profit.msy, start.year){
  patches <- 106
  F.mat <- matrix(ncol=patches, nrow=patches, Fishing) 
  K.mat <- matrix(ncol=patches, nrow=patches, K/11236)  
  B.mat <- matrix(ncol=patches, nrow=patches, B/11236)  
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
                       PV=NA, 
                       Revenue=NA,
                       bbmsy=NA,
                       ffmsy=NA)
  
  for (i in Years){
    
    if(i == start.year){F.mat<- F.mat*MPA.mat} else {F.mat<-F.mat}
    
    t <- i - 2015
    
    F.out <- fishing.effort.OA (t=t, p=p, MSY=MSY, r=r, bmsy=bmsy, fmsy=fmsy, 
                                F.mat=F.mat, B.mat=B.mat, c=c, profit.msy = profit.msy)
    F.mat<-(F.out[[1]])
    profit<-(F.out[[2]])
    PV <-(F.out[[3]])
    revenue<- F.out[[4]]
    
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
                         PV = sum(PV), 
                         Revenue= sum(revenue),
                         bbmsy = sum(B.mat)/bmsy,
                         ffmsy = mean (F.mat)/fmsy)
    summary <- rbind (summary, output)
  }
  return(summary[-1,])
}

######################################################################
#Biological 2-D Patch Model Function
#looping over average, low and high estimates of variables 
######################################################################

Scenarios <- function(data, years, MPA.mat, start.year) {
  out<-data.frame(Name=NA, Adjusted=NA)

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
    c <- cost[s] 
    profit.msy <- Profit_msy[s]
    
    MPA<-MPA.Model(r=r, K=K, B=B, Fishing=Fishing, years=years, MPA.mat=MPA.mat, 
                   mrate=mrate, MSY=MSY, bmsy=bmsy, fmsy=fmsy, p=p, c=c, profit.msy=profit.msy, start.year=start.year)
    
    out<-(cbind(out, MPA))
  }
  out<- out[,-c(15,27)]
  
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
                  "Revenue_est",
                  "bbmsy", 
                  "ffmsy",
                  "Leave_lo",
                  "Arrive_lo", 
                  "Surplus_lo",
                  "Catch_lo",
                  "Biomass_lo", 
                  "Profit_lo",
                  "Fishing_lo",
                  "PV_lo",
                  "Revenue_lo",
                  "bbmsy_lo",
                  "ffmsy_lo",
                  "Leave_hi",
                  "Arrive_hi", 
                  "Surplus_hi",
                  "Catch_hi",
                  "Biomass_hi",
                  "Profit_hi",
                  "Fishing_hi",
                  "PV_hi", 
                  "Revenue_hi", 
                  "bbmsy_hi",
                  "ffmsy_hi")
  return(out)
}


######################################################################
#Biological Patch Model Function
#looping over all Fisheries and inflated catch
######################################################################
Biological.Model<- function(df, years, MPA.mat, start.year) {
  
  results <- data.frame(matrix(ncol = 36, nrow = 0))
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
         "Revenue_est",
         "bbmsy", 
         "ffmsy",
         "Leave_lo",
         "Arrive_lo", 
         "Surplus_lo",
         "Catch_lo",
         "Biomass_lo", 
         "Profit_lo",
         "Fishing_lo",
         "PV_lo",
         "Revenue_lo",
         "bbmsy_lo", 
         "ffmsy_lo",
         "Leave_hi",
         "Arrive_hi", 
         "Surplus_hi",
         "Catch_hi",
         "Biomass_hi",
         "Profit_hi",
         "Fishing_hi",
         "PV_hi", 
         "Revenue_hi",
         "bbmsy_hi", 
         "ffmsy_hi")
  colnames(results) <- x

  for(i in 1:nrow(df)) {
    
    data <- df[i,]

    scen <- Scenarios(data=data, years=years, MPA.mat=MPA.mat, start.year=start.year)  
    
    results <- rbind(results, scen)
  }
  results<-results%>%
    mutate(Implementation_year = start.year)
  return(results)
}
