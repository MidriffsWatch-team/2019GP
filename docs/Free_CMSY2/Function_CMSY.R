############################
# CMSY Fucntion 
# 7 Focal Species 
############################

Species_CMSY2<-function(data, priors){
  library(magrittr)
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
  
  Species <- as.vector(unique(data$NombreCientifico))
  
  for (s in Species){
    df <- data%>% filter(NombreCientifico == s) 
    
    year<-as.vector(df$Ano, mode='numeric')
    
    catches<-(df[,4:7])
    
    var <-subset(priors, NombreCientifico == s)
    
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
                    year = cmsy$ref_ts[tail(1),1],	
                    catch = cmsy$ref_ts[tail(1),2],	
                    catch_ma = cmsy$ref_ts[tail(1),3],	
                    b = cmsy$ref_ts[tail(1),4],	
                    b_lo = cmsy$ref_ts[tail(1),5],	
                    b_hi = cmsy$ref_ts[tail(1),6],	
                    bbmsy = cmsy$ref_ts[tail(1),7],	
                    bbmsy_lo = cmsy$ref_ts[tail(1),8],	
                    bbmsy_hi = cmsy$ref_ts[tail(1),9],	
                    s = cmsy$ref_ts[tail(1),10],	
                    s_lo = cmsy$ref_ts[tail(1),11],	
                    s_hi = cmsy$ref_ts[tail(1),12],	
                    f = cmsy$ref_ts[tail(1),13],	
                    f_lo = cmsy$ref_ts[tail(1),14],	
                    f_hi = cmsy$ref_ts[tail(1),15],	
                    fmsy = cmsy$ref_ts[tail(1),16],	
                    fmsy_lo = cmsy$ref_ts[tail(1),17],	
                    fmsy_hi = cmsy$ref_ts[tail(1),18],	
                    ffmsy = cmsy$ref_ts[tail(1),19],	
                    ffmsy_lo = cmsy$ref_ts[tail(1),20],	
                    ffmsy_hi = cmsy$ref_ts[tail(1),21],	
                    er = cmsy$ref_ts[tail(1),22])
      
      variables <- rbind(variables, out)

      output<- data.frame(cmsy$ref_ts)
      
      output<- output%>%
        mutate(Name = c(s), 
               Adjusted = c(adjusted))
      
      ref_ts<- rbind(ref_ts, output)
    }
  }
  
  variables %<>% drop_na()
  ref_ts%<>%drop_na()
  
  return (list(variables, ref_ts))
}