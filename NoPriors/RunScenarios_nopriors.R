#Run CMSY2
genus_catch<-read.csv(here::here("data", "function_inputs", "catch_Genus.csv"))%>%
  filter(Genus == "Atrina"
         |Genus == "Callinectes"
         |Genus == "Octopus"
         |Genus == "Panulirus"
         |Genus == "Cephalopholis"
         |Genus == "Dasyatis"
         |Genus == "Epinephelus"
         |Genus == "Lutjanus"
         |Genus == "Micropogonias"
         |Genus == "Mugil"
         |Genus == "Scomberomorus"
         |Genus == "Squatina")

genus_priors<-read.csv(here::here("data", "function_inputs", "Genus_priors.csv"))

Genus<-Species_CMSY2(data=genus_catch, priors=genus_priors)
Genus_pts <- Genus[[1]]
Genus_ts<- Genus[[2]]

write.csv(Genus_pts, "Genus_pts_0.9.csv")
write.csv(Genus_ts, "Genus_ts_0.9.csv")

#mrate 
mrate <- read.csv(here::here("raw_data", "mrate.csv"))

#variables from CMSY2
pts <- read.csv(here::here("Genus_pts_0.9.csv"))%>%
  merge(mrate, by="Name")%>%
  select(Name, Adjusted, r, r.low, r.hi, k, k.low, k.hi, f, f_lo, f_hi, b, b_lo, b_hi, m.rate, 
         msy, msy.low, msy.hi, bmsy, bmsy.low, bmsy.hi, fmsy, fmsy_lo, fmsy_hi)

#2018 prices
price <- read.csv(here::here("raw_data", "MarketPrice.csv"))

#open access equilibrium at 30%
data.0.3 <- merge(pts, price, by="Name")%>%
  mutate (f_bar = 2 * (1- 0.3/2),
          c = (p * f_bar * 0.3 *msy)/(fmsy * f_bar), 
          c.lo = (p.lo * f_bar * 0.3 *msy.low)/(fmsy_lo * f_bar), 
          c.hi = (p.hi * f_bar * 0.3 *msy.hi)/(fmsy_hi * f_bar), 
          profit.msy = p * msy - (c* fmsy), 
          profit.msy.lo = p.lo * msy.low - (c.lo * fmsy_lo),
          profit.msy.hi = p.hi * msy.hi - (c.hi * fmsy_hi))

write.csv(data.0.3, "input.data.0.3.csv")

#read in MPA Matrix
MPA <- as.matrix(read.csv(here::here("data", "MPA.matrix.csv"))) #489
MPA20 <- as.matrix(read.csv(here::here("Scenarios", "MPA20.mat.csv"))) 
MPA30 <- as.matrix(read.csv(here::here("Scenarios", "MPA30.mat.csv"))) 
MPA40 <- as.matrix(read.csv(here::here("Scenarios", "MPA40.mat.csv"))) 
MPA50 <- as.matrix(read.csv(here::here("Scenarios", "MPA50.mat.csv"))) 

#read in cmsy2 data
data.0.3 <- read.csv(here::here("input.data.0.3.csv"))

#run BAU
BAU <-Biological.Model(df=data.0.3, years=100, MPA.mat = MPA, start.year=0)
BAU <- BAU%>%
  mutate(Reserve_size = "BAU")%>%
  filter(!(Adjusted == "IUU_20_legal" | Adjusted == "IUU_40_legal" | Adjusted == "IUU_60_legal"))

#Run 5%
MPA_2015 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA, start.year=2015)
MPA_2020 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA, start.year=2020)
MPA_2025 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA, start.year=2025)
MPA_2030 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA, start.year=2030)

five_percent<- rbind(MPA_2015, MPA_2020, MPA_2025, MPA_2030)%>%
  mutate(Reserve_size = "5%")

#Run 20%
MPA_2015 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA20, start.year=2015)
MPA_2020 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA20, start.year=2020)
MPA_2025 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA20, start.year=2025)
MPA_2030 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA20, start.year=2030)

twenty_percent<- rbind(MPA_2015, MPA_2020, MPA_2025, MPA_2030)%>%
  mutate(Reserve_size = "20%")

#Run 30%
MPA_2015 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA30, start.year=2015)
MPA_2020 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA30, start.year=2020)
MPA_2025 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA30, start.year=2025)
MPA_2030 <- Biological.Model(df=data.0.3, years=100, MPA.mat = MPA30, start.year=2030)

thirty_percent<- rbind(MPA_2015, MPA_2020, MPA_2025, MPA_2030)%>%
  mutate(Reserve_size = "30%")

#Run 40%
MPA_2015<-Biological.Model(df=data.0.3, years=100, MPA.mat = MPA40, start.year=2015)
MPA_2020<-Biological.Model(df=data.0.3, years=100, MPA.mat = MPA40, start.year=2020) 
MPA_2025<-Biological.Model(df=data.0.3, years=100, MPA.mat = MPA40, start.year=2025)
MPA_2030<-Biological.Model(df=data.0.3, years=100, MPA.mat = MPA40, start.year=2030)

forty_percent<- rbind(MPA_2015, MPA_2020, MPA_2025, MPA_2030)%>%
  mutate(Reserve_size = "40%")

#Run 50%
MPA_2015<-Biological.Model(df=data.0.3, years=100, MPA.mat = MPA50, start.year=2015)
MPA_2020<-Biological.Model(df=data.0.3, years=100, MPA.mat = MPA50, start.year=2020) 
MPA_2025<-Biological.Model(df=data.0.3, years=100, MPA.mat = MPA50, start.year=2025)
MPA_2030<-Biological.Model(df=data.0.3, years=100, MPA.mat = MPA50, start.year=2030)

fifty_percent<- rbind(MPA_2015, MPA_2020, MPA_2025, MPA_2030)%>%
  mutate(Reserve_size = "50%")

#put it all together
MPA_size <- rbind(BAU, five_percent, twenty_percent, thirty_percent, forty_percent, fifty_percent)%>%
  write.csv("PatchModel_size.csv")







