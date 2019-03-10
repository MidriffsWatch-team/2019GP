#read in MPA Matrix
MPA <- as.matrix(read.csv(here::here("data", "MPA.matrix.csv"))) #489
MPA20 <- as.matrix(read.csv(here::here("Scenarios", "MPA20.mat.csv"))) 
MPA30 <- as.matrix(read.csv(here::here("Scenarios", "MPA30.mat.csv"))) 
MPA40 <- as.matrix(read.csv(here::here("Scenarios", "MPA40.mat.csv"))) 
MPA50 <- as.matrix(read.csv(here::here("Scenarios", "MPA50.mat.csv"))) 

#read in cmsy2 data
data.0.3 <- read.csv(here::here("Scenarios", "input.data.0.3.csv"))

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







