nopriors <- read.csv(here::here("NoPriors", "PatchModel_size_nopriors.csv"))%>%
  filter(!(Reserve_size == "BAU" & Adjusted == "IUU_20_legal"))%>%
           filter(!(Reserve_size == "BAU" & Adjusted == "IUU_40_legal"))%>%
                    filter(!(Reserve_size == "BAU" & Adjusted == "IUU_60_legal"))
           

write.csv(nopriors, "PatchModel_size_nopriors.csv")
priors <- read.csv(here::here("Priors_r_0.9","PatchModel_size_priors.csv"))

sum_noprior <-nopriors%>%
  filter(Year <= 2065)%>%
  group_by(Adjusted, Implementation_year, Reserve_size)%>%
  summarize(Biomass_est=sum(Biomass_est)/1000, 
            Biomass_lo=sum(Biomass_lo)/1000, 
            Biomass_hi=sum(Biomass_hi)/1000,
            Catch_est=sum(Catch_est)/1000,
            Catch_lo=sum(Catch_lo)/1000, 
            Catch_hi=sum(Catch_hi)/1000, 
            PV_est=sum(PV_est)/1000000,
            PV_lo=sum(PV_lo)/1000000, 
            PV_hi=sum(PV_hi)/1000000)

sum_prior <-priors%>%
  filter(Year <= 2065)%>%
  group_by(Adjusted, Implementation_year, Reserve_size)%>%
  summarize(Biomass_est=sum(Biomass_est)/1000, 
            Biomass_lo=sum(Biomass_lo)/1000, 
            Biomass_hi=sum(Biomass_hi)/1000,
            Catch_est=sum(Catch_est)/1000,
            Catch_lo=sum(Catch_lo)/1000, 
            Catch_hi=sum(Catch_hi)/1000, 
            PV_est=sum(PV_est)/1000000,
            PV_lo=sum(PV_lo)/1000000, 
            PV_hi=sum(PV_hi)/1000000)

nopriors <- read.csv(here::here("NoPriors", "PatchModel_size_nopriors.csv"))%>%
  
priors <- read.csv(here::here("Priors_r_0.9","PatchModel_size_priors.csv"))
