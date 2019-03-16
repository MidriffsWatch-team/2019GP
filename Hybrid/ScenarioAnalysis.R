library(dplyr)
library(ggplot2)
library(RColorBrewer)
#######################################################################
#Read in Model Runs calculated metric tons and million USD
#######################################################################
hybrid<- readRDS(here::here("PatchModel_hybrid.rds"))%>%
  filter(Year <= 2065)%>%
  filter(Reserve_size == "BAU" | Reserve_size == "5%" |Reserve_size == "30%"| Reserve_size == "50%")%>%
  filter(!(Implementation_year == 2025))%>%
  filter(!(Adjusted == "IUU_20" | Adjusted == "IUU_20_legal"))%>%
  mutate (Biomass_est=(Biomass_est)/1000, 
            Biomass_lo=(Biomass_lo)/1000, 
            Biomass_hi=(Biomass_hi)/1000,
            Catch_est=(Catch_est)/1000,
            Catch_lo=(Catch_lo)/1000, 
            Catch_hi=(Catch_hi)/1000, 
            PV_est=(PV_est)/1000000,
            PV_lo=(PV_lo)/1000000, 
            PV_hi=(PV_hi)/1000000)
#######################################################################
#Create business as usual dataframe 
#######################################################################
BAU <- hybrid%>%
  filter(Reserve_size=="BAU")
IUU40<- BAU%>%
  filter(Adjusted == "IUU_40")%>%
  mutate(Adjusted = "IUU_40_legal")
IUU60<- BAU%>%
  filter(Adjusted == "IUU_60")%>%
  mutate(Adjusted = "IUU_60_legal")

BAU_com<- rbind(BAU, IUU40, IUU60)%>% 
  select(Name, Adjusted, Year, Biomass_est, Biomass_lo, Biomass_hi, 
                              Catch_est, Catch_lo, Catch_hi,
                              PV_est, PV_lo, PV_hi)
names(BAU_com)[4:12]<- c("BAU_B_est", "BAU_B_lo", "BAU_B_hi", 
                     "BAU_C_est", "BAU_C_lo", "BAU_C_hi",
                     "BAU_PV_est", "BAU_PV_lo", "BAU_PV_hi")
Join_BAU <-full_join(hybrid, BAU_com, by = c("Name", "Adjusted", "Year"))
#######################################################################
#
#######################################################################
summary_yr <- Join_BAU%>%
  group_by(Adjusted, Implementation_year, Reserve_size, Year)%>%
  summarize(Biomass_est = sum(Biomass_est), 
            Biomass_lo = sum(Biomass_lo), 
            Biomass_hi =sum(Biomass_hi),
            Catch_est = sum(Catch_est),
            Catch_lo = sum(Catch_lo), 
            Catch_hi = sum(Catch_hi), 
            PV_est = sum(PV_est),
            PV_lo = sum(PV_lo), 
            PV_hi = sum(PV_hi),
            BAU_B_est = sum(BAU_B_est),
            BAU_B_lo = sum(BAU_B_lo),
            BAU_B_hi = sum(BAU_B_hi),
            BAU_C_est = sum(BAU_C_est),
            BAU_C_lo = sum(BAU_C_lo),
            BAU_C_hi = sum(BAU_C_hi),
            BAU_PV_est = sum(BAU_PV_est),
            BAU_PV_lo = sum(BAU_PV_lo),
            BAU_PV_hi = sum(BAU_PV_hi))

#Calculates cummulative sum at each year based on factor groups 
payoff_scen <- summary_yr %>%
  group_by(Adjusted, Implementation_year, Reserve_size) %>%
  arrange(Year, .by_group = TRUE)%>%
  mutate (  B_est_cum = cumsum(Biomass_est),
            B_lo_cum = cumsum(Biomass_lo),
            B_hi_cum = cumsum(Biomass_hi),
            C_est_cum = cumsum(Catch_est),
            C_lo_cum = cumsum(Catch_lo),
            C_hi_cum = cumsum(Catch_hi),
            PV_est_cum = cumsum(PV_est),
            PV_lo_cum = cumsum(PV_lo),
            PV_hi_cum = cumsum(PV_hi),
            BAU_B_est_cum = cumsum(BAU_B_est),
            BAU_B_lo_cum = cumsum(BAU_B_lo),
            BAU_B_hi_cum = cumsum(BAU_B_hi),
            BAU_C_est_cum = cumsum(BAU_C_est),
            BAU_C_lo_cum = cumsum(BAU_C_lo),
            BAU_C_hi_cum = cumsum(BAU_C_hi),
            BAU_PV_est_cum = sum(BAU_PV_est),
            BAU_PV_lo_cum = cumsum(BAU_PV_lo),
            BAU_PV_hi_cum = cumsum(BAU_PV_hi))

#Finds all years where catch in scenario is greater than in BAU
itercept_cross <- payoff_scen%>%
  group_by(Adjusted, Implementation_year, Reserve_size)%>%
  subset(subset = Catch_est > BAU_C_est)%>%
  mutate(Loss =  BAU_C_est_cum - C_est_cum)%>%
  select(Adjusted, Implementation_year, Reserve_size, Year, Catch_est, BAU_C_est, C_est_cum, BAU_C_est_cum, Loss)%>%
  filter(!(Reserve_size == "BAU"))

#Selects the earlier year where scenario catch is greater than BAU          
sliced<- itercept_cross%>%
  group_by(Adjusted, Implementation_year, Reserve_size)%>%
  filter(Year==min(Year))%>%
  mutate(Year_cross= Year)

payoff<- sliced%>%
  select(Adjusted, Implementation_year, Reserve_size, Year_cross)

#Calculates amount of pay off that needs to be aquired in sceanrio after the lines cross  
merged<- itercept_cross %>%
  merge(payoff, by=c("Adjusted", "Implementation_year", "Reserve_size"))%>%
  mutate(Payoff = C_est_cum - BAU_C_est_cum)

#Subset the year in which the the scenarios pays off "Loss" 
itercept_payoff <- merged %>%
  group_by(Adjusted, Implementation_year, Reserve_size)%>%
  subset(subset = Payoff >= Loss )%>%
  filter(Year==min(Year))%>%
  mutate(Year_payoff= Year)

#######################################################################
#Calculate change relative to BAU for each scenario 
#######################################################################
summary_all <- Join_BAU%>%
  group_by(Adjusted, Implementation_year, Reserve_size)%>%
  summarize(B_est = sum(Biomass_est) - sum(BAU_B_est),
            B_lo = sum(Biomass_lo) - sum(BAU_B_lo),
            B_hi = sum(Biomass_hi) - sum(BAU_B_hi),
            C_est = sum(Catch_est) - sum(BAU_C_est),
            C_lo = sum(Catch_lo) - sum(BAU_C_lo),
            C_hi = sum(Catch_hi) - sum(BAU_C_hi),
            PV_est = sum(PV_est) - sum(BAU_PV_est),
            PV_lo = sum(PV_lo) - sum(BAU_PV_lo),
            PV_hi = sum(PV_hi) - sum(BAU_PV_hi),
            B_est_p = (B_est / sum(BAU_B_est))*100,
            B_lo_p = (B_lo / sum(BAU_B_lo))*100,
            B_hi_p = (B_hi / sum(BAU_B_hi))*100,
            C_est_p = (C_est / sum(BAU_C_est))*100,
            C_lo_p = (C_lo / sum(BAU_C_lo))*100,
            C_hi_p = (C_hi / sum(BAU_C_hi))*100,
            PV_est_p = PV_est / sum(BAU_PV_est),
            PV_lo_p = (PV_lo / sum(BAU_PV_lo))*100,
            PV_hi_p = (PV_hi / sum(BAU_PV_hi))*100)%>%
  select(Adjusted, Implementation_year, Reserve_size, B_est, C_est, PV_est, B_est_p, C_est_p, PV_est_p)%>%
  full_join(itercept_payoff, by=c("Adjusted", "Implementation_year", "Reserve_size"))

change_year <- Join_BAU%>%
  group_by(Adjusted, Implementation_year, Reserve_size, Year)%>%
      summarize(B_est = sum(Biomass_est) - sum(BAU_B_est),
                B_lo = sum(Biomass_lo) - sum(BAU_B_lo),
                B_hi = sum(Biomass_hi) - sum(BAU_B_hi),
                C_est = sum(Catch_est) - sum(BAU_C_est),
                C_lo = sum(Catch_lo) - sum(BAU_C_lo),
                C_hi = sum(Catch_hi) - sum(BAU_C_hi),
                PV_est = sum(PV_est) - sum(BAU_PV_est),
                PV_lo = sum(PV_lo) - sum(BAU_PV_lo),
                PV_hi = sum(PV_hi) - sum(BAU_PV_hi),
                B_est_p = (B_est / sum(BAU_B_est))*100,
                B_lo_p = (B_lo / sum(BAU_B_lo))*100,
                B_hi_p = (B_hi / sum(BAU_B_hi))*100,
                C_est_p = (C_est / sum(BAU_C_est))*100,
                C_lo_p = (C_lo / sum(BAU_C_lo))*100,
                C_hi_p = (C_hi / sum(BAU_C_hi))*100,
                PV_est_p = PV_est / sum(BAU_PV_est),
                PV_lo_p = (PV_lo / sum(BAU_PV_lo))*100,
                PV_hi_p = (PV_hi / sum(BAU_PV_hi))*100)
#######################################################################
#Create graphs to check output
#######################################################################
plot1<- change_year%>%
  filter(Adjusted == "IUU_0", Implementation_year==2020)


B<-ggplot(plot1, aes(x=Year, y=B_est, group=Reserve_size, color=Reserve_size))+
  geom_line(size=0.8)+
  labs(subtitle= "Biomass", y="1000s MT")+
  theme_classic(base_size = 9)+
  labs(colour="Reserve size")

C<-ggplot(plot1, aes(x=Year, y=C_est, group=Reserve_size, color=Reserve_size))+
  geom_line(size=0.8)+
  labs(subtitle= "Catch", y="1000s MT")+
  theme_classic(base_size = 9)+
  labs(colour="Reserve size")

P<-ggplot(plot1, aes(x=Year, y=PV_est, group=Reserve_size, color=Reserve_size))+
  geom_line(size=0.8)+
  labs(subtitle= "Profit", y= "MUSD")+
  theme_classic(base_size = )+
  labs(colour="Reserve size")

Line1<-ggarrange(B, C, P, nrow=3, common.legend = TRUE, legend="bottom")
Line1
ggsave("Line1.jpg", width=3, height=4, dpi=300)

#######################################################################
#bubble graph 
#######################################################################
bubble <- Join_BAU%>%
  group_by(Adjusted, Implementation_year, Reserve_size)%>%
  summarize(Biomass_est = sum(Biomass_est), 
            Biomass_lo = sum(Biomass_lo), 
            Biomass_hi =sum(Biomass_hi),
            Catch_est = sum(Catch_est),
            Catch_lo = sum(Catch_lo), 
            Catch_hi = sum(Catch_hi), 
            PV_est = sum(PV_est),
            PV_lo = sum(PV_lo), 
            PV_hi = sum(PV_hi))


bubble_plot<- ggplot(bubble, aes(x= Biomass_est, y=PV_est, color=Reserve_size, shape= Implementation_year))+
  geom_point(aes(size=Catch_est))+
  facet_wrap(~Adjusted)
bubble_plot

ggsave("bubble.jpg", width=10, height=6, dpi=300)
