res<-read.csv(here::here( "Priors_r_0.9","PatchModel_size_priors.csv"))%>%
  filter(Year <= 2065)

Data <-res%>%
  group_by(Name, Adjusted, Implementation_year, Reserve_size, Year)%>%
  mutate(Biomass_est=sum(Biomass_est)/1000, 
            Biomass_lo=sum(Biomass_lo)/1000, 
            Biomass_hi=sum(Biomass_hi)/1000,
            Catch_est=sum(Catch_est)/1000,
            Catch_lo=sum(Catch_lo)/1000, 
            Catch_hi=sum(Catch_hi)/1000, 
            PV_est=sum(PV_est)/1000000,
            PV_lo=sum(PV_lo)/1000000, 
            PV_hi=sum(PV_hi)/1000000
            )

Fisheries <- Data%>%
  filter(!(Name=="Scomberomorus"))%>%
  filter(Adjusted == "IUU_0", Reserve_size=="30%", Implementation_year==2015)

B<-ggplot(Fisheries, aes(x=Year, y=Biomass_est, group= Name, color =Name))+
  geom_line(size=1.5)+
  labs(subtitle= "Biomass for each fishery and implementation date", y="1000s MT")+
  theme_classic(base_size = 12)
B
B<- B +
  facet_wrap(~ Name, ncol=2)
B
#ggsave("Biomass_Fishery_priors.jpg", width=6, height=5, dpi=300)

C<-ggplot(Fisheries, aes(x=Year, y=Catch_est, group= Name, color =Name))+
  geom_line(size=1.5)+
  labs(subtitle= "Biomass for each fishery and implementation date", y="1000s MT")+
  theme_classic(base_size = 12)
C<- C +
  facet_wrap(~ Implementation_year, ncol=2)
C
ggsave("Catch_Fishery_priors.jpg", width=6, height=5, dpi=300)

Fishing <-ggplot(Fisheries, aes(x=Year, y=Fishing_est, group= Name, color =Name))+
  geom_line(size=1.5)+
  labs(subtitle= "Fishing", y=" Fishing effort ")+
  theme_classic(base_size = 12)
Fishing<- Fishing +
  facet_wrap(~ Implementation_year, ncol=2)
Fishing

ggsave("F_Fishery_priors.jpg", width=6, height=5, dpi=300)

# Summary of all scenarios over 50 years 
summary <-res%>%
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

plot(summary)

fit <- manova(cbind(Biomass_est, Catch_est, PV_est) ~ Ajusted, Implementation_year, Reserve_size, 
                                                      Ajusted*Implementation_year*Reserve_size, data=summary)
summary(fit, test="Pillai")

colnames(summary)




