############################################################
# Load data with model runs with priors and
#r.hi = 0.9 and mrate =0.9 for Scom and Micro
############################################################
options(stringsAsFactors = FALSE)

nopriors_sq <- readRDS(here::here("NoPriors","PatchModel_size_nopriors.rds"))%>%
  filter(Name=="Squatina" | Name == "Mugil")
priors <- readRDS(here::here("Priors_r_0.9","PatchModel_size_priors.rds"))


Micro0.7<- read.csv(here::here("Hybrid","Micro_nopriors_0.7.csv")) #mrate =0.7
Scom0.7<- read.csv(here::here("Hybrid","Scom_nopriors_0.7.csv"))   #mrate =0.7
Cal<- read.csv(here::here("Hybrid","Callinectes.csv"))             #priors; r=0.5

hybrid  <- priors %>%
  filter(!(Name== "Callinectes"| Name == "Micropogonias"| Name == "Scomberomorus" | Name=="Squatina" | Name == "Mugil"))%>%
  rbind(Scom0.7)%>%
  rbind(Micro0.7)%>%
  rbind(Cal)%>%
  rbind(nopriors_sq)%>%
  filter(Year <= 2065)

hybrid$Implementation_year <- factor(hybrid$Implementation_year, levels = c("Initial", "BAU", "2015", "2020", "2025", "2030"))
hybrid$Reserve_size <- factor(hybrid$Reserve_size, levels = c("BAU", "5%", "20%", "30%", "40%", "50%"))

############################################################
# Summary of all scenarios 
#
############################################################
summary <-hybrid%>%
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

############################################################
# Biomass, Catch and Profits at different reserve sizes 
#
############################################################
Line_plot <-hybrid%>%
  group_by(Adjusted, Implementation_year, Reserve_size, Year)%>%
  mutate(Biomass_est=sum(Biomass_est)/1000, 
         Biomass_lo=sum(Biomass_lo)/1000, 
         Biomass_hi=sum(Biomass_hi)/1000,
         Catch_est=sum(Catch_est)/1000,
         Catch_lo=sum(Catch_lo)/1000, 
         Catch_hi=sum(Catch_hi)/1000, 
         PV_est=sum(PV_est)/1000000,
         PV_lo=sum(PV_lo)/1000000, 
         PV_hi=sum(PV_hi)/1000000)%>%
  filter(Adjusted=="IUU_0")%>%
  filter (Implementation_year =="2015"|Implementation_year == "BAU")

B<-ggplot(Line_plot, aes(x=Year, y=Biomass_est, group=Reserve_size, color=Reserve_size))+
  geom_line(size=0.6)+
  labs(subtitle= "Biomass", y="1000s MT")+
  scale_color_discrete(name=" ")+
  theme_classic(base_size = 9)
C<-ggplot(Line_plot, aes(x=Year, y=Catch_est, group=Reserve_size, color=Reserve_size))+
  geom_line(size=0.6)+
  labs(subtitle= "Catch", y="1000s MT")+
  scale_color_discrete(name=" ")+
  theme_classic(base_size = 9)
P<-ggplot(Line_plot, aes(x=Year, y=PV_est, group=Reserve_size, color=Reserve_size))+
  geom_line(size=0.6)+
  labs(subtitle= "Profit", y= "MUSD")+
  scale_color_discrete(name=" ")+
  theme_classic(base_size = 9)
Line2<-ggarrange(B, C, P, nrow=3, common.legend = TRUE, legend="right")
ggsave("Line2_hybrid.jpg", width=4, height=4, dpi=300)

############################################################
# Biomass, Catch and Profits at different reserve sizes 
# Fishery Level Analysis 
############################################################
Fisheries <-hybrid%>%
  group_by(Name, Adjusted, Implementation_year, Reserve_size, Year)%>%
  mutate(Biomass_est=sum(Biomass_est)/1000, 
         Biomass_lo=sum(Biomass_lo)/1000, 
         Biomass_hi=sum(Biomass_hi)/1000,
         Catch_est=sum(Catch_est)/1000,
         Catch_lo=sum(Catch_lo)/1000, 
         Catch_hi=sum(Catch_hi)/1000, 
         PV_est=sum(PV_est)/1000000,
         PV_lo=sum(PV_lo)/1000000, 
         PV_hi=sum(PV_hi)/1000000)%>%
  filter(Adjusted == "IUU_0" & Implementation_year==2015 & Reserve_size == "30%")

B<-ggplot(Fisheries, aes(x=Year, y=Biomass_est, group= Name))+
  geom_line(size=0.6)+
  labs(subtitle= "Biomass for each fishery IUU_0, 2015, reserve size 30%", y="1000s MT")+
  theme_classic(base_size = 9)+
  facet_wrap(~ Name, scales="free" ,ncol=3)
ggsave("Biomass_Fishery_hybrid.jpg", width=5, height=4, dpi=300)

C<-ggplot(Fisheries, aes(x=Year, y=Catch_est, group= Name))+
  geom_line(size=0.6)+
  labs(subtitle= "Catch for each fishery IUU_0, 2015, reserve size 30%", y="1000s MT")+
  theme_classic(base_size = 9)+
  facet_wrap(~ Name, scales="free" , ncol=3)
ggsave("Catch_Fishery_hybrid.jpg", width=5, height=4, dpi=300)

Fishing <-ggplot(Fisheries, aes(x=Year, y=ffmsy, group= Name))+
  geom_line(size=0.6)+
  labs(subtitle= "ffmsy for each fishery IUU_0, 2015, reserve size 30%", y=fmsy)+
  theme_classic(base_size = 9)+
  facet_wrap(~ Name, scales="free" , ncol=3)
ggsave("F_Fishery_hybrid.jpg", width=5, height=4, dpi=300)

############################################################
# Status of Fishery in 2015 and 2065
# Kobe Plots
############################################################
bmsy<- expression(paste(B/B[MSY]))
fmsy <- expression(paste(F/F[MSY]))

initial <- read.csv(here::here("Hybrid","Genus_pts_hybrid.csv"))%>%
  filter(Adjusted=="IUU_0")

K_plot <- ggplot(initial, aes(x= bbmsy, y=ffmsy))+
  geom_rect(xmin = 0.0, xmax = 1.0, ymin = 0.0, ymax = 1, fill = 'yellow', alpha = 0.1) +
  geom_rect(xmin = 0, xmax = 1, ymin = 1.0, ymax = 7.1, fill = 'red', alpha = 0.1) +
  geom_rect(xmin = 1, xmax = 1.5, ymin = 0, ymax = 1, fill = 'green', alpha = 0.1) +
  geom_rect(xmin = 1, xmax = 2.1, ymin = 1, ymax = 7.1, fill = 'orange', alpha = 0.1)+
  geom_point(aes(size=catch), show.legend = FALSE)+
  scale_size(range = c(3, 10))+
  geom_text_repel(aes(label = Name, size =10), direction="y", nudge_x=0.05, show.legend=FALSE)+
  labs(x=bmsy, y=fmsy)+
  geom_vline(xintercept = 1, linetype="dotted", color="black") +
  geom_hline(yintercept = 1, linetype="dotted", color="black") +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic(base_size = 10)
ggsave("Kobe2015_label_hybrid.jpg", width=4, height=3, dpi=300)

OA <-hybrid%>%
  filter(Adjusted=="IUU_0", Year=="2065")

KobeOA_size<-ggplot(OA, aes(x= bbmsy, y=ffmsy, group=Name))+
  geom_point(size=2, aes(color=Implementation_year, shape=Implementation_year))+
  #scale_shape_manual(values=c(16, 17))+
  labs(title= "Fishery Status at various reserve network Sizes IUU_0", x=bmsy, y=fmsy)+
  geom_vline(xintercept = 1, linetype="dotted", color="black") +
  geom_hline(yintercept = 1, linetype="dotted", color="black") +
  theme_classic(base_size = 9)

KobeOA_size
KobeOA_size<- KobeOA_size+
  facet_wrap(~ Reserve_size, ncol=3)
KobeOA_size
ggsave("Kobe_byMRSize_hybrid.jpg", width=6, height=4, dpi=300)

ggplotly(KobeOA_size)
