############################################################
# Load data with model runs with priors and
#r.hi = 0.9 and mrate =0.9 for Scom and Micro
############################################################
options(stringsAsFactors = FALSE)
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
  filter(Adjusted=="IUU_40")%>%
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
Line2
#ggsave("Line2_hybrid.jpg", width=4, height=4, dpi=300)

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
# Status of Fishery in 2015
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
  scale_size(range = c(2, 8))+
  labs(x=bmsy, y=fmsy)+
  geom_vline(xintercept = 1, linetype="dotted", color="black") +
  geom_hline(yintercept = 1, linetype="dotted", color="black") +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(limits=c(0.7, 7.1),expand=c(0,0), breaks = c(0.7, 1, 2, 4, 6), labels= c(0.7, 1, 2, 4, 6))+
  theme_classic(base_size = 9)+
  geom_text_repel(aes(label = Name, size=4),show.legend=FALSE)
K_plot
ggsave("Kobe2015_label.jpg", width=4, height=3, dpi=300)

############################################################
# Status of Fishery in 2065
# Kobe Plots
############################################################

initial <- read.csv(here::here("Hybrid","Genus_pts_hybrid.csv"))%>%
  filter(Adjusted=="IUU_0")%>%
  mutate(Implementation_year = "Initial", Reserve_size = "BAU")%>%
  select(Name, Adjusted, Year, bbmsy, ffmsy, Implementation_year, Reserve_size)

OA_plot<-hybrid%>%
  filter(Year== 2065 & Adjusted=="IUU_0")%>%
  filter(Reserve_size == "5%" | Reserve_size =="BAU")%>%
  select(Name, Adjusted, Year, bbmsy, ffmsy, Implementation_year, Reserve_size)%>%
  rbind(initial)
OA_plot$Implementation_year <- factor(OA_plot$Implementation_year, levels = c("Initial", "BAU", "2015", "2020", "2025", "2030"))

KobeOA <- ggplot(OA_plot, aes(x= bbmsy, y=ffmsy, group=Name))+
  geom_point(size=4, aes(color= Implementation_year, shape=Implementation_year))+
  scale_shape_manual("",values=c(16, 17, 15, 18, 20))+
  scale_colour_jcolors("",palette = "pal8")+
  labs(x=bmsy, y=fmsy)+
  geom_vline(xintercept = 1, linetype="dotted", color="black") +
  geom_hline(yintercept = 1, linetype="dotted", color="black") +
  scale_y_continuous(limits=c(0.5, 7.1),expand=c(0,0), breaks = c(0.5, 1, 2, 4, 6), labels= c(0.5, 1, 2, 4, 6))+
  #scale_x_continuous(expand=c(0,0))+
  theme_classic(base_size = 9)+
  theme_classic(base_size = 9)
KobeOA

ggsave("KobeOA_hybrid.jpg", width=4, height=3, dpi=300)

############################################################
# Status of Fishery in 2065
# Kobe Plots for IUU
############################################################
initial <- read.csv(here::here("Hybrid","Genus_pts_hybrid.csv"))%>%
  mutate(Implementation_year = "Initial", Reserve_size = "BAU")%>%
  filter(Adjusted=="IUU_40"| Adjusted=="IUU_60" | Adjusted=="IUU_60_legal" | Adjusted=="IUU_40_legal")%>%
  select(Name, Adjusted, Year, bbmsy, ffmsy, Implementation_year, Reserve_size)
OA_plot2 <-hybrid%>%
  filter(Year== 2065)%>%
  filter(Reserve_size == "5%" | Reserve_size=="BAU")%>%
  filter(!(Adjusted=="IUU_0"))%>%
  select(Name, Adjusted, Year, bbmsy, ffmsy, Implementation_year, Reserve_size)%>%
  rbind(initial)
OA_plot2$Implementation_year <- factor(OA_plot2$Implementation_year, levels = c("Initial", "BAU", "2015", "2020", "2025", "2030"))

KobeOA <- ggplot(OA_plot2, aes(x= bbmsy, y=ffmsy, group=Name))+
  geom_point(size=3, aes(color= Implementation_year, shape=Implementation_year))+
  scale_shape_manual("",values=c(16, 17, 15, 18, 20))+
  scale_colour_jcolors("",palette = "pal8")+
  labs(x=bmsy, y=fmsy)+
  scale_y_continuous(limits=c(0.5, 7.1),expand=c(0,0), breaks = c(0.5, 1, 2, 4, 6), labels= c(0.5, 1, 2, 4, 6))+
  geom_vline(xintercept = 1, linetype="dotted", color="black") +
  geom_hline(yintercept = 1, linetype="dotted", color="black") +
  theme_classic(base_size = 9)
KobeOA
KobeOA_inflated<- KobeOA+
  facet_wrap(~ Adjusted, ncol=2, labeller=label_parsed)
KobeOA_inflated

ggsave("KobeOA_inflated_legal.jpg", width=5, height=4, dpi=300)


############################################################
# Status of Fishery in 2065
# Kobe Plots for Reserve Size 
############################################################
initial <- read.csv(here::here("Hybrid","Genus_pts_hybrid.csv"))%>%
  mutate(Implementation_year = "Initial", Reserve_size = "BAU")%>%
  filter(Adjusted=="IUU_0")%>%
  select(Name, Adjusted, Year, bbmsy, ffmsy, Implementation_year, Reserve_size)
OA_plot3 <-hybrid%>%
  filter(Year== 2065)%>%
  filter(Adjusted=="IUU_0")%>%
  select(Name, Adjusted, Year, bbmsy, ffmsy, Implementation_year, Reserve_size)%>%
  rbind(initial)
OA_plot3$Implementation_year <- factor(OA_plot3$Implementation_year, levels = c("Initial", "BAU", "2015", "2020", "2025", "2030"))
KobeOA <- ggplot(OA_plot3, aes(x= bbmsy, y=ffmsy, group=Name))+
  geom_point(size=3, aes(color= Implementation_year, shape=Implementation_year))+
  scale_shape_manual("",values=c(16, 17, 15, 18, 20))+
  scale_colour_jcolors("",palette = "pal8")+
  labs(x=bmsy, y=fmsy)+
  scale_y_continuous(limits=c(0.4, 7.1),expand=c(0,0), breaks = c(0.4, 1, 2, 4, 6), labels= c(0.4, 1, 2, 4, 6))+
  geom_vline(xintercept = 1, linetype="dotted", color="black") +
  geom_hline(yintercept = 1, linetype="dotted", color="black") +
  theme_classic(base_size = 9)
KobeOA
KobeOA_inflated<- KobeOA+
  facet_wrap(~ Reserve_size, ncol=2)
KobeOA_inflated

ggsave("KobeOA_size.jpg", width=5, height=4, dpi=300)
