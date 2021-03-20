
library(tidyverse)
library(lubridate)


chem <- read_csv("Geochemistry-Plotter/Geochem-Tidy.csv")

##### Sodium Exchange Plots ######################
x = c(1:10)
y = c(11:20)


ca.na <- chem %>%
  ggplot()+
  geom_point(aes(Ca.meq, Na.meq, colour = type, shape = type))+
  labs(title = "Exchangeable Cations", 
       x = expression("Ca"^"2+"~"(meq L"^"-1"*")"), y = expression("Na"^"+"~"(meq L"^"-1"*")"))+
  scale_colour_discrete(name = "Water Type", labels =c("Groundwater","Creek Water","Snowmelt"))
ggsave("Ca.Na.png",ca.na, scale = .75)

mg.na <- chem %>%
  ggplot()+
  geom_point(aes(Mg.meq, Na.meq, colour = type))+
  ggtitle("Magnesium and Sodium meq/L")
mg.na
ggsave("Mg.Na.png",mg.na)

#### Potassium Exchange Plots #################


ca.k <- chem %>%
  ggplot()+
  geom_point(aes(Ca.meq, K.meq, colour = type))+
  ggtitle("Calcium and Potassium meq/L")
ca.k
ggsave("Ca.K.png",ca.k)

mg.k <- chem %>%
  ggplot()+
  geom_point(aes(Mg.meq, K.meq,colour = type))+
  ggtitle("Magnesium and Potassium meq/L")
mg.k
ggsave("Mg.K.png",mg.k)

#############################################

ba.na <- chem %>%
  dplyr::select(site, type, Na, Ba)%>%
  filter(type != "blank")%>%
  ggplot()+
  geom_point(aes(Ba, Na,colour = type))
ba.na



s.so4 <- chem %>%
  dplyr::select(site, type, S, SO4)%>%
  filter(type != "blank")%>%
  ggplot()+
  geom_point(aes(S, SO4, colour = type))
s.so4

so4.site <- chem %>%
  ggplot()+
  geom_point(aes(type,S))
so4.site




  
piper.chem <- chem %>%
  dplyr::select(site, type, Cl, SO4, Ca, Mg, Na, K)%>%
  filter(Cl > 0 & SO4 >0 & Ca>0 & Mg >0 & type != "blank" & type != "snow_dup")%>%
  mutate(Ca.meq = (Ca*2/40.078),
         Mg.meq = (Mg*2/24.305),
         Na.meq = (Na*1/22.990),
         K.meq  = (as.double(K)*1/39.098),
         Cl.meq = (as.double(Cl)*1/35.45),
         S.meq  = (as.double(SO4)*2/96.056),
         Ca.pct = 100*Ca.meq/sum(Ca.meq,Mg.meq,Na.meq,K.meq),
         Mg.pct = 100*Mg.meq/sum(Ca.meq,Mg.meq,Na.meq,K.meq),
         Na.pct = 100*Na.meq/sum(Ca.meq,Mg.meq,Na.meq,K.meq),
         K.pct = 100*K.meq/sum(Ca.meq,Mg.meq,Na.meq,K.meq),
         Na.K = Na.pct + K.pct)
  
cation <- chem %>%
  dplyr::select(-Cl, -SO4, -NO2)%>%
  gather(key = "chem", value = "conc",-site,-type) %>%
  filter(conc > 0)%>%
  mutate(conc = as.double(conc))
cation

cat.plt <- cation %>%
  filter(site != "blank" & chem == "Ca" | chem == "Na" |chem == "K" | chem == "Mg")%>%
  ggplot()+
  geom_point(aes(x = chem, y = conc, colour = site, shape = type))+
  labs(x = "Cation", y = expression(Concentration~mg~L^{-1}), title = "Major Ion Concentrations")
ggsave("Cation Plot.png", cat.plt)




l <- list(Ca = piper.chem$Ca.per,
          Mg = piper.chem$Mg.per,
          Cl = as.double(piper.chem$Cl.meq),
          SO4 = as.double(piper.chem$S.meq))

p <- piper(l)

plot(p, main = "Piper-Hill AF Snow and Water Samples", cex = 1.5)
legend("topright", p@IDs, col=p@pt.col, pch=p@pt.pch)



cat.tern <- piper.chem %>%
  ggtern()+
  geom_point(aes(Ca.per, Mg.per, Na.K, colour = site, shape = type), size = 3)+
  ggtitle("Major Cations (Percent meq/L)")+
  xlab(expression(Ca^{+2}))+
  ylab(expression(Mg^{+2}))+
  zlab(expression(Na^{+1}~+~K^{+1}))+
  scale_colour_discrete(name = "Site")+
  scale_shape_discrete(name = "Sample Type")+
  theme_bw()
cat.tern
ggsave("Cation ternary.png", cat.tern)
  MU             