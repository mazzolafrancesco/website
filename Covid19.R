install.packages("openxlsx")
install.packages("plm")
install.packages("sandwich")
install.packages("lmtest")
install.packages("foreign")
install.packages("strucchange")
library(openxlsx)
library(dplyr)
library(tidyr)
library(plm)
library(sandwich)
library(lmtest)
library(ggplot2)
library(foreign)
library(Hmisc)
library(strucchange)
library(zoo)
library(forcats)

# population --------------------------------------------------------------
pop_comune<-read.xlsx("https://www.dropbox.com/scl/fi/1ugjc8tczvgitvngik2xq/pop_comune.xlsx?dl=1&rlkey=qkhm46a9ebxoyaqsiewzag6hh")
pop_comune$comune<-substr(pop_comune$comune,1,6)
pop_comune$pop_18<-as.numeric(pop_comune$pop_18) 
pop_comune <- pop_comune %>% filter(!is.na(pop_comune$pop_18))

density<-read.xlsx("https://www.dropbox.com/scl/fi/7s3zfh763sz6pc41mdcoo/Dati-comunali-e-provinciali.xlsx?dl=1&rlkey=12xhj0v2fp7j9p4izgfuh7gj4",sheet="Dati comunali")
density$Codice.Comune<-as.character(density$Codice.Comune)
density$Codice.Comune<-ifelse(nchar(density$Codice.Comune)<5,paste0("0",as.character(density$Codice.Comune)),as.character(density$Codice.Comune))
density$Codice.Comune<-ifelse(nchar(density$Codice.Comune)<6,paste0("0",as.character(density$Codice.Comune)),as.character(density$Codice.Comune))
density<-density[colnames(density) %in% c("Codice.Comune","Superficie.totale.(Km2)")]

dip_anziani<-read.xlsx("https://www.dropbox.com/s/23uecfut60tcx6w/indice_dipendenza_anziani.xlsx?dl=1",sheet="Sheet2")
dip_anziani<-dip_anziani[!is.na(dip_anziani$provincia),names(dip_anziani) != "X1"]
dip_anziani$dip_anziani<-dip_anziani$last/100
dip_anziani$provincia<-substr(dip_anziani$provincia,14,50)

# Business 25/03 ----------------------------------------------------------
#import datasets
ind_active<-read.xlsx("https://www.dropbox.com/scl/fi/vr9mh4ufowf8xmip23ww5/dati_comunali_2017_DPCM_covid19.xlsx?dl=1&rlkey=c3iel7xmpb3lw6ey8dgi2swtd",sheet="Settori attivi_industria")
serv_act<-read.xlsx("https://www.dropbox.com/scl/fi/vr9mh4ufowf8xmip23ww5/dati_comunali_2017_DPCM_covid19.xlsx?dl=1&rlkey=c3iel7xmpb3lw6ey8dgi2swtd",sheet="settori attivi servizi")
ind_susp<-read.xlsx("https://www.dropbox.com/scl/fi/vr9mh4ufowf8xmip23ww5/dati_comunali_2017_DPCM_covid19.xlsx?dl=1&rlkey=c3iel7xmpb3lw6ey8dgi2swtd",sheet="settori sospesi industria")
serv_susp<-read.xlsx("https://www.dropbox.com/scl/fi/vr9mh4ufowf8xmip23ww5/dati_comunali_2017_DPCM_covid19.xlsx?dl=1&rlkey=c3iel7xmpb3lw6ey8dgi2swtd",sheet="settori sospesi servizi")


ind_active$`Valore_aggiunto.(valori.in.euro)`<-as.numeric(ind_active$`Valore_aggiunto.(valori.in.euro)`)
ind_active$`Fatturato.(valori.in.euro)`<-as.numeric(ind_active$`Fatturato.(valori.in.euro)`)
ind_active$`Numero.Addetti`<-as.numeric(ind_active$`Numero.Addetti`)
ind_active$`Numero.Dipendenti`<-as.numeric(ind_active$`Numero.Dipendenti`)
ind_active$`Unita_locali`<-as.numeric(ind_active$`Unita_locali`)
ind_susp$`Valore_aggiunto.(valori.in.euro)`<-as.numeric(ind_susp$`Valore_aggiunto.(valori.in.euro)`)
ind_susp$`Fatturato.(valori.in.euro)`<-as.numeric(ind_susp$`Fatturato.(valori.in.euro)`)
ind_susp$`Numero.Addetti`<-as.numeric(ind_susp$`Numero.Addetti`)
ind_susp$`Numero.Dipendenti`<-as.numeric(ind_susp$`Numero.Dipendenti`)
ind_susp$`Unita_locali`<-as.numeric(ind_susp$`Unita_locali`)

serv_act$`Valore_aggiunto.(valori.in.euro)`<-as.numeric(serv_act$`Valore_aggiunto.(valori.in.euro)`)
serv_act$`Fatturato.(valori.in.euro)`<-as.numeric(serv_act$`Fatturato.(valori.in.euro)`)
serv_act$`Numero.Addetti`<-as.numeric(serv_act$`Numero.Addetti`)
serv_act$`Numero.Dipendenti`<-as.numeric(serv_act$`Numero.Dipendenti`)
serv_act$`Unita_locali`<-as.numeric(serv_act$`Unita_locali`)
serv_susp$`Valore_aggiunto.(valori.in.euro)`<-as.numeric(serv_susp$`Valore_aggiunto.(valori.in.euro)`)
serv_susp$`Fatturato.(valori.in.euro)`<-as.numeric(serv_susp$`Fatturato.(valori.in.euro)`)
serv_susp$`Numero.Addetti`<-as.numeric(serv_susp$`Numero.Addetti`)
serv_susp$`Unita_locali`<-as.numeric(serv_susp$`Unita_locali`)
serv_susp$`Numero.Dipendenti`<-as.numeric(serv_susp$`Numero.Dipendenti`)

ind<-inner_join(ind_susp[colnames(ind_susp) %in% c("Denominazione_provincia","codice_comune","Unita_locali","Numero.Addetti","Valore_aggiunto.(valori.in.euro)")],
                ind_active[colnames(ind_active) %in% c("Denominazione_provincia","codice_comune","Unita_locali","Numero.Addetti","Valore_aggiunto.(valori.in.euro)")],
                by=c("codice_comune","Denominazione_provincia")) #.x is active, .y is suspended;

ind$susp_act_va<-ind$`Valore_aggiunto.(valori.in.euro).x`/(ind$`Valore_aggiunto.(valori.in.euro).x`+ind$`Valore_aggiunto.(valori.in.euro).y`)
ind$susp_act_units<-ind$Unita_locali.x/(ind$Unita_locali.x+ind$Unita_locali.y)
ind$susp_act_empl<-ind$Numero.Addetti.x/(ind$Numero.Addetti.x+ind$Numero.Addetti.y)


serv<-inner_join(serv_susp[colnames(serv_susp) %in% c("Denominazione_provincia","codice_comune","Unita_locali","Numero.Addetti","Valore_aggiunto.(valori.in.euro)")],
                 serv_act[colnames(serv_act) %in% c("Denominazione_provincia","codice_comune","Unita_locali","Numero.Addetti","Valore_aggiunto.(valori.in.euro)")],
                 by=c("codice_comune","Denominazione_provincia")) #.x is active, .y is suspended;

serv$susp_act_va<-serv$`Valore_aggiunto.(valori.in.euro).x`/(serv$`Valore_aggiunto.(valori.in.euro).x`+serv$`Valore_aggiunto.(valori.in.euro).y`)
serv$susp_act_units<-serv$Unita_locali.x/(serv$Unita_locali.x+serv$Unita_locali.y)
serv$susp_act_empl<-serv$Numero.Addetti.x/(serv$Numero.Addetti.x+serv$Numero.Addetti.y)

#remove(ind_active,ind_susp,serv_act,serv_susp)

totbus_susp<-inner_join(ind_susp[colnames(ind_susp) %in% c("Denominazione_provincia","codice_comune","Unita_locali","Numero.Addetti","Valore_aggiunto.(valori.in.euro)","Fatturato.(valori.in.euro)")],
                        serv_susp[colnames(serv_susp) %in% c("Denominazione_provincia","codice_comune","Unita_locali","Numero.Addetti","Valore_aggiunto.(valori.in.euro)","Fatturato.(valori.in.euro)")],
                        by=c("codice_comune","Denominazione_provincia")) #.x is ind, .y is serv;
totbus_susp$va_susp<-totbus_susp$`Valore_aggiunto.(valori.in.euro).x`+totbus_susp$`Valore_aggiunto.(valori.in.euro).y`
totbus_susp$empl_susp<-totbus_susp$Numero.Addetti.x+totbus_susp$Numero.Addetti.y
totbus_susp$units_susp<-totbus_susp$Unita_locali.x+totbus_susp$Unita_locali.y
totbus_susp$rev_susp<-totbus_susp$`Fatturato.(valori.in.euro).x`+totbus_susp$`Fatturato.(valori.in.euro).y`

totbus_active<-inner_join(ind_active[colnames(ind_active) %in% c("Denominazione_provincia","codice_comune","Unita_locali","Numero.Addetti","Valore_aggiunto.(valori.in.euro)","Fatturato.(valori.in.euro)")],
                          serv_act[colnames(serv_act) %in% c("Denominazione_provincia","codice_comune","Unita_locali","Numero.Addetti","Valore_aggiunto.(valori.in.euro)","Fatturato.(valori.in.euro)")],
                          by=c("codice_comune","Denominazione_provincia")) #.x is ind, .y is serv;
totbus_active$va_act<-totbus_active$`Valore_aggiunto.(valori.in.euro).x`+totbus_active$`Valore_aggiunto.(valori.in.euro).y`
totbus_active$empl_act<-totbus_active$Numero.Addetti.x+totbus_active$Numero.Addetti.y
totbus_active$units_act<-totbus_active$Unita_locali.x+totbus_active$Unita_locali.y
totbus_active$rev_act<-totbus_active$`Fatturato.(valori.in.euro).x`+totbus_active$`Fatturato.(valori.in.euro).y`

totbus<-inner_join(totbus_susp[colnames(totbus_susp) %in% c("Denominazione_provincia","codice_comune","va_susp","empl_susp","units_susp","rev_susp","Fatturato.(valori.in.euro).y")],
                   totbus_active[colnames(totbus_active) %in% c("Denominazione_provincia","codice_comune","va_act","empl_act","units_act","rev_act")],
                   by=c("codice_comune","Denominazione_provincia"))
totbus$susp_act_va<-totbus$va_susp/(totbus$va_susp+totbus$va_act)
totbus$susp_act_empl<-totbus$empl_susp/(totbus$empl_susp+totbus$empl_act)
totbus$susp_act_units<-totbus$units_susp/(totbus$units_susp+totbus$units_act)
totbus$susp_act_empl<-ifelse(is.na(totbus$susp_act_empl),totbus$susp_act_units,totbus$susp_act_empl)

remove(ind_active,ind_susp,serv_act,serv_susp,totbus_active,totbus_susp)

totbus$codice_provincia<-substr(totbus$codice_comune,1,3)
totbus$labint<-(totbus$empl_susp)/(totbus$units_susp)

# pop_comune$provincia<-substr(pop_comune$comune,1,3)
# maxpop_p<-pop_comune %>% group_by(provincia) %>% 
#   mutate(maxpop_p=max(pop_18,na.rm=TRUE))
# maxpop_p$maxpop_p<-ifelse(maxpop_p$maxpop_p==maxpop_p$pop_18,maxpop_p$maxpop_p,NA)
# maxpop_p<-maxpop_p[!is.na(maxpop_p$maxpop_p),colnames(maxpop_p) %in% c("comune","maxpop_p","provincia")]
# 
# maxpop_p<-left_join(maxpop_p,totbus[colnames(totbus) %in% c("codice_comune","susp_act_empl","susp_act_units")],by=c("comune"="codice_comune"))
# names(maxpop_p)[names(maxpop_p) == "susp_act_empl"] <- "susp_act_empl_p"
# names(maxpop_p)[names(maxpop_p) == "susp_act_units"] <- "susp_act_units_p"
# totbus<-left_join(totbus,maxpop_p[colnames(maxpop_p) %in% c("provincia","susp_act_empl_p","susp_act_units_p")],by=c("codice_provincia"="provincia"))



# Business 11/03 ----------------------------------------------------------
# units1103nord<-read.csv("https://www.dropbox.com/s/r0we0n1ha5fua0q/units_1103_nord.csv?dl=1")
# units1103sud<-read.csv("https://www.dropbox.com/s/iqsvsw2r75y09lq/units_1103_sud.csv?dl=1")
# units1103<-rbind(units1103sud,units1103nord) %>% distinct()
# remove(units1103sud,units1103nord)
# units1103$D3<-as.character(units1103$D3)
# units1103$D1<-as.character(units1103$D1)
# units1103$provincia<-substr(units1103$D1,1,3)
# units1103<-units1103[units1103$Ateco.2007 != "TOTALE" & 
#                        !(units1103$D3) %in% c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","Z"),]
# units1103_prov<-units1103[nchar(as.character(units1103$D1))<6 & nchar(as.character(units1103$D1))>4,]
# #units1103<-units1103[substr(units1103$D1,1,1)=="0" | substr(units1103$D1,1,1)=="1",]
# units1103<-units1103[nchar(units1103$D1)>5,]
# units1103_susp<-units1103 %>% filter((D3) %in% c("451","452",
#                                                  "473", "474", "477", "478", 
#                                                  "561","563",
#                                                  "96")) %>%
#   group_by(D1) %>% summarise(units_susp=sum(Value))
# 
# units1103_tot<-full_join(totbus[colnames(totbus) %in% c("codice_comune","units_act","units_susp")],units1103_susp,by=c("codice_comune"="D1"))
# units1103_tot$susp_act_units<-units1103_tot$units_susp.y/(units1103_tot$units_susp.x+units1103_tot$units_act)
empl1103<-read.csv("https://www.dropbox.com/s/55vlcwsk8ifzx1g/empl_1103.csv?dl=1")
empl1103$D3<-as.character(empl1103$D3)
empl1103$D1<-as.character(empl1103$D1)
empl1103$provincia<-substr(empl1103$D1,1,3)
empl1103<-empl1103[empl1103$Ateco.2007 != "TOTALE" & 
                     !(empl1103$D3) %in% c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","Z"),]
empl1103_prov<-empl1103[nchar(as.character(empl1103$D1))<6 & nchar(as.character(empl1103$D1))>4,]
empl1103_prov<-empl1103_prov[]
#empl1103<-empl1103[substr(empl1103$D1,1,1)=="0" | substr(empl1103$D1,1,1)=="1",]
empl1103<-empl1103[nchar(empl1103$D1)>5,]
empl1103_susp<-empl1103 %>% filter(D3 %in% c("451","452",
                                             "473", "474", "477", "478", 
                                             "561","563",
                                             "96")) %>%
  group_by(D1) %>% summarise(empl_susp=sum(Value))

retail<-empl1103%>% filter(D3 %in% c("451","452","473", "474", "477", "478")) %>% group_by(D1) %>% summarise(retail=sum(Value))
food<-empl1103%>% filter(D3 %in% c("561","563")) %>% group_by(D1) %>% summarise(food=sum(Value))
pers<-empl1103%>% filter(D3 %in% c("96")) %>% group_by(D1) %>% summarise(pers=sum(Value))

scuolasport<-empl1103 %>% filter(D3 %in% c("85","931")) %>%
  group_by(D1) %>% summarise(empl_scuolasport=sum(Value))

empl1103_tot<-left_join(totbus[colnames(totbus) %in% c("codice_comune","empl_act","empl_susp")],empl1103_susp,by=c("codice_comune"="D1"))
empl1103_tot<-left_join(empl1103_tot,retail,by=c("codice_comune"="D1"))
empl1103_tot<-left_join(empl1103_tot,food,by=c("codice_comune"="D1"))
empl1103_tot<-left_join(empl1103_tot,pers,by=c("codice_comune"="D1"))

empl1103_tot<-left_join(empl1103_tot,scuolasport,by=c("codice_comune"="D1"))
empl1103_tot$empl_scuolasport<-ifelse(is.na(empl1103_tot$empl_scuolasport),0,empl1103_tot$empl_scuolasport)
empl1103_tot$shut11<-empl1103_tot$empl_susp.y/(empl1103_tot$empl_susp.x+empl1103_tot$empl_act-empl1103_tot$empl_scuolasport) #-empl1103_tot$empl_scuolasport
empl1103_tot$shut11<-ifelse(empl1103_tot$empl_susp.y>empl1103_tot$empl_susp.x,
                            empl1103_tot$empl_susp.x/(empl1103_tot$empl_susp.x+empl1103_tot$empl_act-empl1103_tot$empl_scuolasport), #-empl1103_tot$empl_scuolasport
                            empl1103_tot$shut11)

empl1103_tot$ret11<-ifelse(is.na(empl1103_tot$retail),0,empl1103_tot$retail/(empl1103_tot$empl_susp.x+empl1103_tot$empl_act-empl1103_tot$empl_scuolasport))
empl1103_tot$food11<-ifelse(is.na(empl1103_tot$food),0,empl1103_tot$food/(empl1103_tot$empl_susp.x+empl1103_tot$empl_act-empl1103_tot$empl_scuolasport))
empl1103_tot$pers11<-ifelse(is.na(empl1103_tot$pers),0,empl1103_tot$pers/(empl1103_tot$empl_susp.x+empl1103_tot$empl_act-empl1103_tot$empl_scuolasport))

remove(empl1103,empl1103_susp,empl1103_prov,scuolasport,retail,food,pers)

#Tutte le attività commerciali al dettaglio: 
#45 COMMERCIO ALL'INGROSSO E AL DETTAGLIO E RIPARAZIONE DI AUTOVEICOLI E MOTOCICLI (45.1, 45.2, 45.3, 45.4) 
#47 COMMERCIO AL DETTAGLIO (ESCLUSO QUELLO DI AUTOVEICOLI E DI MOTOCICLI) (47.3, 47.4, 47.5, 47.6, 47.7, 47.8, 47.9)
#56 ATTIVITA DEI SERVIZI DI RISTORAZIONE (56.1, 56.3)
#96 ALTRE ATTIVITÀ DI SERVIZI PER LA PERSONA (96.0)
#tranne: 
#47.1 iper/supermercati
#56.2 catering;
#96.01 Lavanderia e pulitura di articoli tessili e pelliccia (also 96.01.1 attivita delle lavanderie industriali, 96.01.2 altre lavanderie, tintorie)
#96.03 Servizi di pompe funebri e attivita connesse

#ISTRUZIONE: 85
#ATTIVITA SPORTIVE: 931 

# correl ------------------------------------------------------------------
correl<-inner_join(totbus,empl1103_tot,by="codice_comune")
#correl<-correl[!is.na(correl$susp_act_empl) | !is.na(correl$shut11),]
correl<-correl[!is.na(correl$susp_act_empl) & !is.na(correl$shut11),]
correl<-correl[colnames(correl) %in% c("codice_comune","susp_act_empl","empl_act.x","empl_susp.x","empl_susp.y","labint",
                                       "va_susp","va_act","shut11","empl_scuolasport","ret11","food11","pers11")]
correl<-inner_join(correl,pop_comune,by=c("codice_comune"="comune"))

#correl<-correl[correl$codice_comune %in% unique(dailydeaths$COD_PROVCOM),]
correl$province<-substr(correl$codice_comune,1,3)
correl$vapop_m<-(correl$va_susp+correl$va_act)/correl$pop_18

cor(correl$shut11,correl$susp_act_empl)
plot(correl$shut11,correl$susp_act_empl) #y is 1103

correl$shut25<-ifelse(correl$empl_susp.x>correl$empl_susp.y,
                      (correl$empl_susp.x-correl$empl_susp.y)/(correl$empl_act.x+correl$empl_susp.x-correl$empl_scuolasport), #-correl$empl_scuolasport
                      0)
#correl$shut25<-correl$susp_act_empl-correl$shut11
hist(correl$shut25)
cor(correl$shut25,correl$shut11) 

correl<-inner_join(correl,density,by=c("codice_comune"="Codice.Comune"))
correl$density<-correl$pop_18/correl$`Superficie.totale.(Km2)`

#Codogno, Castiglione d’Adda, Casalpusterlengo, Fombio, Maleo, Somaglia, Bertonico, Terranova dei Passerini, Castelgerundo e San Fiorano
#098019, 098014, 098010, 098026, 098035, 098054, 098002, 098057, 098062, 098047
correl<-correl[!(correl$codice_comune) %in% c("098019","098014","098010","098026","098035","098054","098002","098057","098062","098047"),]

#shares of shutdown1103, incr2503, open;
correl$open<-1-correl$susp_act_empl
a <- correl[colnames(correl) %in% c("codice_comune","open")]
a$propsector<-"a_open"
names(a)[names(a) == "open"] <- "value"
b <- correl[colnames(correl) %in% c("codice_comune","shut11")]
b$propsector<-"c_shut11"
names(b)[names(b) == "shut11"] <- "value"
c <- correl[colnames(correl) %in% c("codice_comune","shut25")]
c$propsector<-"b_shut25"
names(c)[names(c) == "shut25"] <- "value"
data <- rbind(a,b,c)
# Stacked
data = data[with(data, order(propsector,value)), ]
data %>% 
  ggplot(aes(fill=propsector, y=value*100, x=fct_reorder2(codice_comune,propsector, value),width=1)) + 
  geom_bar(position="stack", stat="identity") + coord_cartesian(ylim=c(0, 100), expand=FALSE) +
  labs(y= "Exposure (in %)") +
  scale_fill_discrete(name = "Proportions", labels = c("Open", "SecondExposure", "FirstExposure")) +
  ggtitle("Sectoral shutdown exposure") + 
  theme(legend.position="bottom",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
remove(data,a,b,c)

rcorr(as.matrix(correl[colnames(correl) %in% c("food11","ret11","pers11")]))

# 4433 death registry -----------------------------------------------------
fulldaily<-read.csv("https://www.dropbox.com/s/fyj9tc54tvahea4/comuni_giornaliero_full.csv?dl=1")
fulldaily$COD_PROVCOM<-ifelse(nchar(as.character(fulldaily$COD_PROVCOM))<5,paste0("0",as.character(fulldaily$COD_PROVCOM)),as.character(fulldaily$COD_PROVCOM))
fulldaily$COD_PROVCOM<-ifelse(nchar(as.character(fulldaily$COD_PROVCOM))<6,paste0("0",as.character(fulldaily$COD_PROVCOM)),as.character(fulldaily$COD_PROVCOM))
fulldaily$GE<-ifelse(nchar(as.character(fulldaily$GE))<4,paste0("0",as.character(fulldaily$GE)),as.character(fulldaily$GE))
fulldaily$month<-substr(fulldaily$GE,1,2)
#fulldaily <- fulldaily %>% filter(CL_ETA %in% c("14","15","16","17","18","19","20","21"))#65+
#fulldaily <- fulldaily %>% filter(CL_ETA %in% c("17","18","19","20","21"))#80+
# fulldaily$T_20<-fulldaily$M_20
# fulldaily$T_19<-fulldaily$M_19
# fulldaily$T_18<-fulldaily$M_18
# fulldaily$T_17<-fulldaily$M_17
# fulldaily$T_16<-fulldaily$M_16
# fulldaily$T_15<-fulldaily$M_15

geog_codes<-fulldaily[colnames(fulldaily) %in% c("COD_PROVCOM","NOME_REGIONE","NOME_PROVINCIA","TIPO_COMUNE")] %>% filter(!is.na(fulldaily$NOME_REGIONE) | !is.na(fulldaily$NOME_PROVINCIA)) %>% unique()
#add Balmuccia (002008), Rassa (002110) e Malvicino (006090) in Piemonte e Pedesina (014047) in Lombardia;
fulldaily<-fulldaily %>% add_row(TIPO_COMUNE="1", T_20 = 0, T_19=0, T_18=0, T_17=0, T_16=0, T_15=0, COD_PROVCOM = "002008", NOME_COMUNE="Balmuccia", NOME_REGIONE="Piemonte")
fulldaily<-fulldaily %>% add_row(TIPO_COMUNE="1", T_20 = 0, T_19=0, T_18=0, T_17=0, T_16=0, T_15=0, COD_PROVCOM = "002110", NOME_COMUNE="Rassa", NOME_REGIONE="Piemonte")
fulldaily<-fulldaily %>% add_row(TIPO_COMUNE="1", T_20 = 0, T_19=0, T_18=0, T_17=0, T_16=0, T_15=0, COD_PROVCOM = "006090", NOME_COMUNE="Malvicino", NOME_REGIONE="Piemonte")
fulldaily<-fulldaily %>% add_row(TIPO_COMUNE="1", T_20 = 0, T_19=0, T_18=0, T_17=0, T_16=0, T_15=0, COD_PROVCOM = "014047", NOME_COMUNE="Pedesina", NOME_REGIONE="Lombardia")  
fulldaily<-fulldaily %>% filter(TIPO_COMUNE %in% c("1","2")) # & month !="01") # 1=Dati fino al 15 aprile 2020; 2=Dati fino al 31 marzo 2020
fulldaily_1<-fulldaily %>% filter(TIPO_COMUNE=="1") %>% complete(COD_PROVCOM, nesting(GE), fill = list(T_20 = 0, T_19=0,T_18=0,T_17=0,T_16=0,T_15=0))
fulldaily_2<-fulldaily %>% filter(TIPO_COMUNE=="2" & !month %in% c("04")) %>% complete(COD_PROVCOM, nesting(GE), fill = list(T_20 = 0, T_19=0,T_18=0,T_17=0,T_16=0,T_15=0))
fulldaily<-rbind(fulldaily_1,fulldaily_2) 
fulldaily<-left_join(fulldaily[!is.na(fulldaily$COD_PROVCOM),!colnames(fulldaily) %in% c("NOME_REGIONE","NOME_PROVINCIA","NOME_COMUNE","REG","PROV","month","TIPO_COMUNE")],geog_codes,by="COD_PROVCOM")
fulldaily$month<-substr(fulldaily$GE,1,2)
remove(fulldaily_2,fulldaily_1)
fulldaily<-fulldaily %>% 
  filter(!GE %in% c(NA, #"0405","0406","0407","0408","0409","0410","0411","0412","0413","0414","0415",
                    "0229","0416","0417","0418","0419","0420","0421","0422","0423","0424","0425","0426","0427","0428","0429","0430","0431")) #&
#"0215","0214","0213","0212","0211","0210","0209","0208","0207","0206","0205","0204","0203","0202","0201")
fulldaily$saturday<-ifelse(fulldaily$GE %in% c("0201","0208","0215","0222","0229","0307","0314","0321","0328","0404","0411"),1,0)
fulldaily$sunday<-ifelse(fulldaily$GE %in% c("0202","0209","0216","0223","0301","0308","0315","0322","0329","0405","0412"),1,0)
fulldaily$monday<-ifelse(fulldaily$GE %in% c("0203","0210","0217","0224","0302","0309","0316","0323","0330","0406","0413"),1,0)
fulldaily$tuesday<-ifelse(fulldaily$GE %in% c("0204","0211","0218","0225","0303","0310","0317","0324","0331","0407","0414"),1,0)
fulldaily$wednesday<-ifelse(fulldaily$GE %in% c("0205","0212","0219","0226","0304","0311","0318","0325","0401","0408","0415"),1,0)
fulldaily$thursday<-ifelse(fulldaily$GE %in% c("0206","0213","0220","0227","0305","0312","0319","0326","0402","0409"),1,0)
fulldaily$friday<-ifelse(fulldaily$GE %in% c("0207","0214","0221","0228","0306","0313","0320","0327","0403","0410"),1,0)

fulldaily %>% group_by(GE) %>% summarise(unique_muni=n_distinct(COD_PROVCOM)) %>% ggplot(aes(x= GE, y=unique_muni,group=1)) + geom_line() +theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90))
fulldaily$T_20<-as.numeric(as.character(fulldaily$T_20))
fulldaily$T_19<-as.numeric(as.character(fulldaily$T_19))
fulldaily$T_18<-as.numeric(as.character(fulldaily$T_18))
fulldaily$T_17<-as.numeric(as.character(fulldaily$T_17))
fulldaily$T_16<-as.numeric(as.character(fulldaily$T_16))
fulldaily$T_15<-as.numeric(as.character(fulldaily$T_15))

#total pop
fulldaily_1 <- fulldaily %>% group_by(GE,COD_PROVCOM) %>% summarise(T_20_c=sum(T_20))
#rolling weekly averages;
fulldaily_2 <- fulldaily %>% group_by(COD_PROVCOM,GE) %>%
  summarise(T_15=sum(T_15),T_16=sum(T_16),T_17=sum(T_17),T_18=sum(T_18),T_19=sum(T_19))
fulldaily_2 <- fulldaily_2 %>% #group_by(COD_PROVCOM,GE) %>%
  arrange(COD_PROVCOM,GE) %>%
  mutate(avg1519_c=(rollmean(T_15,7,fill = NA)+rollmean(T_16,7,fill = NA)+
                      rollmean(T_17,7,fill = NA)+rollmean(T_18,7,fill = NA)+rollmean(T_19,7,fill = NA))/5)

fulldaily %>% 
  filter(!month %in% c("04")) %>%
  group_by(GE) %>% summarise(T_20=sum(T_20,na.rm=TRUE),T_19=sum(T_19,na.rm=TRUE),T_18=sum(T_18,na.rm=TRUE),T_17=sum(T_17,na.rm=TRUE),
                             T_16=sum(T_16,na.rm=TRUE),T_16=sum(T_16,na.rm=TRUE),T_15=sum(T_15,na.rm=TRUE)) %>% 
  ggplot( aes(x= GE, y=T_20, group=1,color="2020")) + 
  geom_line(size=2) + 
  geom_line(aes(y=T_19,color="2019"),linetype = "dashed")+ 
  geom_line(aes(y=T_18,color="2018"),linetype = "dashed")+ 
  geom_line(aes(y=T_17,color="2017"),linetype = "dashed")+ 
  geom_line(aes(y=T_16,color="2016"),linetype = "dashed")+ 
  geom_line(aes(y=T_15,color="2015"),linetype = "dashed")+ 
  theme(axis.text.x = element_text(face = "bold", size = 8, angle = 90)) +  
  theme(legend.position="bottom") +
  #geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0311'))) + #1st lockdown
  #geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0325')))  + #2nd lockdown 
  ggtitle("Daily deaths in Italy over January-March 2020 and in the preceding years") +
  #scale_x_continuous() +
  labs (x = "Day", y= "Deaths, sum across Italian regions", color = "Legend") +
  scale_color_manual(name = "Years",
                     values = c("2015" = "black","2016" = "green", "2017" = "orange", "2018" = "yellow", "2019" = "blue", "2020" = "red"),
                     labels = c("2015", "2016", "2017", "2018", "2019", "2020"))


fulldaily<-fulldaily[colnames(fulldaily) %in% c("NOME_REGIONE","NOME_PROVINCIA","COD_PROVCOM","GE","month","TIPO_COMUNE",
                                                "monday","tuesday","wednesday","thursday","friday","saturday","sunday")] %>% unique()
fulldaily<-left_join(fulldaily,fulldaily_1,by=c("COD_PROVCOM","GE"))
fulldaily<-left_join(fulldaily,fulldaily_2,by=c("COD_PROVCOM","GE"))
#fulldaily<-left_join(fulldaily,fulldaily_3,by=c("COD_PROVCOM","GE"))
#fulldaily<-left_join(fulldaily,fulldaily_4,by=c("COD_PROVCOM","GE"))

fulldaily$excD<-ifelse(fulldaily$T_20_c-fulldaily$avg1519_c<0,0,fulldaily$T_20_c-fulldaily$avg1519_c)
fulldaily<-fulldaily[!is.na(fulldaily$excD),!names(fulldaily) %in% c("T_15","T_16","T_17","T_18","T_19")]
#fulldaily$excDe<-ifelse(fulldaily$T_20_ce-fulldaily$avg1519_ce<0,0,fulldaily$T_20_c-fulldaily$avg1519_c)
remove(fulldaily_1,fulldaily_2,fulldaily_3,fulldaily_4)

#density,dip_anziani,pop_18
fulldaily<-left_join(fulldaily,pop_comune[colnames(pop_comune) != "comune_name"], by=c("COD_PROVCOM"="comune"))
fulldaily<-left_join(fulldaily,density,by=c("COD_PROVCOM"="Codice.Comune"))
fulldaily$density<-fulldaily$pop_18/fulldaily$`Superficie.totale.(Km2)`
fulldaily<-left_join(fulldaily,dip_anziani[colnames(dip_anziani) %in% c("dip_anziani","provincia")],by=c("NOME_PROVINCIA"="provincia"))

fulldaily<-fulldaily[!is.na(fulldaily$pop_18),]

fulldaily<-fulldaily %>%
  group_by(COD_PROVCOM) %>%   #group by comune
  #calculate growth as (quarter / 5-period-lagged quarter) - 1
  arrange(GE) %>%
  mutate(cumulT_20_c = cumsum(excD))
fulldaily$cumulT_20_c_pop<-fulldaily$cumulT_20_c/fulldaily$pop_18

#Codogno, Castiglione d’Adda, Casalpusterlengo, Fombio, Maleo, Somaglia, Bertonico, Terranova dei Passerini, Castelgerundo e San Fiorano
#"098019", "098014", "098010", "098026", "098035", "098054", "098002", "098057", "098062", "098047"
#Vo': 028105; Parma: 034027 (02/26);
#big cities: "058091", "015146", "063049", "001272", "082053", "010025", "037006", "048017"

fulldaily %>% 
  filter(COD_PROVCOM %in% c("098019", "098014", "098010", "098026", "098035", "098054", "098002", "098057", "098062", "098047")) %>%
  #group_by(GE) %>% mutate(cumulT_20_c_pop=mean(cumulT_20_c_pop,na.rm=TRUE)) %>% 
  ggplot( aes(x= GE, y=cumulT_20_c_pop*10000, group=COD_PROVCOM, color=COD_PROVCOM)) + 
  geom_line()+ 
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) +  
  theme(legend.position="bottom") +
  #scale_y_continuous(name="Deaths",breaks=seq(0,.03,.0005)) + #, limits=c(0, 150)) 
  geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0311')))  #+ lockdown lombardia
#geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0311')))  #1st lockdown
#geom_rect(aes(xmin= which(levels(as.factor(GE)) == '0302'), xmax= which(levels(as.factor(GE)) == '0312'), ymin=-Inf, ymax=Inf), fill=.1) 
#geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0325')))   #2nd lockdown 

# identify 1st death in a m: t=0;
fulldaily_scaled<-fulldaily #fulldaily$month!="01" #[fulldaily$pop_18>10000,]
# rice <- ts(fulldaily_scaled[fulldaily_scaled$COD_PROVCOM %in% c("098019"),]$cumulT_20_c_pop*10000)
# bp.rice <- breakpoints(rice ~ 1) #store the breakpoints
# summary(bp.rice)
# plot(bp.rice) #the BIC chooses 5 breakpoints; plot the graph with breakdates and their confidence intervals
# plot(rice)
# lines(bp.rice)
# ci.rice <- confint(bp.rice) ## confidence intervals
# ci.rice
# lines(ci.rice)

#fulldaily_scaled<-fulldaily_scaled %>% group_by(COD_PROVCOM) %>% arrange(GE) %>% mutate(strucbreak=quantile(cumulT_20_c_pop, probs=.1, na.rm=TRUE))
fulldaily_scaled<-fulldaily_scaled %>% group_by(COD_PROVCOM) %>% arrange(GE) %>% mutate(strucbreak=sd(cumulT_20_c_pop,na.rm=TRUE))

#fulldaily_scaled %>% filter(COD_PROVCOM=="001272") %>% arrange(GE) %>% summarise(strucbreak=sd(cumulT_20_c_pop*10000,na.rm=TRUE))
fulldaily_scaled$t<-ifelse(fulldaily_scaled$cumulT_20_c_pop-fulldaily_scaled$strucbreak>0,1,0) 
fulldaily_scaled_1<- fulldaily_scaled %>% arrange (GE) %>% filter(t==1) %>% group_by(COD_PROVCOM) %>%
  mutate(timearrival=row_number())
fulldaily_scaled<-left_join(fulldaily_scaled,fulldaily_scaled_1[colnames(fulldaily_scaled_1) %in% c("COD_PROVCOM","GE","timearrival")],by=c("COD_PROVCOM","GE"))
remove(fulldaily_scaled_1)

fulldaily_scaled$tau0311<-ifelse(fulldaily_scaled$GE=="0311","-10",
                                 ifelse(fulldaily_scaled$GE=="0312","-9",ifelse(fulldaily_scaled$GE=="0313","-8",ifelse(fulldaily_scaled$GE=="0314","-7",
                                                                                                                        ifelse(fulldaily_scaled$GE=="0315","-6",ifelse(fulldaily_scaled$GE=="0316","-5",ifelse(fulldaily_scaled$GE=="0317","-4",
                                                                                                                                                                                                               ifelse(fulldaily_scaled$GE=="0318","-3",ifelse(fulldaily_scaled$GE=="0319","-2",ifelse(fulldaily_scaled$GE=="0320","-1",
                                                                                                                                                                                                                                                                                                      ifelse(fulldaily_scaled$GE=="0321","0",ifelse(fulldaily_scaled$GE=="0322","1",ifelse(fulldaily_scaled$GE=="0323","2",
                                                                                                                                                                                                                                                                                                                                                                                           ifelse(fulldaily_scaled$GE=="0324","3",ifelse(fulldaily_scaled$GE=="0325","4",ifelse(fulldaily_scaled$GE=="0326","5",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ifelse(fulldaily_scaled$GE=="0327","6",ifelse(fulldaily_scaled$GE=="0328","7",ifelse(fulldaily_scaled$GE=="0329","8",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ifelse(fulldaily_scaled$GE=="0330","9",ifelse(fulldaily_scaled$GE=="0331","10",ifelse(fulldaily_scaled$GE=="0401","11",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ifelse(fulldaily_scaled$GE=="0402","12",ifelse(fulldaily_scaled$GE=="0403","13",ifelse(fulldaily_scaled$GE=="0404","14",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(fulldaily_scaled$GE=="0310","-11",ifelse(fulldaily_scaled$GE=="0309","-12",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(fulldaily_scaled$GE=="0308","-13",ifelse(fulldaily_scaled$GE=="0307","-14",ifelse(fulldaily_scaled$GE=="0306","-15",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ifelse(fulldaily_scaled$GE=="0305","-16",ifelse(fulldaily_scaled$GE=="0304","-17",ifelse(fulldaily_scaled$GE=="0303","-18",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(fulldaily_scaled$GE=="0302","-19",ifelse(fulldaily_scaled$GE=="0301","-20",ifelse(fulldaily_scaled$GE=="0229","-21",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ifelse(fulldaily_scaled$GE=="0228","-22",ifelse(fulldaily_scaled$GE=="0227","-23",ifelse(fulldaily_scaled$GE=="0226","-24",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ifelse(fulldaily_scaled$GE=="0225","-25",ifelse(fulldaily_scaled$GE=="0224","-26",ifelse(fulldaily_scaled$GE=="0223","-27",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ifelse(fulldaily_scaled$GE=="0222","-28",ifelse(fulldaily_scaled$GE=="0221","-29",ifelse(fulldaily_scaled$GE=="0220","-30",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ifelse(fulldaily_scaled$GE=="0219","-31",ifelse(fulldaily_scaled$GE=="0218","-32",ifelse(fulldaily_scaled$GE=="0217","-33",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 NA))))))))))))))))))))))))))))))))))))))))))))))))
fulldaily_scaled$tau0311<-ifelse(fulldaily_scaled$GE=="0405","15",ifelse(fulldaily_scaled$GE=="0406","16",ifelse(fulldaily_scaled$GE=="0407","17",
                                                                                                                 ifelse(fulldaily_scaled$GE=="0408","18",ifelse(fulldaily_scaled$GE=="0409","19",ifelse(fulldaily_scaled$GE=="0410","20",
                                                                                                                                                                                                        ifelse(fulldaily_scaled$GE=="0411","21",ifelse(fulldaily_scaled$GE=="0412","22",ifelse(fulldaily_scaled$GE=="0413","23",
                                                                                                                                                                                                                                                                                               ifelse(fulldaily_scaled$GE=="0414","24",ifelse(fulldaily_scaled$GE=="0415","25",
                                                                                                                                                                                                                                                                                                                                              ifelse(fulldaily_scaled$GE=="0216","-34",ifelse(fulldaily_scaled$GE=="0215","-35",ifelse(fulldaily_scaled$GE=="0214","-36",
                                                                                                                                                                                                                                                                                                                                                                                                                                       ifelse(fulldaily_scaled$GE=="0213","-37",ifelse(fulldaily_scaled$GE=="0212","-38",ifelse(fulldaily_scaled$GE=="0211","-39",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ifelse(fulldaily_scaled$GE=="0210","-40",ifelse(fulldaily_scaled$GE=="0209","-41",ifelse(fulldaily_scaled$GE=="0208","-42",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ifelse(fulldaily_scaled$GE=="0207","-43",ifelse(fulldaily_scaled$GE=="0206","-44",ifelse(fulldaily_scaled$GE=="0205","-45",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(fulldaily_scaled$GE=="0204","-46",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         fulldaily_scaled$tau0311))))))))))))))))))))))))

fulldaily_scaled$tau0311<-as.numeric(as.character(fulldaily_scaled$tau0311))
nohit <- fulldaily_scaled %>% group_by(COD_PROVCOM) %>% summarise(maxdeath=max(cumulT_20_c_pop,na.rm=TRUE)*10000) %>% filter(maxdeath<10)
fulldaily_scaled<-fulldaily_scaled[!fulldaily_scaled$COD_PROVCOM %in% nohit$COD_PROVCOM,]


#Codogno, Castiglione d’Adda, Casalpusterlengo, Fombio, Maleo, Somaglia, Bertonico, Terranova dei Passerini, Castelgerundo e San Fiorano
#"098019", "098014", "098010", "098026", "098035", "098054", "098002", "098057", "098062", "098047"

fulldaily_scaled %>% 
  arrange(tau0311) %>%
  filter(timearrival<70 & !is.na(cumulT_20_c_pop)) %>% #&
  #  COD_PROVCOM %in% c("098019", "098014", "098010", "098026", "098035", "098054", "098002", "098057", "098062", "098047")) %>%
  group_by(timearrival) %>% mutate(cumulT_20_c_pop=mean(cumulT_20_c_pop,na.rm=TRUE)) %>% 
  ggplot( aes(x= timearrival, y=cumulT_20_c_pop*10000, group=1)) + 
  geom_line()+ 
  #stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  #stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  #stat_smooth(method = 'nls', formula = y ~ a * exp(b * x), aes(colour = 'Exponential'), se = FALSE, method.args = list(start = list(a = 1, b = -.0001))) +
  #stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(colour = 'logarithmic'), se = FALSE, method.args = list(start = list(a = 1, b = 1))) +
  #stat_smooth(se = F, method = "lm", formula = y ~ poly(x, 18),level = 1-1e-7) +#aes(x = seq(length(GE)),y=death, group=factor(high_susp)), se = F, method = "lm", formula = y ~ poly(x, 8)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) +  
  theme(legend.position="bottom") +
  theme_bw() # +
#scale_colour_brewer(name = 'Trendline', palette = 'Set2')+
#geom_vline(aes(xintercept = 0))  + #lockdown lombardia
#geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0311'))) + #1t lockdown
#geom_rect(aes(xmin= 10, xmax= 35), ymin=-Inf, ymax=Inf, fill=.1) 
#geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0325')))   #2nd lockdown 

fulldaily_scaled$weekofyear<-ifelse(fulldaily_scaled$GE %in% c("0201","0202","0203","0204","0205","0206","0207"),1,
                                    ifelse(fulldaily_scaled$GE %in% c("0208","0209","0210","0211","0212","0213","0214"),2,
                                           ifelse(fulldaily_scaled$GE %in% c("0215","0216","0217","0218","0219","0220","0221"),3,
                                                  ifelse(fulldaily_scaled$GE %in% c("0222","0223","0224","0225","0226","0227","0228"),4,
                                                         ifelse(fulldaily_scaled$GE %in% c("0301","0302","0303","0304","0305","0306","0307"),5,
                                                                ifelse(fulldaily_scaled$GE %in% c("0308","0309","0310","0311","0312","0313","0314"),6,
                                                                       ifelse(fulldaily_scaled$GE %in% c("0315","0316","0317","0318","0319","0320","0321"),7,
                                                                              ifelse(fulldaily_scaled$GE %in% c("0322","0323","0324","0325","0326","0327","0328"),8,
                                                                                     ifelse(fulldaily_scaled$GE %in% c("0329","0330","0331","0401","0402","0403","0404"),9,
                                                                                            ifelse(fulldaily_scaled$GE %in% c("0405","0406","0407","0408","0409","0410","0411"),10,
                                                                                                   ifelse(fulldaily_scaled$GE %in% c("0412","0413","0414","0415"),11,NA)))))))))))

fulldaily_scaled<-fulldaily_scaled[!fulldaily_scaled$COD_PROVCOM %in% c("098019", "098014", "098010", "098026", "098035", "098054", "098002", "098057", "098062", "098047"),]
paralleltrend<-inner_join(fulldaily_scaled[colnames(fulldaily_scaled) %in% c("COD_PROVCOM")],
                          correl[colnames(correl) %in% c("codice_comune","residual","shut25","shut11","susp_act_empl","va_susp","va_act")],
                          by=c("COD_PROVCOM"="codice_comune")) %>% distinct()
paralleltrend$high_susp<-ifelse(paralleltrend$shut11>quantile(paralleltrend$shut11, probs=.5, na.rm=TRUE),1,NA)
paralleltrend$high_susp<-ifelse(paralleltrend$shut11<quantile(paralleltrend$shut11, probs=.5, na.rm=TRUE),0,paralleltrend$high_susp)

paralleltrend2<-inner_join(fulldaily_scaled[!is.na(fulldaily_scaled$timearrival) & !is.na(fulldaily_scaled$COD_PROVCOM) &
                                              !fulldaily_scaled$COD_PROVCOM %in% nohit$COD_PROVCOM & fulldaily_scaled$pop_18>2500,
                                            colnames(fulldaily_scaled) %in% c("COD_PROVCOM","timearrival","tau0311")],
                           correl[colnames(correl) %in% c("codice_comune","shut11")],
                           by=c("COD_PROVCOM"="codice_comune")) %>% distinct()

weekarrival<- paralleltrend2[paralleltrend2$tau0311==0,colnames(paralleltrend2) %in% c("COD_PROVCOM","timearrival"),] %>% unique()
weekarrival$weekarrival<-ifelse(weekarrival$timearrival>=0 & weekarrival$timearrival<quantile(weekarrival$timearrival,probs=.1,na.rm=TRUE), 1,
                                ifelse(weekarrival$timearrival>=quantile(weekarrival$timearrival,probs=.1,na.rm=TRUE) & weekarrival$timearrival<quantile(weekarrival$timearrival,probs=.2,na.rm=TRUE), 2,
                                       ifelse(weekarrival$timearrival>=quantile(weekarrival$timearrival,probs=.2,na.rm=TRUE) & weekarrival$timearrival<quantile(weekarrival$timearrival,probs=.3,na.rm=TRUE), 3,
                                              ifelse(weekarrival$timearrival>=quantile(weekarrival$timearrival,probs=.3,na.rm=TRUE) & weekarrival$timearrival<quantile(weekarrival$timearrival,probs=.4,na.rm=TRUE), 4,
                                                     ifelse(weekarrival$timearrival>=quantile(weekarrival$timearrival,probs=.4,na.rm=TRUE) & weekarrival$timearrival<quantile(weekarrival$timearrival,probs=.5,na.rm=TRUE), 5,
                                                            ifelse(weekarrival$timearrival>=quantile(weekarrival$timearrival,probs=.5,na.rm=TRUE) & weekarrival$timearrival<quantile(weekarrival$timearrival,probs=.6,na.rm=TRUE), 6,
                                                                   ifelse(weekarrival$timearrival>=quantile(weekarrival$timearrival,probs=.6,na.rm=TRUE) & weekarrival$timearrival<quantile(weekarrival$timearrival,probs=.7,na.rm=TRUE), 7,
                                                                          ifelse(weekarrival$timearrival>=quantile(weekarrival$timearrival,probs=.7,na.rm=TRUE) & weekarrival$timearrival<quantile(weekarrival$timearrival,probs=.8,na.rm=TRUE), 8,
                                                                                 ifelse(weekarrival$timearrival>=quantile(weekarrival$timearrival,probs=.8,na.rm=TRUE) & weekarrival$timearrival<quantile(weekarrival$timearrival,probs=.9,na.rm=TRUE), 9,
                                                                                        ifelse(weekarrival$timearrival>=quantile(weekarrival$timearrival,probs=.9,na.rm=TRUE), 10,
                                                                                               NA))))))))))

# ifelse(weekarrival$timearrival>0 & weekarrival$timearrival<7, 1,
#        ifelse(weekarrival$timearrival>=7 & weekarrival$timearrival<14, 2,
#               ifelse(weekarrival$timearrival>=14 & weekarrival$timearrival<21, 3,
#                      ifelse(weekarrival$timearrival>=21 & weekarrival$timearrival<28, 4,
#                             ifelse(weekarrival$timearrival>=28 & weekarrival$timearrival<35, 5,
#                                    ifelse(weekarrival$timearrival>=35 & weekarrival$timearrival<42, 6,
#                                           ifelse(weekarrival$timearrival>=42 & weekarrival$timearrival<49, 7,
#                                                  ifelse(weekarrival$timearrival>=49 & weekarrival$timearrival<56, 8,
#                                                         ifelse(weekarrival$timearrival>=56 & weekarrival$timearrival<63, 9,
#                                                                ifelse(weekarrival$timearrival>=63 & weekarrival$timearrival<70, 10,
#                                                                       ifelse(weekarrival$timearrival>=70 & weekarrival$timearrival<77, 11,
#                                                                              ifelse(weekarrival$timearrival>=77, 12,
#                                                                                     NA))))))))))))

weekarrival<-weekarrival[!is.na(weekarrival$weekarrival),]
describe(weekarrival$weekarrival)
paralleltrend2<-inner_join(paralleltrend2,weekarrival[!colnames(weekarrival) %in% c("timearrival")],by=c("COD_PROVCOM"))


paralleltrend2<-paralleltrend2 %>%
  filter(#COD_PROVCOM %in% listcomune & !is.na(tau0311) &
    tau0311==0)  %>%
  group_by(weekarrival) %>% mutate(high_susp=shut11-median(shut11))
paralleltrend2$high_susp<-ifelse(paralleltrend2$high_susp>=0,1,0)

# paralleltrend2<-paralleltrend2 %>% group_by(weekarrival) %>% #mutate(high_susp=shut11-median(shut11))
#       filter(tau0311==10)  %>%
#       mutate(high_susp=shut11-quantile(shut11, probs=.75, na.rm=TRUE),
#       low_susp=quantile(shut11, probs=.25, na.rm=TRUE)-shut11)
#   paralleltrend2$high_susp<-ifelse(paralleltrend2$high_susp>0,1,ifelse(paralleltrend2$low_susp>0,0,NA))

#dd<-left_join(fulldaily_scaled,paralleltrend,by=c("COD_PROVCOM"))
dd<-left_join(fulldaily_scaled,paralleltrend2[!colnames(paralleltrend2) %in% c("tau0311","timearrival","weekarrival","shut11")],by=c("COD_PROVCOM"))
dd$high_susp<-factor(dd$high_susp)
dd<-dd[!is.na(dd$high_susp),] # & !is.na(dd$density) & !is.na(dd$dip_anziani),]

dd<-dd %>% group_by(high_susp,tau0311) %>% #
  mutate(death=sum(excD,na.rm=TRUE)/sum(pop_18,na.rm=TRUE)*100000) %>% #mean(excD/pop_18,na.rm=TRUE)*100000) #sum(excD,na.rm=TRUE)/sum(pop_18,na.rm=TRUE)*100000)
  select(high_susp,death,timearrival,GE,COD_PROVCOM,density,dip_anziani,tau0311,month,pop_18,
         monday,tuesday,wednesday,thursday,friday,friday,saturday,weekofyear)#,vapop_m,susp_act_empl.y,incr_exp,tau0311)

dd<-dd[!is.na(dd$death),]
dd$month<-substr(dd$GE,1,2)
dd$province<-substr(dd$COD_PROVCOM,1,3)
#dd$logdeath1<-log(dd$death+1)

# dd_res_lm = lm(death ~ monday + tuesday + wednesday + thursday + friday + saturday + province - 1,
#                data=dd) #lag(death,-1)
# dd$resid_death <- residuals(dd_res_lm)#exp(dd$death-predict(dd_res_lm))-1 # Save the residual values
# dd<-dd %>% group_by(high_susp,GE) %>%
#   mutate(resid_death=mean(resid_death,na.rm=TRUE)) %>%
#   select(high_susp,death,month,GE,vapop_m,COD_PROVCOM,month,resid_death,tau0311,timearrival)
# summary(dd_res_lm)

dd <- dd %>% group_by(COD_PROVCOM) %>% arrange(desc(GE)) %>% mutate(ma=rollmean(death, 2,fill = list(NA, NULL, NA)))


dd %>%
  filter(!is.na(high_susp) & month %in% c("02","03","04") & !is.na(timearrival) &
           !GE %in% c(
             "0217","0218","0219","0220","0221","0222","0223","0224","0225","0226","0227","0228","0229",
             "0216","0215","0214","0213","0212","0211","0210","0209","0208","0207","0206","0205","0204","0203","0202","0201")) %>% #NOME_REGIONE %in% c("Abruzzo","Lazio")
  group_by(high_susp,tau0311) %>% summarise(ma=mean(ma,na.rm=TRUE),death=mean(death,na.rm=TRUE)) %>%
  ggplot( aes(x= tau0311, y=death, group=high_susp, color=high_susp)) + 
  geom_line(linetype = "dashed") + 
  geom_line(aes(y=ma,color=high_susp),size=2)+
  #stat_smooth(method = "lm", formula = y ~ poly(x, 6),level = 1-1e-1) +#aes(x = seq(length(GE)),y=death, group=factor(high_susp)), se = F, method = "lm", formula = y ~ poly(x, 8)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) + 
  scale_x_continuous(breaks = seq(-50, 50, by = 2))+  
  theme(legend.position="bottom") +
  geom_vline(aes(xintercept = 0))  + 
  #geom_rect(aes(xmin= 10, xmax= 24), ymin=-Inf, ymax=Inf, fill=.1) +
  ggtitle("Mortality rates in high and low shutdown exposure municipalities") +
  xlab("Days relative to first treatment date (March 21st, 2020)") + ylab("Excess death rate, daily average") +
  labs(colour = "HighShutdown")
#geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0311'))) #+ #1t lockdown
#geom_rect(aes(xmin= which(levels(as.factor(GE)) == '0321'), xmax= which(levels(as.factor(GE)) == '0331'), ymin=-Inf, ymax=Inf), fill=.1)
#length(listcomune)

summary(dd_res_lm)
describe(correl$shut11)
sd(correl$shut11)

# politics ----------------------------------------------------------------
#https://elezionistorico.interno.gov.it/index.php?tpel=R&dtel=31/05/2015&tpa=I&tpe=R&lev0=0&levsut0=0&lev1=10&levsut1=1&ne1=10&es0=S&es1=S&ms=S
#and a little bit of wikipedia
region<-c("Emilia-Romagna","Abruzzo","Basilicata","Calabria","Campania","Lazio","Liguria","Lombardia",
          "Marche","Molise","Piemonte","Puglia","Sardegna","Sicilia","Toscana","Umbria","Veneto","Friuli-Venezia Giulia")
mmddyy<-c("012620","021019","032419","012620","053115","030418","053115","030418",
          "053115","042218","052619","053115","022419","051117","053115","102719","053115","042918")
destra<-c(0,1,1,1,0,0,1,1,0,1,1,0,1,1,0,1,1,1)
elect_reg<-data.frame(cbind(region,mmddyy,destra))

dailydeaths<-left_join(dailydeaths,elect_reg,by=c("NOME_REGIONE"="region"))


# Hospitals ------------------------------------------------------------
#http://www.dati.salute.gov.it/dati/dettaglioDataset.jsp?menu=dati&idPag=18
hospitals<-read.csv("https://www.dropbox.com/s/vl96il6cwlak7h6/C_17_dataset_18_0_upFile.csv?dl=1",sep = ";")
hospitals<-hospitals[hospitals$Anno=="2018",]
hospitals$Codice.Comune<-as.character(hospitals$Codice.Comune)
hospitals$Codice.Comune<-ifelse(nchar(hospitals$Codice.Comune)<5,paste0("0",hospitals$Codice.Comune),hospitals$Codice.Comune)
hospitals$Codice.Comune<-ifelse(nchar(hospitals$Codice.Comune)<6,paste0("0",hospitals$Codice.Comune),hospitals$Codice.Comune)
hospitals$Totale.posti.letto<-as.numeric(hospitals$Totale.posti.letto)
# casacura<- hospitals[hospitals$Codice.tipo.struttura==5.1,] %>% 
#   group_by(Codice.Comune)  %>%
#   summarise(casacura=n(),totpostiletto=sum(Totale.posti.letto)) 

ospedali<-hospitals[!(hospitals$Codice.tipo.struttura) %in% c(9,5.1),] %>% #exclude research centers and casacura;
  group_by(Codice.Comune)  %>%
  summarise(ospedali=n(),totpostiletto=sum(Totale.posti.letto))

ospedali$province<-substr(ospedali$Codice.Comune,1,3)
pop_comune$province<-substr(pop_comune$comune,1,3)
pop_comune<- pop_comune %>% group_by(province) %>% mutate(pop_prov=sum(pop_18,na.rm=TRUE))
ospedali<-left_join(ospedali,pop_comune[colnames(pop_comune) %in% c("province","pop_prov")],by=c("province")) %>% unique()
ospedali <- ospedali %>% group_by(province) %>% summarise(capacity_p=sum(totpostiletto)/mean(pop_prov))

# Tourism -----------------------------------------------------------------
tourism<-read.xlsx("https://www.dropbox.com/s/qvew1xjqaexn6z5/turism.xlsx?dl=1")
geog_codes$province<-substr(geog_codes$COD_PROVCOM,1,3)
tourism<-left_join(tourism,geog_codes[colnames(geog_codes) %in% c("province","NOME_PROVINCIA")],by=c("provincia"="NOME_PROVINCIA")) %>% unique()
tourism<-left_join(tourism,pop_comune[colnames(pop_comune) %in% c("pop_prov","province")],by="province") %>% unique()

tourism$tou_for<-tourism$tou_for/tourism$pop_prov*100
tourism$tou_ita<-tourism$tou_ita/tourism$pop_prov*100

# Regression --------------------------------------------------------------
correl$totempl<-(correl$empl_susp.x+correl$empl_act.x)/(correl$pop_18)
correl<-correl %>% filter(pop_18>2500) %>% group_by(province) %>% mutate(strong11=quantile(shut11, probs=0.5, na.rm=TRUE),
                                                                         qtle1=quantile(shut11,probs=1/5, na.rm=TRUE),qtle2=quantile(shut11,probs=2/5, na.rm=TRUE),
                                                                         qtle3=quantile(shut11,probs=3/5, na.rm=TRUE),qtle4=quantile(shut11,probs=4/5, na.rm=TRUE))
correl$t1<-ifelse(correl$shut11<correl$qtle1,1,0)
correl$t2<-ifelse(correl$shut11>=correl$qtle1 & correl$shut11<correl$qtle2,1,0)
correl$t3<-ifelse(correl$shut11>=correl$qtle2 & correl$shut11<correl$qtle3,1,0)
correl$t4<-ifelse(correl$shut11>=correl$qtle3 & correl$shut11<correl$qtle4,1,0)
correl$t5<-ifelse(correl$shut11>=correl$qtle4,1,0)
correl$strong11<-ifelse(correl$shut11>correl$strong11,1,0)
df<-inner_join(correl[colnames(correl) %in% c("codice_comune","shut11","shut25","vapop_m","susp_act_empl","strong11","ret11","food11","pers11","open",
                                              "t1","t2","t3","t4","t5","labint","totempl")], 
               fulldaily_scaled[!fulldaily_scaled$COD_PROVCOM %in% nohit$COD_PROVCOM,names(fulldaily_scaled) %in% c("COD_PROVCOM","GE","month","pop_18","density","timearrival",
                                                                                                                    "excD","NOME_REGIONE","NOME_PROVINCIA","dip_anziani","cumulT_20_c_pop","tau0311",
                                                                                                                    "monday","tuesday","wednesday","thursday","friday","saturday")],
               by=c("codice_comune"="COD_PROVCOM"))
df<-left_join(df,weekarrival[colnames(weekarrival) %in% c("COD_PROVCOM","weekarrival")],by=c("codice_comune"="COD_PROVCOM"))
df<-df[!(df$month) %in% c("01"),] #04
df$dpost1103<-ifelse(df$GE %in% c("0321","0322","0323","0324","0325","0326","0327","0328","0329","0330","0331","0401","0402","0403","0404",
                                  "0405","0406","0407","0408","0409","0410","0411","0412","0413","0414","0415"),1,0)
df$dpost1103<-ifelse(df$GE %in% c(#"0222","0223","0224","0225","0226","0227","0228","0229",
  "0221","0220","0219","0218","0217","0216","0215","0214","0213","0212","0211","0210","0209","0208",
  "0207","0206","0205","0204","0203","0202","0201"),NA,df$dpost1103)
df$dpost2503<-ifelse(df$GE %in% c("0405","0406","0407","0408","0409","0410","0411","0412","0413","0414","0415"),1,0)
df$dpost2503<-ifelse(df$GE %in% c("0221","0220","0219","0218","0217","0216","0215","0214","0213","0212","0211","0210","0209","0208",
                                  "0207","0206","0205","0204","0203","0202","0201"),NA,df$dpost2503)

df$place11<-ifelse(df$GE %in% c("0301","0302","0303","0304","0305","0306","0307","0308","0309","0310",
                                "0311","0312","0313","0314","0315"),1,0)
df$place11<-ifelse(df$GE %in% c("0316","0317","0318","0319","0320",
                                "0321","0322","0323","0324","0325","0326","0327","0328","0329","0330",
                                "0331","0401","0402","0403","0404","0405","0406","0407","0408","0409",
                                "0410","0411","0412","0413","0414","0415",
                                "0210","0211","0212","0213","0214","0215",
                                "0209","0208","0207","0206","0205","0204","0203","0202","0201"),NA,df$place11)

df$place25<-ifelse(df$GE %in% c("0310","0311","0312","0313","0314","0315"),1,0)
df$place25<-ifelse(df$GE %in% c("0316","0317","0318","0319","0320","0321","0322","0323","0324","0325",
                                "0326","0327","0328","0329","0330","0331","0401","0402","0403","0404",
                                "0405","0406","0407","0408","0409","0410","0411","0412","0413","0414","0415",
                                "0210","0211","0212","0213","0214","0215",
                                "0209","0208","0207","0206","0205","0204","0203","0202","0201"),NA,df$place25)

df$province<-substr(df$codice_comune,1,3)
df<-left_join(df,ospedali,by="province")
df<-left_join(df,paralleltrend2[colnames(paralleltrend2) %in% c("COD_PROVCOM","high_susp")],by=c("codice_comune"="COD_PROVCOM"))
df<-left_join(df,tourism[colnames(tourism) %in% c("provincia","tou_for","tou_ita")],by=c("NOME_PROVINCIA"="provincia"))

# # df<-df %>% filter(!is.na(timearrival)) %>% group_by(codice_comune) %>% mutate(nonna_t=mean(row_number())) %>% arrange(nonna_t)
# capoluogo <- pop_comune %>% group_by(province) %>% mutate(maxpop_p=max(pop_18,na.rm=TRUE)) %>% select(province,comune,maxpop_p,pop_18) %>% unique()
# capoluogo$capo<-ifelse(capoluogo$pop_18/capoluogo$maxpop_p>=1,1,0)
# capoluogo <- capoluogo %>% filter(pop_18>2500)
# capoluogo<-left_join(capoluogo,correl[colnames(correl) %in% c("codice_comune","shut11","shut25")],
#                      by=c("comune"="codice_comune"))
# capo1<-capoluogo %>% filter(capo==1)
# capoluogo<-capoluogo %>% filter(capo==0)
# capo2<- capoluogo %>% group_by(province) %>% summarise(shut11_avgp=mean(shut11,na.rm=TRUE),shut25_avgp=mean(shut25,na.rm=TRUE))
# capo1<-left_join(capo1,capo2,by="province")
# df<-left_join(df,capo1[colnames(capo1) %in% c("comune","shut11_avgp","shut25_avgp")],by=c("codice_comune"="comune")) #inner (out->in) spillovers
# names(capo1)[names(capo1) == "shut11"] <- "shut11_p"
# names(capo1)[names(capo1) == "shut25"] <- "shut25_p"
# df<-left_join(df,capo1[colnames(capo1) %in% c("province","shut11_p","shut25_p")],by="province") #outer (in-> out) spillovers
# df$shut11_p<-ifelse(is.na(df$shut11_avgp),df$shut11_p,NA)
# df$shut25_p<-ifelse(is.na(df$shut25_avgp),df$shut25_p,NA)

largshut<- correl %>% group_by(province) %>% filter(pop_18>15000) %>% mutate(maxshut11=max(shut11,na.rm=TRUE),maxshut25=max(shut25,na.rm=TRUE)) %>%
  select(codice_comune,province,maxshut11,shut11,maxshut25,shut25)
largshut$big11<-ifelse(largshut$shut11/largshut$maxshut11>=1 & largshut$shut11/largshut$maxshut11<=1,1,0)
largshut$big25<-ifelse(largshut$shut25/largshut$maxshut25>=1 & largshut$shut25/largshut$maxshut25<=1,1,0)
capo1<-largshut %>% filter(big11==1)
capo1b<-largshut %>% filter(big25==1)
largshut1<-largshut %>% filter(big11==0)
largshut1b<-largshut %>% filter(big25==0)
capo2<- largshut %>% group_by(province) %>% summarise(shut11_avgp=mean(shut11,na.rm=TRUE))
capo2b<- largshut %>% group_by(province) %>% summarise(shut25_avgp=mean(shut25,na.rm=TRUE))
capo1<-left_join(capo1,capo2,by="province")
capo1b<-left_join(capo1b,capo2b,by="province")
df<-left_join(df,capo1[colnames(capo1) %in% c("codice_comune","shut11_avgp")],by="codice_comune") #inner (out->in) spillovers
df<-left_join(df,capo1b[colnames(capo1b) %in% c("codice_comune","shut25_avgp")],by="codice_comune") #inner (out->in) spillovers
names(capo1)[names(capo1) == "shut11"] <- "shut11_p"
names(capo1b)[names(capo1b) == "shut25"] <- "shut25_p"
df<-left_join(df,capo1[colnames(capo1) %in% c("province","shut11_p")],by="province") #outer (in-> out) spillovers
df<-left_join(df,capo1b[colnames(capo1b) %in% c("province","shut25_p")],by="province") #outer (in-> out) spillovers
df$shut11_p<-ifelse(is.na(df$shut11_avgp),df$shut11_p,NA)
df$shut25_p<-ifelse(is.na(df$shut25_avgp),df$shut25_p,NA)


#write.dta(data.frame(df,stsringsAsFactors = FALSE), "/data/R_projects/website/Website/Covid19.dta")

effect <- plm(excD ~ lag(excD,1), 
              data = df,
              index = "codice_comune", #c("region", "area"), 
              model = "within")

# print summary using robust standard errors
coeftest(effect, vcov. = vcovHC, type = "HC1")
nobs(effect)

# map ---------------------------------------------------------------------
install.packages("ggalt")
install.packages("ggthemes")
library(maptools)
library(ggalt)
library(ggthemes)
library(tibble)
library(viridis)

# get italy region map
italy_map <- map_data("italy")

# your data will need to have these region names
print(unique(italy_map$region))
geographics<-dailydeaths[colnames(dailydeaths) %in% c("NOME_PROVINCIA","NOME_REGIONE","COD_PROVCOM")]
totbus_p<-left_join(totbus,geographics,by=c("codice_comune"="COD_PROVCOM"))
totbus_p$region<-totbus_p$NOME_PROVINCIA
totbus_p<-totbus_p %>% group_by(region) %>% summarise(mean_susp_act_empl=mean(susp_act_empl,na.rm=TRUE))
gg <- ggplot()
gg <- gg + geom_map(aes(map_id=region), data=italy_map, map=italy_map) +
  #color="#b2b2b2", size=0.1, fill=NA, 
  expand_limits(x = italy_map$long, y = italy_map$lat)
gg <- gg + geom_map(data=totbus_p, map=italy_map,
                    aes(fill=mean_susp_act_empl, map_id=region),
                    color="#b2b2b2", size=0.1) + 
  scale_fill_viridis_c(trans = "sqrt", alpha = .7)  + 
  theme(legend.title = element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),axis.ticks.y=element_blank()) #legend.position="right"
gg

#save(totbus_p,file="/data/R_projects/website/website/Covid19.RData")


# Death#######
death_male<-read.xlsx("https://www.dropbox.com/s/wq88vrrudorj60a/synthetic-table.xlsx?dl=1",sheet="Male 65 and older by Age Group")
death_female<-read.xlsx("https://www.dropbox.com/s/wq88vrrudorj60a/synthetic-table.xlsx?dl=1",sheet="Female65 and older by Age Group")

names(death_male)<-death_male[1,]
death_male<-death_male[-1,]
names(death_male)[3]="region"
names(death_male)[4]="city" #province
names(death_male)[5]="municipality" #comune

names(death_male)[8]<-"young_19" #65-74
names(death_male)[9]<-"medium_19" #75-84
names(death_male)[10]<-"old_19" #85+
names(death_male)[11]<-"young_20" #65-74
names(death_male)[12]<-"medium_20" #75-84
names(death_male)[13]<-"old_20" #85+
death_male$death_19<-as.numeric(death_male$young_19)+as.numeric(death_male$medium_19)+as.numeric(death_male$old_19) #65+ men deaths in 2019
death_male$death_20<-as.numeric(death_male$young_20)+as.numeric(death_male$medium_20)+as.numeric(death_male$old_20) #65+ men deaths in 2020

names(death_female)<-death_female[1,]
death_female<-death_female[-1,]
names(death_female)[3]="region"
names(death_female)[4]="city"
names(death_female)[5]="municipality" #comune

names(death_female)[8]<-"young_19" #65-74
names(death_female)[9]<-"medium_19" #75-84
names(death_female)[10]<-"old_19" #85+
names(death_female)[11]<-"young_20" #65-74
names(death_female)[12]<-"medium_20" #75-84
names(death_female)[13]<-"old_20" #85+
death_female$death_19<-as.numeric(death_female$young_19)+as.numeric(death_female$medium_19)+as.numeric(death_female$old_19) #65+ women deaths in 2019
death_female$death_20<-as.numeric(death_female$young_20)+as.numeric(death_female$medium_20)+as.numeric(death_female$old_20) #65+ women deaths in 2020


death_tot<-inner_join(death_male[colnames(death_male) %in% c("region","CODES_NUTS3_LAU2 ","death_19","death_20")],
                      death_female[colnames(death_female) %in% c("CODES_NUTS3_LAU2 ","death_19","death_20")],by=c("CODES_NUTS3_LAU2 ")) #103 provinces, 20 regions;
death_tot$death_19<-death_tot$death_19.x+death_tot$death_19.y
death_tot$death_20<-death_tot$death_20.x+death_tot$death_20.y
death_tot<-death_tot[colnames(death_tot) %in% c("region","CODES_NUTS3_LAU2 ","death_19","death_20")]

death_tot$delta<-death_tot$death_20-death_tot$death_19

remove(death_female,death_male)

# Google mobility ---------------------------------------------------------
mobility<-read.csv("https://www.dropbox.com/s/q8n11641eok0z17/Global_Mobility_Report.csv?dl=1")
mobility<-mobility[mobility$country_region=="Italy" & mobility$sub_region_1!="",]

regions<-dip_anziani[colnames(dip_anziani) %in% c("regione","provincia")]
mobility  %>% ggplot( aes(x= date, y=workplaces_percent_change_from_baseline,group=sub_region_1, color=sub_region_1)) + geom_line()+ 
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) +  
  theme(legend.position="bottom") 
stat_smooth(method = "lm", formula = y ~ poly(x, 8),level = 1-1e-15) +#aes(x = seq(length(GE)),y=death, group=factor(high_susp)), se = F, method = "lm", formula = y ~ poly(x, 8)) +
  
  +
  geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0221'))) + #paziente-zero
  geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0311'))) + #1t lockdown
  geom_rect(aes(xmin= which(levels(as.factor(GE)) == '0321'), xmax= which(levels(as.factor(GE)) == '0331'), ymin=-Inf, ymax=Inf), fill=.1) +
  geom_vline(aes(xintercept = which(levels(as.factor(GE)) == '0325'))) #2nd lockdown 


#```{r load Covid19, echo=FALSE}
#load("/data/R_projects/website/website/Covid19.RData")
#hist(df$delta,breaks=1000,main=paste("Number of deaths among 65+ people"), xlab="65+ excess deaths")
#```

# ### Table 1: Province FE
# |  | All | Elderly | weighted-Eld | noLombardia | Placebo |
#   |--------------------------|--------------------------|--------------------------|--------------------------|--------------------------|--------------------------|
#   |  | $D_{n,t}$ | $EldD_{n,t}$ | $wEldD_{n,t}$ | $wEldD_{n,t}$ | $wEldD_{n,t}$ |
#   |--------------------------|--------------------------|--------------------------|--------------------------|--------------------------|--------------------------|
#   | $dpost_t$ | $\underset{(.274)}{1.291^{***}}$ | $\underset{(.267)}{1.230^{***}}$ | $\underset{(.104)}{.473^{***}}$ | $\underset{(.118)}{.370^{***}}$ | $\underset{(.0201)}{.044^{**}}$ |
#   | $suspended_n$ | $\underset{(.353)}{.339}$ | $\underset{(.335)}{.0249}$ | $\underset{(.122)}{.0253}$ | $\underset{(.098)}{-.145}$ | $\underset{(.106)}{-.249^{**}}$ |
#   | $susp_n \times dpost_t$ | $\underset{(.457)}{-1.45^{***}}$ | $\underset{(.447)}{-1.385^{***}}$ | $\underset{(.174)}{-.548^{***}}$ | $\underset{(.205)}{-.481^{**}}$ | $\underset{(.036)}{-.0235}$ |
#   | $logPop_n$ | $\underset{(.377)}{1.515^{***}}$ | $\underset{(.350)}{1.398^{***}}$ | $\underset{(.131)}{.533^{***}}$ | $\underset{(.103)}{.447^{**}}$ | $\underset{(.076)}{.337^{***}}$ |
#   | $logDens_n$ | $\underset{(.161)}{-.390^{**}}$ | $\underset{(.149)}{-.361^{**}}$ | $\underset{(.055)}{-.137^{**}}$ | $\underset{(.036)}{-.0973^{**}}$ | $\underset{(.026)}{-.0704^{***}}$ |
#   | Const | $\underset{(2.62)}{-10.75^{***}}$ | $\underset{(2.44)}{-9.91^{***}}$ | $\underset{(.914)}{-3.80^{***}}$ | $\underset{(.747)}{-3.21^{***}}$ | $\underset{(.052)}{-2.338^{***}}$ |
#   |----------------|------------------------|-----------------------|---------------------|--------------------------|--------------------------|
#   | $Province FE$ | $\checkmark$ | $\checkmark$ | $\checkmark$ | $\checkmark$ | $\checkmark$ |
#   | $N$ | $42,790$ | $42,790$ | $42,220$ | $26,550$ | $25,430$ |
#   | $R^2$ | $.25$ | $.24$ | $.25$ | $.29$ | $.29$ |
