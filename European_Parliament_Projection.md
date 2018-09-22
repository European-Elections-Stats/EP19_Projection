---
title: "European Parliament Seat Projection"
output:
  html_document:
    keep_md: yes
---



# European Parliament Seat Projection
The following code produces a seat projection for the European Parliament, if it were elected today. The code scrapes national poll data for all 27 member states, calculates the seat distribution for the respective national parties and aggregates the data for an EU wide seat projection. The final code chunk uploads the data to google sheets.  
The data produced by the code are visualised with Tableau and interpreted on [http://europeanelectionsstats.eu/](http://europeanelectionsstats.eu/)  
Some more details on the method can be found here: [https://europeanelectionsstats.eu/method/](https://europeanelectionsstats.eu/method/)  
If you find errors or have questions/suggestions please submit them on github or contact contact@europeanelectionsstats.eu


## Install and load the necessary packages

```r
#installing packages

if(!is.element('electoral', installed.packages()[,1]))
{install.packages('electoral')
}else {print("electoral library already installed")}

if(!is.element('plyr', installed.packages()[,1]))
{install.packages('plyr')
}else {print("plyr library already installed")}
if(!is.element('dplyr', installed.packages()[,1]))
{install.packages('dplyr')
}else {print("dplyr library already installed")}

if(!is.element('rvest', installed.packages()[,1]))
{install.packages('rvest')
}else {print("rvest library already installed")}
if(!is.element('magrittr', installed.packages()[,1]))
{install.packages('magrittr')
}else {print("magrittr library already installed")}

#activating packages 
library("electoral")
library("plyr")
library("dplyr")
library("rvest")
library("magrittr")
```


## Code for every member state

```r
#### code for each MS: scrape, clean, calculate, create national data frames --------------------------------

#GERMANY
de.csv<-read.csv(url("https://pollofpolls.eu/get/polls/DE-parliament/format/csv"))
afdpolls<-tail(de.csv$afd,3)
afdmean<-mean(afdpolls,na.rm=TRUE)
fdppolls<-tail(de.csv$fdp,3)
fdpmean<-mean(fdppolls,na.rm=TRUE)
gruenepolls<-tail(de.csv$gruene,3)
gruenemean<-mean(gruenepolls,na.rm=TRUE)
linkepolls<-tail(de.csv$linke,3)
linkemean<-mean(linkepolls,na.rm=TRUE)
spdpolls<-tail(de.csv$spd,3)
spdmean<-mean(spdpolls,na.rm=TRUE)
unionpolls<-tail(de.csv$union,3)
unionmean<-mean(unionpolls,na.rm=TRUE)

LAST_POLLS<-c(afdmean,fdpmean,gruenemean,linkemean,spdmean,unionmean,0,0,0,0,0,0,0,0)
PARTIES<-c("AFD (Germany)","FDP (Germany)","GRUENE (Germany)","LINKE (Germany)","SPD (Germany)","UNION (Germany)","N.A1","N.A2","N.A3","N.A4","N.A5","N.A6","N.A7","N.A8")
GROUPS<-c("ni.far right","ALDE","GREENS","GUE-NGL","S&D","EPP","ECR","EFDD","ni.far left","new.far left","new.moderate","new.far right","ni.moderate","ENF")
DE<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(DE$PARTIES),votes=c(DE$LAST_POLLS),n_seats=96,method="dhondt")
DE.parties=DE$PARTIES
EPgroups=DE$GROUP
GERMANY<-data.frame(EPgroups,DE.parties,seats)

GERMANY1<-aggregate(seats~EPgroups,data=GERMANY,sum)

#AUSTRIA
au.csv<-read.csv(url("https://pollofpolls.eu/get/polls/AT-parliament/format/csv"))
fpoepolls<-tail(au.csv$fpoe,3)
fpoemean<-mean(fpoepolls,na.rm=TRUE)
augruene<-tail(au.csv$gruene,3)
augruenemean<-mean(augruene,na.rm=TRUE)
neospolls<-tail(au.csv$neos,3)
neosmean<-mean(neospolls,na.rm=TRUE)
oevppolls<-tail(au.csv$oevp,3)
oevpmean<-mean(oevppolls,na.rm=TRUE)
spoepolls<-tail(au.csv$spoe,3)
spoemean<-mean(spoepolls,na.rm=TRUE)
pilzpolls<-tail(au.csv$pilz,3)
pilzmean<-mean(pilzpolls,na.rm=TRUE)

LAST_POLLS<-c(fpoemean,augruenemean,neosmean,oevpmean,spoemean,pilzmean,0,0,0,0,0,0,0,0)
PARTIES<-c("FPOE (Austria)","GRUENE (Austria)","NEOS (Austria)","OEVP (Austria)","SPOE (Austria)","PILZ (Austria)","NA1","NA2","NA3","NA4","NA5","NA6","NA7","NA8")
GROUPS<-c("ENF","GREENS","ALDE","EPP","S&D","new.far left","GUE-NGL","ECR","EFDD","ni.far left","ni.moderate","new.moderate","new.far right","ni.far right")
AU<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(AU$PARTIES),votes=c(AU$LAST_POLLS),n_seats=19,method="dhondt")
AU.parties=AU$PARTIES
EPgroups=AU$GROUP
AUSTRIA<-data.frame(EPgroups,AU.parties,seats)

AUSTRIA1<-aggregate(seats~EPgroups,data=AUSTRIA,sum)

#BELGIUM
be.csv<-read.csv(url("https://pollofpolls.eu/get/polls/BE-parliament/format/csv"))
cdhpolls<-tail(be.csv$cdh,3)
cdhmean<-mean(cdhpolls,na.rm=TRUE)
cdvpolls<-tail(be.csv$cdv,3)
cdvmean<-mean(cdvpolls,na.rm=TRUE)
defipolls<-tail(be.csv$defi,3)
defimean<-mean(defipolls,na.rm=TRUE)
ecolopolls<-tail(be.csv$ecolo,3)
ecolomean<-mean(ecolopolls,na.rm=TRUE)
groenpolls<-tail(be.csv$groen,3)
groenmean<-mean(groenpolls,na.rm=TRUE)
mrpolls<-tail(be.csv$mr,3)
mrmean<-mean(mrpolls,na.rm=TRUE)
nvapolls<-tail(be.csv$nva,3)
nvamean<-mean(nvapolls,na.rm=TRUE)
openvldpolls<-tail(be.csv$openvld,3)
openvldmean<-mean(openvldpolls,na.rm=TRUE)
pspolls<-tail(be.csv$ps,3)
psmean<-mean(pspolls,na.rm=TRUE)
pvdaptbpolls<-tail(be.csv$pvdaptb,3)
pvdaptbmean<-mean(pvdaptbpolls,na.rm=TRUE)
spapolls<-tail(be.csv$spa,3)
spamean<-mean(spapolls,na.rm=TRUE)
velaamspolls<-tail(be.csv$vlaamsbelang,3)
velaamsmean<-mean(velaamspolls,na.rm=TRUE)

LAST_POLLS<-c(cdhmean,cdvmean,defimean,ecolomean,groenmean,mrmean,nvamean,openvldmean,psmean,pvdaptbmean,spamean,velaamsmean,0,0,0,0,0,0)
PARTIES<-c("cdH (Belgium)","CDV (Belgium)","DEFI (Belgium)","ECOLO (Belgium)","GROEN (Belgium)","MR (Belgium)","NVA (Belgium)","OPENVLD (Belgium)","PS (Belgium)","PVDAPTB (Belgium)","SPA (Belgium)","VELAAMSBELANG (Belgium)","NA1","NA2","NA3","NA4","NA5","NA6")
GROUPS<-c("EPP","EPP","new.moderate","GREENS","GREENS","ALDE","ECR","ALDE","S&D","GUE-NGL","S&D","ENF","new.far right","new.far left","ni.moderate","ni.far left","EFDD","ni.far right")
BE<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(BE$PARTIES),votes=c(BE$LAST_POLLS),n_seats=21,method="dhondt")
BE.parties=BE$PARTIES
EPgroups=BE$GROUP
BELGIUM<-data.frame(EPgroups,BE.parties,seats)

BELGIUM1<-aggregate(seats~EPgroups,data=BELGIUM,sum)

#BULGARIA
bu.csv<-read.csv(url("https://pollofpolls.eu/get/polls/BG-parliament/format/csv"))
bsppolls<-tail(bu.csv$bsp,3)
bspmean<-mean(bsppolls,na.rm=TRUE)
dpspolls<-tail(bu.csv$dps,3)
dpsmean<-mean(dpspolls,na.rm=TRUE)
gerbpolls<-tail(bu.csv$gerb,3)
gerbmean<-mean(gerbpolls,na.rm=TRUE)
rbpolls<-tail(bu.csv$rb,3)
rbmean<-mean(rbpolls,na.rm=TRUE)
abvpolls<-tail(bu.csv$abv,3)
abvmean<-mean(abvpolls,na.rm=TRUE)
patriotspolls<-tail(bu.csv$patriots,3)
patriotsmean<-mean(patriotspolls,na.rm=TRUE)
volyapolls<-tail(bu.csv$volya,3)
volyamean<-mean(volyapolls,na.rm=TRUE)
yesbgpolls<-tail(bu.csv$yesbg,3)
yesbgmean<-mean(yesbgpolls,na.rm=TRUE)
bmpopolls<-tail(bu.csv$bmpo,3)
bmpomean<-mean(bmpopolls,na.rm=TRUE)

LAST_POLLS<-c(bspmean,dpsmean,gerbmean,rbmean,abvmean,patriotsmean,volyamean,yesbgmean,bmpomean,0,0,0,0,0,0,0,0)
PARTIES<-c("BSP (Bulgaria)","DPS (Bulgaria)","GERB (Bulgaria)","RB (Bulgaria)","ABV (Bulgaria)","PATRIOTS (Bulgaria)","VOLYA (Bulgaria)","YESBG (Bulgaria)","BMPO (Bulgaria)","NA4 (Bulgaria)","NA5 (Bulgaria)","NA6","NA7","NA8","NA9","NA10","NA11")
GROUPS<-c("S&D","ALDE","EPP","EPP","S&D","ECR","new.far right","GREENS","ECR","EFDD","GUE-NGL","ni.moderate","ni.far left","ni.far right","new.moderate","new.far left","ENF")
BU<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(BU$PARTIES),votes=c(BU$LAST_POLLS),n_seats=17,method="dhondt")
BU.parties=BU$PARTIES
EPgroups=BU$GROUP
BULGARIA<-data.frame(EPgroups,BU.parties,seats)

BULGARIA1<-aggregate(seats~EPgroups,data=BULGARIA,sum)

#CROATIA

hr.csv<-read.csv(url("https://pollofpolls.eu/get/polls/HR-parliament/format/csv"))
bm365polls<-tail(hr.csv$bm365,3)
bm365mean<-mean(bm365polls,na.rm=TRUE)
hdzpolls<-tail(hr.csv$hdz,3)
hdzmean<-mean(hdzpolls,na.rm=TRUE)
sdphrpolls<-tail(hr.csv$sdp,3)
sdphrmean<-mean(sdphrpolls,na.rm=TRUE)
zivizidpolls<-tail(hr.csv$zivizid,3)
zivizidmean<-mean(zivizidpolls, na.rm=TRUE)
mostpolls<-tail(hr.csv$most,3)
mostmean<-mean(mostpolls,na.rm=TRUE)
hnspolls<-tail(hr.csv$hns,3)
hnsmean<-mean(hnspolls,na.rm=TRUE)
hsspolls<-tail(hr.csv$hss,3)
hssmean<-mean(hsspolls,na.rm=TRUE)
idspolls<-tail(hr.csv$ids,3)
idsmean<-mean(idspolls,na.rm=TRUE)
ppolls<-tail(hr.csv$p,3)
pmean<-mean(ppolls,na.rm=TRUE)

LAST_POLLS<-c(bm365mean,hdzmean,sdphrmean,zivizidmean,mostmean,hnsmean,hssmean,idsmean,pmean,0,0,0,0,0,0,0,0,0)
PARTIES<-c("BM365 (Croatia)","HDZ (Croatia)","SDP (Croatia)","ZIVIZID (Croatia)","MOST (Croatia)","HNS (Croatia)","HSS (Croatia)","IDS (Croatia)","P (Croatia)","NA4","NA5","NA6","NA7","NA8","NA9","NA10","NA11","NA12")
GROUPS<-c("new.moderate","EPP","S&D","new.far left","new.moderate","ALDE","EPP","ALDE","ALDE","EFDD","GUE-NGL","ni.moderate","ni.far left","ni.far right","new.far right","ECR","GREENS","ENF")
HR<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(HR$PARTIES),votes=c(HR$LAST_POLLS),n_seats=12,method="dhondt")
HR.parties=HR$PARTIES
EPgroups=HR$GROUP
CROATIA<-data.frame(EPgroups,HR.parties,seats)

CROATIA1<-aggregate(seats~EPgroups,data=CROATIA,sum)

#CYPRUS

cy.csv<-read.csv(url("https://pollofpolls.eu/get/polls/CY-parliament/format/csv"))
akelpolls<-tail(cy.csv$AKEL,3)
akelmean<-mean(akelpolls,na.rm=TRUE)
dikopolls<-tail(cy.csv$DIKO,3)
dikomean<-mean(dikopolls,na.rm=TRUE)
disypolls<-tail(cy.csv$DISY,3)
disymean<-mean(disypolls,na.rm=TRUE)
edekpolls<-tail(cy.csv$EDEK,3)
edekmean<-mean(edekpolls,na.rm=TRUE)
elampolls<-tail(cy.csv$ELAM,3)
elammean<-mean(elampolls,na.rm=TRUE)
kapolls<-tail(cy.csv$KA,3)
kamean<-mean(kapolls,na.rm=TRUE)
kospolls<-tail(cy.csv$KOSP,3)
kospmean<-mean(kospolls,na.rm=TRUE)
sypolpolls<-tail(cy.csv$SYPOL,3)
sypolmean<-mean(sypolpolls,na.rm=TRUE)

LAST_POLLS<-c(akelmean,dikomean,disymean,edekmean,elammean,kamean,kospmean,sypolmean,0,0,0,0,0,0,0)
PARTIES<-c("AKEL (Cyprus)","DIKO (Cyprus)","DISY (Cyprus)","EDEK (Cyprus)","ELAM (Cyprus)","KA (Cyprus)","KOSP (Cyprus)","SYPOL (Cyprus)","NA4","NA5","NA6","NA7","NA8","NA9","NA10")
GROUPS<-c("GUE-NGL","S&D","EPP","S&D","new.far right","ECR","GREENS","ALDE","EFDD","ni.moderate","ni.far left","ni.far right","new.far left","new.moderate","ENF")
CY<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(CY$PARTIES),votes=c(CY$LAST_POLLS),n_seats=6,method="dhondt")
CY.parties=CY$PARTIES
EPgroups=CY$GROUP
CYPRUS<-data.frame(EPgroups,CY.parties,seats)

CYPRUS1<-aggregate(seats~EPgroups,data=CYPRUS,sum)

#CZECH REPUBLIC 

cz.csv<-read.csv(url("https://pollofpolls.eu/get/polls/CZ-parliament/format/csv"))
anopolls<-tail(cz.csv$ano,3)
anomean<-mean(anopolls,na.rm=TRUE)
cssdpolls<-tail(cz.csv$cssd,3)
cssdmean<-mean(cssdpolls,na.rm=TRUE)
kdupolls<-tail(cz.csv$kdu,3)
kdumean<-mean(kdupolls,na.rm=TRUE)
kscmpolls<-tail(cz.csv$kscm,3)
kscmmean<-mean(kscmpolls,na.rm=TRUE)
odspolls<-tail(cz.csv$ods,3)
odsmean<-mean(odspolls,na.rm=TRUE)
piratipolls<-tail(cz.csv$pirati,3)
piratimean<-mean(piratipolls,na.rm=TRUE)
top9polls<-tail(cz.csv$top9,3)
top9mean<-mean(top9polls,na.rm=TRUE)
szpolls<-tail(cz.csv$sz,3)
szmean<-mean(szpolls,na.rm=TRUE)
spdczpolls<-tail(cz.csv$spd,3)
spdczmean<-mean(spdczpolls,na.rm=TRUE)
stanpolls<-tail(cz.csv$czstan,3)
stanmean<-mean(stanpolls,na.rm=TRUE)

LAST_POLLS<-c(anomean,cssdmean,kdumean,kscmmean,odsmean,piratimean,top9mean,szmean,spdczmean,stanmean,0,0,0,0,0,0)
PARTIES<-c("ANO (Czech Rep)","CSSD (Czech Rep)","KDU (Czech Rep)","KSCM (Czech Rep)","ODS (Czech Rep)","PIRATI (Czech Rep)","TOP9 (Czech Rep)","SZ (Czech Rep)","SPD (Czech Rep)","STAN (Czech Rep)","NA6","NA7","NA8","NA9","NA10","NA11")
GROUPS<-c("ALDE","S&D","EPP","GUE-NGL","ECR","new.moderate","EPP","GREENS","new.far right","EPP","ni.far left","ni.far right","new.far left","ni.moderate","EFDD","ENF")
CZ<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(CZ$PARTIES),votes=c(CZ$LAST_POLLS),n_seats=21,method="dhondt")
CZ.parties=CZ$PARTIES
EPgroups=CZ$GROUP
CZECH<-data.frame(EPgroups,CZ.parties,seats)

CZECH1<-aggregate(seats~EPgroups,data=CZECH,sum)

#DENMARK

dk.csv<-read.csv(url("https://pollofpolls.eu/get/polls/DK-parliament/format/csv"))
Apolls<-tail(dk.csv$A,3)
Amean<-mean(Apolls,na.rm=TRUE)
Bpolls<-tail(dk.csv$B,3)
Bmean<-mean(Bpolls,na.rm=TRUE)
Cpolls<-tail(dk.csv$C,3)
Cmean<-mean(Cpolls,na.rm=TRUE)
Fpolls<-tail(dk.csv$F,3)
Fmean<-mean(Fpolls,na.rm=TRUE)
Ipolls<-tail(dk.csv$I,3)
Imean<-mean(Ipolls,na.rm=TRUE)
Opolls<-tail(dk.csv$O,3)
Omean<-mean(Opolls,na.rm=TRUE)
RGpolls<-tail(dk.csv$RG,3)
RGmean<-mean(RGpolls,na.rm=TRUE)
Vpolls<-tail(dk.csv$V,3)
Vmean<-mean(Vpolls,na.rm=TRUE)
Kpolls<-tail(dk.csv$K,3)
Kmean<-mean(Kpolls,na.rm=TRUE)
altpolls<-tail(dk.csv$Alt,3)
altmean<-mean(altpolls,na.rm=TRUE)

LAST_POLLS<-c(Amean,Bmean,Cmean,Fmean,Imean,Omean,RGmean,Vmean,Kmean,altmean,0,0,0,0,0,0,0)
PARTIES<-c("A (Denmark)","B (Denmark)","C (Denmark)","F (Denmark)","I (Denmark)","O (Denmark)","RG (Denmark)","V (Denmark)","K (Denmark)","ALT (Denmark)","NA6","NA7","NA8","NA9","NA10","NA11","NA12")
GROUPS<-c("S&D","ALDE","EPP","GREENS","ALDE","ECR","GUE-NGL","ALDE","EPP","new.moderate","ni.far left","ni.far right","new.far left","ni.moderate","new.far right","EFDD","ENF")
DK<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(DK$PARTIES),votes=c(DK$LAST_POLLS),n_seats=14,method="dhondt")
DK.parties=DK$PARTIES
EPgroups=DK$GROUP
DENMARK<-data.frame(EPgroups,DK.parties,seats)

DENMARK1<-aggregate(seats~EPgroups,data=DENMARK,sum)

#ESTONIA
ee.csv<-read.csv(url("https://pollofpolls.eu/get/polls/EE-parliament/format/csv"))
ekrepolls<-tail(ee.csv$EKRE,3)
ekremean<-mean(ekrepolls,na.rm=TRUE)
greenpolls<-tail(ee.csv$Green,3)
greenmean<-mean(greenpolls,na.rm=TRUE)
irlpolls<-tail(ee.csv$IRL,3)
irlmean<-mean(irlpolls,na.rm=TRUE)
keskpolls<-tail(ee.csv$Kesk,3)
keskmean<-mean(keskpolls,na.rm=TRUE)
refpolls<-tail(ee.csv$Ref,3)
refmean<-mean(refpolls,na.rm=TRUE)
sdepolls<-tail(ee.csv$SDE,3)
sdemean<-mean(sdepolls,na.rm=TRUE)
evapolls<-tail(ee.csv$EVA,3)
evamean<-mean(evapolls,na.rm=TRUE)

LAST_POLLS<-c(ekremean,greenmean,irlmean,keskmean,refmean,sdemean,evamean,0,0,0,0,0,0,0,0)
PARTIES<-c("EKRE (Estonia)","GREEN (Estonia)","IRL (Estonia)","KESK (Estonia)","Ref (Estonia)","SDE (Estonia)","EVA (Estonia)","NA1","NA2","NA3","NA6","NA7","NA8","NA9","NA10")
GROUPS<-c("new.far right","GREENS","EPP","ALDE","ALDE","S&D","new.moderate","GUE-NGL","ECR","EFDD","ni.far left","ni.far right","new.far left","ni.moderate","ENF")
EE<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(EE$PARTIES),votes=c(EE$LAST_POLLS),n_seats=7,method="dhondt")
EE.parties=EE$PARTIES
EPgroups=EE$GROUP
ESTONIA<-data.frame(EPgroups,EE.parties,seats)

ESTONIA1<-aggregate(seats~EPgroups,data=ESTONIA,sum)

#FINLAND

fn.csv<-read.csv(url("https://pollofpolls.eu/get/polls/FI-parliament/format/csv"))
kdpolls<-tail(fn.csv$KD,3)
kdmean<-mean(kdpolls,na.rm=TRUE)
keskfnpolls<-tail(fn.csv$KESK,3)
keskfnmean<-mean(keskfnpolls,na.rm=TRUE)
kokpolls<-tail(fn.csv$KOK,3)
kokmean<-mean(kokpolls,na.rm=TRUE)
psfnpolls<-tail(fn.csv$PS,3)
psfnmean<-mean(psfnpolls,na.rm=TRUE)
sdpfnpolls<-tail(fn.csv$SDP,3)
sdpfnmean<-mean(sdpfnpolls,na.rm=TRUE)
sfppolls<-tail(fn.csv$SFP,3)
sfpmean<-mean(sfppolls,na.rm=TRUE)
vaspolls<-tail(fn.csv$VAS,3)
vasmean<-mean(vaspolls,na.rm=TRUE)
vihrpolls<-tail(fn.csv$VIHR,3)
vihrmean<-mean(vihrpolls,na.rm=TRUE)
sinpolls<-tail(fn.csv$SIN,3)
sinmean<-mean(sinpolls,na.rm=TRUE)

LAST_POLLS<-c(kdmean,keskfnmean,kokmean,psfnmean,sdpfnmean,sfpmean,vasmean,vihrmean,sinmean,0,0,0,0,0,0,0,0)
PARTIES<-c("KD (Finland)","KESK (Finland)","KOK (Finland)","PS (Finland)","SDP (Finland)","SFP (Finland)","VAS (Finland)","VIHR (Finland)","SIN (Finland)","NA3","NA6","NA7","NA8","NA9","NA10","NA11","NA12")
GROUPS<-c("EPP","ALDE","EPP","ECR","S&D","ALDE","GUE-NGL","GREENS","ECR","EFDD","ni.far left","ni.far right","ni.moderate","new.far left","new.moderate","new.far right","ENF")
FN<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(FN$PARTIES),votes=c(FN$LAST_POLLS),n_seats=14,method="dhondt")
FN.parties=FN$PARTIES
EPgroups=FN$GROUP
FINLAND<-data.frame(EPgroups,FN.parties,seats)

FINLAND1<-aggregate(seats~EPgroups,data=FINLAND,sum)

#FRANCE
fr.csv<-read.csv(url("https://pollofpolls.eu/get/polls/FR-2019-EP/format/csv"))
dlfpolls<-tail(fr.csv$DLF,3)
dlfmean<-mean(dlfpolls,na.rm=TRUE)
eelvpolls<-tail(fr.csv$EELV,3)
eelvmean<-mean(eelvpolls,na.rm=TRUE)
fipolls<-tail(fr.csv$FI,3)
fimean<-mean(fipolls,na.rm=TRUE)
fnpolls<-tail(fr.csv$FN,3)
fnmean<-mean(fnpolls,na.rm=TRUE)
npapolls<-tail(fr.csv$NPA,3)
npamean<-mean(npapolls,na.rm=TRUE)
psfrpolls<-tail(fr.csv$PS,3)
psfrmean<-mean(psfrpolls,na.rm=TRUE)
rempolls<-tail(fr.csv$REM,3)
remmean<-mean(rempolls,na.rm=TRUE)
lrpolls<-tail(fr.csv$LR,3)
lrmean<-mean(lrpolls,na.rm=TRUE)

LAST_POLLS<-c(dlfmean,eelvmean,fimean,fnmean,npamean,psfrmean,remmean,lrmean,0,0,0,0,0,0,0)
PARTIES<-c("Debout la France (France)","Ecolo (France)","France Insoumise (France)","FN (France)","Parti anticapitalist (France)","PS (France)","En Marche (France)","Les Repub. (France)","NA2","NA3","NA6","NA7","NA8","NA9","NA10")
GROUPS<-c("EFDD","GREENS","GUE-NGL","ENF","GUE-NGL","S&D","new.moderate","EPP","ECR","ni.far left","ni.moderate","new.far left","new.far right","ALDE","ni.far right")
FR<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(FR$PARTIES),votes=c(FR$LAST_POLLS),n_seats=79,method="dhondt")
FR.parties=FR$PARTIES
EPgroups=FR$GROUP
FRANCE<-data.frame(EPgroups,FR.parties,seats)

FRANCE1<-aggregate(seats~EPgroups,data=FRANCE,sum)

#GREECE
gr.csv<-read.csv(url("https://pollofpolls.eu/get/polls/GR-parliament/format/csv"))
anelpolls<-tail(gr.csv$ANEL,3)
anelmean<-mean(anelpolls,na.rm=TRUE)
grxapolls<-tail(gr.csv$GRXA,3)
grxamean<-mean(grxapolls,na.rm=TRUE)
kkepolls<-tail(gr.csv$KKE,3)
kkemean<-mean(kkepolls,na.rm=TRUE)
ndpolls<-tail(gr.csv$ND,3)
ndmean<-mean(ndpolls,na.rm=TRUE)
syrizapolls<-tail(gr.csv$SYRIZA,3)
syrizamean<-mean(syrizapolls,na.rm=TRUE)
ekpolls<-tail(gr.csv$EK,3)
ekmean<-mean(ekpolls,na.rm=TRUE)
laepolls<-tail(gr.csv$LAE,3)
laemean<-mean(laepolls,na.rm=TRUE)
pepolls<-tail(gr.csv$PE,3)
pemean<-mean(pepolls,na.rm=TRUE)
kapolls<-tail(gr.csv$KA,3)
kamean<-mean(kapolls,na.rm=TRUE)

LAST_POLLS<-c(anelmean,grxamean,kkemean,ndmean,syrizamean,ekmean,laemean,pemean,kamean,0,0,0,0,0,0,0)
PARTIES<-c("ANEL (Greece)","GRXA (Greece)","KKE (Greece)","ND (Greece)","SYRIZA (Greece)","EK (Greece)","LAE (Greece)","PE (Greece)","KA (Greece)","NA3","NA6","NA7","NA8","NA9","NA10","NA11")
GROUPS<-c("ECR","ni.far right","ni.far left","EPP","GUE-NGL","ALDE","GUE-NGL","ni.far right","S&D","ni.moderate","new.far left","new.far right","new.moderate","EFDD","GREENS","ENF")
GR<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(GR$PARTIES),votes=c(GR$LAST_POLLS),n_seats=21,method="dhondt")
GR.parties=GR$PARTIES
EPgroups=GR$GROUP
GREECE<-data.frame(EPgroups,GR.parties,seats)

GREECE1<-aggregate(seats~EPgroups,data=GREECE,sum)

#HUNGARY
hu.csv<-read.csv(url("https://pollofpolls.eu/get/polls/HU-parliament/format/csv"))
dkpolls<-tail(hu.csv$DK,3)
dkmean<-mean(dkpolls,na.rm=TRUE)
fidespolls<-tail(hu.csv$FideszKDNP,3)
fidesmean<-mean(fidespolls,na.rm=TRUE)
jobbikpolls<-tail(hu.csv$Jobbik,3)
jobbikmean<-mean(jobbikpolls,na.rm=TRUE)
lmppolls<-tail(hu.csv$LMP,3)
lmpmean<-mean(lmppolls,na.rm=TRUE)
mszppolls<-tail(hu.csv$MSZP,3)
mszpmean<-mean(mszppolls,na.rm=TRUE)
mkkppolls<-tail(hu.csv$MKKP,3)
mkkpmean<-mean(mkkppolls,na.rm=TRUE)
mmpolls<-tail(hu.csv$MM,3)
mmmean<-mean(mmpolls,na.rm=TRUE)

LAST_POLLS<-c(dkmean,fidesmean,jobbikmean,lmpmean,mszpmean,mkkpmean,mmmean,0,0,0,0,0,0,0,0)
PARTIES<-c("DK (Hungary)","FideszKDNP (Hungary)","JOBBIK (Hungary)","LMP (Hungary)","MSZP (Hungary)","MKKP (Hungary)","MM (Hungary)","NA2","NA3","NA6","NA7","NA8","NA9","NA10","NA11")
GROUPS<-c("S&D","EPP","ni.far right","GREENS","S&D","ni.moderate","ALDE","GUE-NGL","ECR","EFDD","ni.far left","new.far left","new.moderate","new.far right","ENF")
HU<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(HU$PARTIES),votes=c(HU$LAST_POLLS),n_seats=21,method="dhondt")
HU.parties=HU$PARTIES
EPgroups=HU$GROUP
HUNGARY<-data.frame(EPgroups,HU.parties,seats)

HUNGARY1<-aggregate(seats~EPgroups,data=HUNGARY,sum)

#IRELAND
ir.csv<-read.csv(url("https://pollofpolls.eu/get/polls/IE-parliament/format/csv"))
ffpolls<-tail(ir.csv$FF,3)
ffmean<-mean(ffpolls,na.rm=TRUE)
fgpolls<-tail(ir.csv$FG,3)
fgmean<-mean(fgpolls,na.rm=TRUE)
gppolls<-tail(ir.csv$GP,3)
gpmean<-mean(gppolls,na.rm=TRUE)
labpolls<-tail(ir.csv$Lab,3)
labmean<-mean(labpolls,na.rm=TRUE)
ripolls<-tail(ir.csv$RI,3)
rimean<-mean(ripolls,na.rm=TRUE)
sdpolls<-tail(ir.csv$SD,3)
sdmean<-mean(sdpolls,na.rm=TRUE)
sfpolls<-tail(ir.csv$SF,3)
sfmean<-mean(sfpolls,na.rm=TRUE)
spbppolls<-tail(ir.csv$SPBP,3)
spbpmean<-mean(spbppolls,na.rm=TRUE)
iapolls<-tail(ir.csv$IA,3)
iamean<-mean(iapolls,na.rm=TRUE)

LAST_POLLS<-c(ffmean,fgmean,gpmean,labmean,rimean,sdmean,sfmean,spbpmean,iamean,0,0,0,0,0,0,0,0)
PARTIES<-c("Finna Fail (Ireland)","Fine Gael (Ireland)","Green Party (Ireland)","Labour (Ireland)","Renua Ireland (Ireland)","Social Democrats (Ireland)","Sinn Fein (Ireland)","S-PBP (Ireland)","Independent Alliance (Ireland)","NA6","NA7","NA8","NA9","NA10","NA11","NA12","NA13")
GROUPS<-c("ALDE","EPP","GREENS","S&D","new.moderate","new.moderate","GUE-NGL","GUE-NGL","new.moderate","EFDD","ni.far left","new.far left","ni.far right","new.far right","ni.moderate","ECR","ENF")
IR<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(IR$PARTIES),votes=c(IR$LAST_POLLS),n_seats=13,method="dhondt")
IR.parties=IR$PARTIES
EPgroups=IR$GROUP
IRELAND<-data.frame(EPgroups,IR.parties,seats)

IRELAND1<-aggregate(seats~EPgroups,data=IRELAND,sum)

#ITALY
it.csv<-read.csv(url("https://pollofpolls.eu/get/polls/IT-parliament/format/csv"))
lnpolls<-tail(it.csv$LN,3)
lnmean<-mean(lnpolls,na.rm=TRUE)
pdITpolls<-tail(it.csv$PD,3)
pdITmean<-mean(pdITpolls,na.rm=TRUE)
m5spolls<-tail(it.csv$M5S,3)
m5smean<-mean(m5spolls,na.rm=TRUE)
fdipolls<-tail(it.csv$FdI,3)
fdimean<-mean(fdipolls,na.rm=TRUE)
fiITpolls<-tail(it.csv$FI,3)
fiITmean<-mean(fiITpolls,na.rm=TRUE)
leupolls<-tail(it.csv$LeU,3)
leumean<-mean(leupolls,na.rm=TRUE)
epolls<-tail(it.csv$E,3)
emean<-mean(epolls,na.rm=TRUE)

LAST_POLLS<-c(lnmean,pdITmean,m5smean,fdimean,fiITmean,leumean,emean,0,0,0,0,0,0,0,0,0)
PARTIES<-c("Lega (Italy)","Partito Democratico (Italy)","M5S (Italy)","Fratelli d'Italia (Italy)","Forza Italia (Italy)","Liberie Uguali (Italy)","'+'Europa (Italy)","NA6","NA7","NA8","NA9","NA10","NA11","NA12","NA13","NA14")
GROUPS<-c("ENF","S&D","EFDD","ni.far right","EPP","S&D","new.moderate","GUE-NGL","ni.far left","new.far left","new.far right","ni.moderate","ECR","GREENS","ALDE","ni.far right")
IT<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(IT$PARTIES),votes=c(IT$LAST_POLLS),n_seats=76,method="dhondt")
IT.parties=IT$PARTIES
EPgroups=IT$GROUP
ITALY<-data.frame(EPgroups,IT.parties,seats)

ITALY1<-aggregate(seats~EPgroups,data=ITALY,sum)

#LATVIA
lv.csv<-read.csv(url("https://pollofpolls.eu/get/polls/LV-parliament/format/csv"))
lrapolls<-tail(lv.csv$LRA,3)
lramean<-mean(lrapolls,na.rm=TRUE)
natapolls<-tail(lv.csv$NatA,3)
natamean<-mean(natapolls,na.rm=TRUE)
nslpolls<-tail(lv.csv$NSL,3)
nslmean<-mean(nslpolls,na.rm=TRUE)
sdpspolls<-tail(lv.csv$SDPS,3)
sdpsmean<-mean(sdpspolls,na.rm=TRUE)
vienotibapolls<-tail(lv.csv$V,3)
vienotibamean<-mean(vienotibapolls,na.rm=TRUE)
zzspolls<-tail(lv.csv$ZZS,3)
zzsmean<-mean(zzspolls,na.rm=TRUE)
jkppolls<-tail(lv.csv$JKP,3)
jkpmean<-mean(jkppolls,na.rm=TRUE)
lkspolls<-tail(lv.csv$LKS,3)
lksmean<-mean(lkspolls,na.rm=TRUE)
parpolls<-tail(lv.csv$PAR,3)
parmean<-mean(parpolls,na.rm=TRUE)
kpvpolls<-tail(lv.csv$KPV,3)
kpvmean<-mean(kpvpolls,na.rm=TRUE)

LAST_POLLS<-c(lramean,natamean,nslmean,sdpsmean,vienotibamean,zzsmean,jkpmean,lksmean,parmean,kpvmean,0,0,0,0,0,0)
PARTIES<-c("LRA (Latvia)","NatA (Latvia)","NSL (Latvia)","Saskana (Latvia)","Vienotiba (Latvia)","ZZS (Latvia)","JKP (Latvia)","LKS (Latvia)","PAR (Latvia)","KPV (Latvia)","NA9","NA10","NA11","NA12","NA13","NA14")
GROUPS<-c("new.moderate","ECR","ni.far right","S&D","EPP","ALDE","new.moderate","GREENS","EPP","new.far right","new.far left","ni.moderate","ni.far left","EFDD","GUE-NGL","ENF")
LV<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(LV$PARTIES),votes=c(LV$LAST_POLLS),n_seats=8,method="dhondt")
LV.parties=LV$PARTIES
EPgroups=LV$GROUP
LATVIA<-data.frame(EPgroups,LV.parties,seats)

LATVIA1<-aggregate(seats~EPgroups,data=LATVIA,sum)

#LITHUANIA
lt.csv<-read.csv(url("https://pollofpolls.eu/get/polls/LT-parliament/format/csv"))
dppolls<-tail(lt.csv$DP,4)
dpmean<-mean(dppolls,na.rm=TRUE)
llrapolls<-tail(lt.csv$LLRA,4)
llramean<-mean(llrapolls,na.rm=TRUE)
lrlspolls<-tail(lt.csv$LRLS,4)
lrlsmean<-mean(lrlspolls,na.rm=TRUE)
lsdppolls<-tail(lt.csv$LSDP,4)
lsdpmean<-mean(lsdppolls,na.rm=TRUE)
lvzspolls<-tail(lt.csv$LVZS,4)
lvzsmean<-mean(lvzspolls,na.rm=TRUE)
tskdpolls<-tail(lt.csv$TSKD,4)
tskdmean<-mean(tskdpolls,na.rm=TRUE)
ttpolls<-tail(lt.csv$TT,4)
ttmean<-mean(ttpolls,na.rm=TRUE)
LSDDPpolls<-tail(lt.csv$LSDDP,4)
LSDDPmean<-mean(LSDDPpolls,na.rm=TRUE)

LAST_POLLS<-c(dpmean,llramean,lrlsmean,lsdpmean,lvzsmean,tskdmean,ttmean,LSDDPmean,0,0,0,0,0,0,0,0)
PARTIES<-c("Darbo Partija (Lithuania)","LLRA (Lithuania)","Liberaly Sajudis (Lithuania)","LSDP (Lithuania)","Lietuvos (Lithuania)","TS-KD (Lithuania)","TT (Lithuania)","LSDDP (Lithuania)","NA9","NA10","NA11","NA12","NA13","NA14","NA15","NA16")
GROUPS<-c("ALDE","ECR","ALDE","S&D","GREENS","EPP","EFDD","S&D","GUE-NGL","new.far right","new.far left","new.moderate","ni.far left","ni.far right","ni.moderate","ENF")
LT<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(LT$PARTIES),votes=c(LT$LAST_POLLS),n_seats=11,method="dhondt")
LT.parties=LT$PARTIES
EPgroups=LT$GROUP
LITHUANIA<-data.frame(EPgroups,LT.parties,seats)

LITHUANIA1<-aggregate(seats~EPgroups,data=LITHUANIA,sum)

#LUXEMBOURG
lu.csv<-read.csv(url("https://pollofpolls.eu/get/polls/LU-parliament/format/csv"))
adrpolls<-tail(lu.csv$ADR,3)
adrmean<-mean(adrpolls,na.rm=TRUE)
csvpolls<-tail(lu.csv$CSV,3)
csvmean<-mean(csvpolls,na.rm=TRUE)
dgpolls<-tail(lu.csv$DG,3)
dgmean<-mean(dgpolls,na.rm=TRUE)
dlpolls<-tail(lu.csv$DL,3)
dlmean<-mean(dlpolls,na.rm=TRUE)
dpLUpolls<-tail(lu.csv$DP,3)
dpLUmean<-mean(dpLUpolls,na.rm=TRUE)
lsappolls<-tail(lu.csv$LSAP,3)
lsapmean<-mean(lsappolls,na.rm=TRUE)

LAST_POLLS<-c(adrmean,csvmean,dgmean,dlmean,dpLUmean,lsapmean,0,0,0,0,0,0,0,0)
PARTIES<-c("ADR (Luxembourg)","CSV (Luxembourg)","Dei Greng (Luxembourg)","Dei Lenk (Luxembourg)","DP (Luxembourg)","LSAP (Luxembourg)","NA1","NA2","NA9","NA10","NA11","NA12","NA13","NA14")
GROUPS<-c("ECR","EPP","GREENS","GUE-NGL","ALDE","S&D","EFDD","new.far right","new.far left","new.moderate","ni.far left","ni.far right","ni.moderate","ENF")
LU<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(LU$PARTIES),votes=c(LU$LAST_POLLS),n_seats=6,method="dhondt")
LU.parties=LU$PARTIES
EPgroups=LU$GROUP
LUXEMBOURG<-data.frame(EPgroups,LU.parties,seats)

LUXEMBOURG1<-aggregate(seats~EPgroups,data=LUXEMBOURG,sum)

#MALTA
mt.csv<-read.csv(url("https://pollofpolls.eu/get/polls/MT-parliament/format/csv"))
adpolls<-tail(mt.csv$AD,3)
admean<-mean(adpolls,na.rm=TRUE)
plpolls<-tail(mt.csv$PL,3)
plmean<-mean(plpolls,na.rm=TRUE)
pnpolls<-tail(mt.csv$PN,3)
pnmean<-mean(pnpolls,na.rm=TRUE)

LAST_POLLS<-c(admean,plmean,pnmean,0,0,0,0,0,0,0,0,0,0,0)
PARTIES<-c("Alternattiva Demokratika (Malta)","Partit Laburista (Malta)","Partit Nazzjonalista (Malta)","NA3","NA4","NA5","NA1","NA2","NA9","NA10","NA11","NA12","NA13","NA14")
GROUPS<-c("GREENS","S&D","EPP","GUE-NGL","ALDE","ECR","EFDD","new.far right","new.far left","new.moderate","ni.far left","ni.far right","ni.moderate","ENF")
MT<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(MT$PARTIES),votes=c(MT$LAST_POLLS),n_seats=6,method="dhondt")
MT.parties=MT$PARTIES
EPgroups=MT$GROUP
MALTA<-data.frame(EPgroups,MT.parties,seats)

MALTA1<-aggregate(seats~EPgroups,data=MALTA,sum)

#NETHERLANDS
#Scraping from Wikipedia, because pollofpolls.eu only provides very rough. They lead to a tie between to parties, causing a problem for the code.  
nl.wiki.url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Dutch_general_election"
nl.wiki <- nl.wiki.url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()
nl.wiki <- nl.wiki[[1]] #uses the first (=[[1]]) dataframe in the list of dataframes
nl.wiki <- nl.wiki[-c(1), ] #deletes first row, because its same as header

vvdpolls1<-head(nl.wiki$VVD,2)
vvdpolls2<-as.numeric(gsub("%", "",vvdpolls1))
vvdpolls3<-data.frame(vvdpolls2)
vvdmean<-mean(vvdpolls3$vvdpolls2)

pvvpolls1<-head(nl.wiki$PVV,2)
pvvpolls2<-as.numeric(gsub("%", "",pvvpolls1))
pvvpolls3<-data.frame(pvvpolls2)
pvvmean<-mean(pvvpolls3$pvvpolls2)

cdapolls1<-head(nl.wiki$CDA,2)
cdapolls2<-as.numeric(gsub("%", "",cdapolls1))
cdapolls3<-data.frame(cdapolls2)
cdamean<-mean(cdapolls3$cdapolls2)

d66polls1<-head(nl.wiki$D66,2)
d66polls2<-as.numeric(gsub("%", "",d66polls1))
d66polls3<-data.frame(d66polls2)
d66mean<-mean(d66polls3$d66polls2)

glpolls1<-head(nl.wiki$GL,2)
glpolls2<-as.numeric(gsub("%", "",glpolls1))
glpolls3<-data.frame(glpolls2)
glmean<-mean(glpolls3$glpolls2)

sppolls1<-head(nl.wiki$SP,2)
sppolls2<-as.numeric(gsub("%", "",sppolls1))
sppolls3<-data.frame(sppolls2)
SPmean<-mean(sppolls3$sppolls2)

pvdapolls1<-head(nl.wiki$PvdA,2)
pvdapolls2<-as.numeric(gsub("%", "",pvdapolls1))
pvdapolls3<-data.frame(pvdapolls2)
pvdamean<-mean(pvdapolls3$pvdapolls2)

cupolls1<-head(nl.wiki$CU,2)
cupolls2<-as.numeric(gsub("%", "",cupolls1))
cupolls3<-data.frame(cupolls2)
cumean<-mean(cupolls3$cupolls2)

pvddpolls1<-head(nl.wiki$PvdD,2)
pvddpolls2<-as.numeric(gsub("%", "",pvddpolls1))
pvddpolls3<-data.frame(pvddpolls2)
pvddmean<-mean(pvddpolls3$pvddpolls2)

p50pluspolls1<-head(nl.wiki$`50+`,2)
p50pluspolls2<-as.numeric(gsub("%", "",p50pluspolls1))
p50pluspolls3<-data.frame(p50pluspolls2)
p50plusmean<-mean(p50pluspolls3$p50pluspolls2)

sgppolls1<-head(nl.wiki$SGP,2)
sgppolls2<-as.numeric(gsub("%", "",sgppolls1))
sgppolls3<-data.frame(sgppolls2)
sgpmean<-mean(sgppolls3$sgppolls2)

denkpolls1<-head(nl.wiki$DENK,2)
denkpolls2<-as.numeric(gsub("%", "",denkpolls1))
denkpolls3<-data.frame(denkpolls2)
denkmean<-mean(denkpolls3$denkpolls2)

fvdpolls1<-head(nl.wiki$FvD,2)
fvdpolls2<-as.numeric(gsub("%", "",fvdpolls1))
fvdpolls3<-data.frame(fvdpolls2)
fvdmean<-mean(fvdpolls3$fvdpolls2)

LAST_POLLS<-c(cdamean,cumean,d66mean,glmean,p50plusmean,pvdamean,pvddmean,pvvmean,SPmean,vvdmean,denkmean,fvdmean,sgpmean,0,0,0,0)
PARTIES<-c("CDA (Netherlands)","Christian Union (Netherlands)","D66 (Netherlands)","Green Left (Netherlands)","50plus (Netherlands)","PVDA (Netherlands)","Party for Animals (Netherlands)","Party for freedom (Netherlands)","SP (Netherlands)","VVD (Netherlands)","Denk (Netherlands)","Forum for Democracy (Netherlands)","SGP (Netherlands)","NA14","NA15","NA16","NA17")
GROUPS<-c("EPP","ECR","ALDE","GREENS","new.moderate","S&D","GUE-NGL","ENF","GUE-NGL","ALDE","new.far left","new.far right","ECR","ni.moderate","ni.far left","EFDD","ni.far right")
NL<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(NL$PARTIES),votes=c(NL$LAST_POLLS),n_seats=29,method="dhondt")
NL.parties=NL$PARTIES
EPgroups=NL$GROUP
NETHERLANDS<-data.frame(EPgroups,NL.parties,seats)

NETHERLANDS1<-aggregate(seats~EPgroups,data=NETHERLANDS,sum)

#POLAND
pl.csv<-read.csv(url("https://pollofpolls.eu/get/polls/PL-parliament/format/csv"))
PiSpolls<-tail(pl.csv$PiS,3)
PiSmean<-mean(PiSpolls,na.rm=TRUE)
popolls<-tail(pl.csv$PO,3)
pomean<-mean(popolls,na.rm=TRUE)
pslpolls<-tail(pl.csv$PSL,3)
pslmean<-mean(pslpolls,na.rm=TRUE)
sldpolls<-tail(pl.csv$SLD,3)
sldmean<-mean(sldpolls,na.rm=TRUE)
libpolls<-tail(pl.csv$Lib,3)
libmean<-mean(libpolls,na.rm=TRUE)
kukiz15polls<-tail(pl.csv$Kukiz15,3)
kukiz15mean<-mean(kukiz15polls,na.rm=TRUE)
npolls<-tail(pl.csv$N,3)
nmean<-mean(npolls,na.rm=TRUE)

LAST_POLLS<-c(PiSmean,pomean,pslmean,sldmean,libmean,kukiz15mean,nmean,0,0,0,0,0,0,0,0)
PARTIES<-c("PiS (Poland)","PO (Poland)","PSL (Poland)","SLD (Poland)","Wolnosc (Poland)","Kukiz15 (Poland)","Nowoczesna (Poland)","NA2","NA9","NA10","NA11","NA12","NA13","NA14","NA15")
GROUPS<-c("ECR","EPP","EPP","S&D","EFDD","new.far right","ALDE","GUE-NGL","new.far left","new.moderate","ni.far left","ni.far right","ni.moderate","GREENS","ENF")
PL<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(PL$PARTIES),votes=c(PL$LAST_POLLS),n_seats=52,method="dhondt")
PL.parties=PL$PARTIES
EPgroups=PL$GROUP
POLAND<-data.frame(EPgroups,PL.parties,seats)

POLAND1<-aggregate(seats~EPgroups,data=POLAND,sum)

#PORTUGAL 
pt.csv<-read.csv(url("https://pollofpolls.eu/get/polls/PT-parliament/format/csv"))
bepolls<-tail(pt.csv$BE,3)
bemean<-mean(bepolls,na.rm=TRUE)
cdspppolls<-tail(pt.csv$CDSPP,3)
cdsppmean<-mean(cdspppolls,na.rm=TRUE)
cdupolls<-tail(pt.csv$CDU,3)
cdumean<-mean(cdupolls,na.rm=TRUE)
ppdpsdpolls<-tail(pt.csv$PPDPSD,3)
ppdpsdmean<-mean(ppdpsdpolls,na.rm=TRUE)
psPTpolls<-tail(pt.csv$PS,3)
psPTmean<-mean(psPTpolls,na.rm=TRUE)

LAST_POLLS<-c(bemean,cdsppmean,cdumean,ppdpsdmean,psPTmean,0,0,0,0,0,0,0,0,0,0,0)
PARTIES<-c("Bloco Esquerda (Portugal)","CDS-PP (Portugal)","CDU (Portugal)","PPD/PSD (Portugal)","PS (Portugal)","NA3","NA4","NA2","NA9","NA10","NA11","NA12","NA13","NA14","NA15","NA16")
GROUPS<-c("GUE-NGL","EPP","GUE-NGL","EPP","S&D","new.far right","ALDE","EFDD","new.far left","new.moderate","ni.far left","ni.far right","ni.moderate","GREENS","ECR","ENF")
PT<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(PT$PARTIES),votes=c(PT$LAST_POLLS),n_seats=21,method="dhondt")
PT.parties=PT$PARTIES
EPgroups=PT$GROUP
PORTUGAL<-data.frame(EPgroups,PT.parties,seats)

PORTUGAL1<-aggregate(seats~EPgroups,data=PORTUGAL,sum)

#ROMANIA
ro.csv<-read.csv(url("https://pollofpolls.eu/get/polls/RO-parliament/format/csv"))
pmppolls<-tail(ro.csv$PMP,3)
pmpmean<-mean(pmppolls,na.rm=TRUE)
pnlpolls<-tail(ro.csv$PNL,3)
pnlmean<-mean(pnlpolls,na.rm=TRUE)
psdpolls<-tail(ro.csv$PSD,3)
psdmean<-mean(psdpolls,na.rm=TRUE)
udmrpolls<-tail(ro.csv$UDMR,3)
udmrmean<-mean(udmrpolls,na.rm=TRUE)
alderopolls<-tail(ro.csv$ALDERO,3)
alderomean<-mean(alderopolls,na.rm=TRUE)
usrpolls<-tail(ro.csv$USR,3)
usrmean<-mean(usrpolls,na.rm=TRUE)

LAST_POLLS<-c(pmpmean,pnlmean,psdmean,udmrmean,alderomean,usrmean,0,0,0,0,0,0,0,0,0,0)
PARTIES<-c("PMP (Romania)","PNL (Romania)","PSD (Romania)","UDMR (Romania)","ALDERO (Romania)","USR (Romania)","NA4","NA2","NA9","NA10","NA11","NA12","NA13","NA14","NA15","NA16")
GROUPS<-c("EPP","EPP","S&D","EPP","ALDE","new.moderate","GUE-NGL","EFDD","new.far left","new.far right","ni.far left","ni.far right","ni.moderate","GREENS","ECR","ENF")
RO<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(RO$PARTIES),votes=c(RO$LAST_POLLS),n_seats=33,method="dhondt")
RO.parties=RO$PARTIES
EPgroups=RO$GROUP
ROMANIA<-data.frame(EPgroups,RO.parties,seats)

ROMANIA1<-aggregate(seats~EPgroups,data=ROMANIA,sum)

#SWEDEN
se.csv<-read.csv(url("https://pollofpolls.eu/get/polls/SE-parliament/format/csv"))
cSEpolls<-tail(se.csv$C,3)
cSEmean<-mean(cSEpolls,na.rm=TRUE)
kdSEpolls<-tail(se.csv$KD,3)
kdSEmean<-mean(kdSEpolls,na.rm=TRUE)
lSEpolls<-tail(se.csv$L,3)
lSEmean<-mean(lSEpolls,na.rm=TRUE)
mSEpolls<-tail(se.csv$M,3)
mSEmean<-mean(mSEpolls,na.rm=TRUE)
mpSEpolls<-tail(se.csv$MP,3)
mpSEmean<-mean(mpSEpolls,na.rm=TRUE)
sSEpolls<-tail(se.csv$S,3)
sSEmean<-mean(sSEpolls,na.rm=TRUE)
sdSEpolls<-tail(se.csv$SD,3)
sdSEmean<-mean(sdSEpolls,na.rm=TRUE)
vSEpolls<-tail(se.csv$V,3)
vSEmean<-mean(vSEpolls,na.rm=TRUE)

LAST_POLLS<-c(cSEmean,kdSEmean,lSEmean,mSEmean,mpSEmean,sSEmean,sdSEmean,vSEmean,0,0,0,0,0,0,0,0)
PARTIES<-c("Centerpartiet (Sweden)","Kristdemokraterna (Sweden)","Liberalerna (Sweden)","Moderaterna (Sweden)","Miljopartiet (Sweden)","Socialdemokraterna (Sweden)","Sverigedemokraterna (Sweden)","Vansterpartiet (Sweden)","NA9","NA10","NA11","NA12","NA13","NA14","NA15","NA16")
GROUPS<-c("ALDE","EPP","ALDE","EPP","GREENS","S&D","EFDD","GUE-NGL","new.far left","new.far right","ni.far left","ni.far right","ni.moderate","new.moderate","ECR","ENF")
SE<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(SE$PARTIES),votes=c(SE$LAST_POLLS),n_seats=21,method="dhondt")
SE.parties=SE$PARTIES
EPgroups=SE$GROUP
SWEDEN<-data.frame(EPgroups,SE.parties,seats)

SWEDEN1<-aggregate(seats~EPgroups,data=SWEDEN,sum)

#SLOVENIA
si.csv<-read.csv(url("https://pollofpolls.eu/get/polls/SI-parliament/format/csv"))
desuspolls<-tail(si.csv$DeSUS,3)
desusmean<-mean(desuspolls,na.rm=TRUE)
nsipolls<-tail(si.csv$NSi,3)
nsimean<-mean(nsipolls,na.rm=TRUE)
sdSIpolls<-tail(si.csv$SD,3)
sdSImean<-mean(sdSIpolls,na.rm=TRUE)
sdsSIpolls<-tail(si.csv$SDS,3)
sdsSImean<-mean(sdsSIpolls,na.rm=TRUE)
pabSIpolls<-tail(si.csv$PAB,3)
pabSImean<-mean(pabSIpolls,na.rm=TRUE)
smcpolls<-tail(si.csv$SMC,3)
smcmean<-mean(smcpolls,na.rm=TRUE)
snspolls<-tail(si.csv$SNS,3)
snsmean<-mean(snspolls,na.rm=TRUE)
theleftpolls<-tail(si.csv$TheLeft,3)
theleftmean<-mean(theleftpolls,na.rm=TRUE)
lmsSIpolls<-tail(si.csv$LMS,3)
lmsSImean<-mean(lmsSIpolls,na.rm=TRUE)

LAST_POLLS<-c(desusmean,nsimean,sdSImean,sdsSImean,pabSImean,smcmean,snsmean,theleftmean,lmsSImean,0,0,0,0,0,0,0,0)
PARTIES<-c("DeSUS (Slovenia)","N.Si (Slovenia)","SD (Slovenia)","SDS (Slovenia)","Stranka Alenke Bratusek (Slovenia)","SMC (Slovenia)","SNS (Slovenia)","Levica (Slovenia)","Marjana Sarca (Slovenia)","NA10","NA11","NA12","NA13","NA14","NA15","NA16","NA17")
GROUPS<-c("ALDE","EPP","S&D","EPP","ALDE","ALDE","ni.far right","GUE-NGL","new.moderate","new.far right","new.far left","ni.far left","ni.moderate","EFDD","ECR","GREENS","ENF")
SI<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(SI$PARTIES),votes=c(SI$LAST_POLLS),n_seats=8,method="dhondt")
SI.parties=SI$PARTIES
EPgroups=SI$GROUP
SLOVENIA<-data.frame(EPgroups,SI.parties,seats)

SLOVENIA1<-aggregate(seats~EPgroups,data=SLOVENIA,sum)

#SLOVAKIA
sk.csv<-read.csv(url("https://pollofpolls.eu/get/polls/SK-parliament/format/csv"))
kdhSKpolls<-tail(sk.csv$KDH,3)
kdhSKmean<-mean(kdhSKpolls,na.rm=TRUE)
mosthidSKpolls<-tail(sk.csv$Most_Hid,3)
mosthidSKmean<-mean(mosthidSKpolls,na.rm=TRUE)
olanopolls<-tail(sk.csv$OLaNO,3)
olanomean<-mean(olanopolls,na.rm=TRUE)
sasSKpolls<-tail(sk.csv$SaS,3)
sasSKmean<-mean(sasSKpolls,na.rm=TRUE)
smerSDpolls<-tail(sk.csv$Smer_SD,3)
smerSDmean<-mean(smerSDpolls,na.rm=TRUE)
snsSKpolls<-tail(sk.csv$SNS,3)
snsSKmean<-mean(snsSKpolls,na.rm=TRUE)
smk.mkppolls<-tail(sk.csv$SMK_MKP,3)
smk.mkpmean<-mean(smk.mkppolls,na.rm=TRUE)
kotlebapolls<-tail(sk.csv$Kotleba_LSNS,3)
kotlebamean<-mean(kotlebapolls,na.rm=TRUE)
sme.rodinapolls<-tail(sk.csv$Sme_Rodina,3)
sme.rodinamean<-mean(sme.rodinapolls,na.rm=TRUE)
psSKpolls<-tail(sk.csv$PS,3)
psSKmean<-mean(psSKpolls,na.rm=TRUE)
spolupolls<-tail(sk.csv$SPOLU,3)
spolumean<-mean(spolupolls,na.rm=TRUE)

LAST_POLLS<-c(kdhSKmean,mosthidSKmean,olanomean,sasSKmean,smerSDmean,snsSKmean,smk.mkpmean,kotlebamean,sme.rodinamean,psSKmean,spolumean,0,0,0,0,0,0,0,0)
PARTIES<-c("KDH (Slovakia)","Most-Hid (Slovakia)","Obycajini L'udia (Slovakia)","SaS (Slovakia)","Smer-SD (Slovakia)","SNS (Slovakia)","SMK-MKP (Slovakia)","Kotleba-LSNS (Slovakia)","Sme-Rodina (Slovakia)","PS (Slovakia)","SPOLU (Slovakia)","NA12","NA13","NA14","NA15","NA16","NA17","NA18","NA19")
GROUPS<-c("EPP","EPP","ECR","ECR","S&D","EFDD","EPP","EFDD","new.far right","new.moderate","EPP","ni.far left","ni.moderate","ni.far right","GREENS","GUE-NGL","new.far left","ALDE","ENF")
SK<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(SK$PARTIES),votes=c(SK$LAST_POLLS),n_seats=14,method="dhondt")
SK.parties=SK$PARTIES
EPgroups=SK$GROUP
SLOVAKIA<-data.frame(EPgroups,SK.parties,seats)

SLOVAKIA1<-aggregate(seats~EPgroups,data=SLOVAKIA,sum)

#SPAIN
es.csv<-read.csv(url("https://pollofpolls.eu/get/polls/ES-parliament/format/csv"))
ppESpolls<-tail(es.csv$pp,3)
ppESmean<-mean(ppESpolls,na.rm=TRUE)
psoeESpolls<-tail(es.csv$psoe,3)
psoeESmean<-mean(psoeESpolls,na.rm=TRUE)
ercESpolls<-tail(es.csv$erc,3)
ercESmean<-mean(ercESpolls,na.rm=TRUE)
pnvESpolls<-tail(es.csv$pnv,3)
pnvESmean<-mean(pnvESpolls,na.rm=TRUE)
csESpolls<-tail(es.csv$cs,3)
csESmean<-mean(csESpolls,na.rm=TRUE)
voxpolls<-tail(es.csv$vox,3)
voxmean<-mean(voxpolls,na.rm=TRUE)
podemospolls<-tail(es.csv$podemos,3)
podemosmean<-mean(podemospolls,na.rm=TRUE)
pacmapolls<-tail(es.csv$pacma,3)
pacmamean<-mean(pacmapolls,na.rm=TRUE)
pdecatpolls<-tail(es.csv$pdecat,3)
pdecatmean<-mean(pdecatpolls,na.rm=TRUE)

LAST_POLLS<-c(ppESmean,psoeESmean,ercESmean,pnvESmean,csESmean,voxmean,podemosmean,pacmamean,pdecatmean,0,0,0,0,0,0,0)
PARTIES<-c("Partido Popular (Spain)","PSOE (Spain)","Esquerra Republicana (Spain)","PNV (Spain)","Ciudadanos (Spain)","Vox (Spain)","Podemos (Spain)","PACMA (Spain)","PDeCAT (Spain)","NA10","NA11","NA12","NA13","NA14","NA15","NA16")
GROUPS<-c("EPP","S&D","GREENS","ALDE","ALDE","ni.far right","GUE-NGL","new.moderate","ALDE","new.far right","new.far left","ni.moderate","EFDD","ECR","ni.far left","ENF")
ES<-data.frame(GROUPS,PARTIES,LAST_POLLS)

seats<-seats_ha(parties=c(ES$PARTIES),votes=c(ES$LAST_POLLS),n_seats=59,method="dhondt")
ES.parties=ES$PARTIES
EPgroups=ES$GROUP
SPAIN<-data.frame(EPgroups,ES.parties,seats)

SPAIN1<-aggregate(seats~EPgroups,data=SPAIN,sum)

# add country code column  #ISO codes here: https://en.wikipedia.org/wiki/ISO_3166-1
AUSTRIA$Country <- c("AUT") 
BELGIUM$Country <- c("BEL")
BULGARIA$Country <- c("BGR")
CROATIA$Country <- c("HRV")
CYPRUS$Country <- c("CYP")
CZECH$Country <- c("CZE")
DENMARK$Country <- c("DNK")
ESTONIA$Country <- c("EST")
FINLAND$Country <- c("FIN")
FRANCE$Country <- c("FRA")
GERMANY$Country <- c("DEU")
GREECE$Country <- c("GRC")
HUNGARY$Country <- c("HUN")
IRELAND$Country <- c("IRL")
ITALY$Country <- c("ITA")
LATVIA$Country <- c("LVA")
LITHUANIA$Country <- c("LTU")
LUXEMBOURG$Country <- c("LUX")
MALTA$Country <- c("MLT")
NETHERLANDS$Country <- c("NLD")
POLAND$Country <- c("POL")
PORTUGAL$Country <- c("PRT")
ROMANIA$Country <- c("ROU")
SWEDEN$Country <- c("SWE")
SLOVENIA$Country <- c("SVN")
SLOVAKIA$Country <- c("SVK")
SPAIN$Country <- c("ESP")

#rename party columns to same name
colnames(AUSTRIA)[2] <- "National Party"
colnames(BELGIUM)[2] <- "National Party"
colnames(BULGARIA)[2] <- "National Party"
colnames(CROATIA)[2] <- "National Party"
colnames(CYPRUS)[2] <- "National Party"
colnames(CZECH)[2] <- "National Party"
colnames(DENMARK)[2] <- "National Party"
colnames(ESTONIA)[2] <- "National Party"
colnames(FINLAND)[2] <- "National Party"
colnames(FRANCE)[2] <- "National Party"
colnames(GERMANY)[2] <- "National Party"
colnames(GREECE)[2] <- "National Party"
colnames(HUNGARY)[2] <- "National Party"
colnames(IRELAND)[2] <- "National Party"
colnames(ITALY)[2] <- "National Party"
colnames(LATVIA)[2] <- "National Party"
colnames(LITHUANIA)[2] <- "National Party"
colnames(LUXEMBOURG)[2] <- "National Party"
colnames(MALTA)[2] <- "National Party"
colnames(NETHERLANDS)[2] <- "National Party"
colnames(POLAND)[2] <- "National Party"
colnames(PORTUGAL)[2] <- "National Party"
colnames(ROMANIA)[2] <- "National Party"
colnames(SWEDEN)[2] <- "National Party"
colnames(SLOVENIA)[2] <- "National Party"
colnames(SLOVAKIA)[2] <- "National Party"
colnames(SPAIN)[2] <- "National Party"
```


## Create EU-wide aggregate data frames

```r
#### create aggregate data frames with all national data frames ---------------
df.countries <- bind_rows(AUSTRIA, BELGIUM, BULGARIA, CROATIA, CYPRUS, CZECH, DENMARK, ESTONIA, FINLAND, FRANCE, GERMANY, GREECE, HUNGARY, IRELAND, ITALY, LATVIA, LITHUANIA, LUXEMBOURG, MALTA, NETHERLANDS, POLAND, PORTUGAL, ROMANIA, SWEDEN, SLOVENIA, SLOVAKIA, SPAIN)
colnames(df.countries)[1]<-"EP_Group" #ajust column names to TotalSeatDis
colnames(df.countries)[3]<-"Seats_EP2019" #ajust column names to TotalSeatDis
df.countries <- df.countries[,c(4,2,1,3)] #reorder columns, so that country (the observation) is at the beginning
View(df.countries)

## TOTAL SEAT DISTRIBUTION 
TOTAL<-GERMANY1$seats+AUSTRIA1$seats+BELGIUM1$seats+BULGARIA1$seats+CROATIA1$seats+CYPRUS1$seats+CZECH1$seats+DENMARK1$seats+ESTONIA1$seats+FINLAND1$seats+FRANCE1$seats+GREECE1$seats+HUNGARY1$seats+IRELAND1$seats+ITALY1$seats+LATVIA1$seats+LITHUANIA1$seats+LUXEMBOURG1$seats+MALTA1$seats+NETHERLANDS1$seats+POLAND1$seats+PORTUGAL1$seats+ROMANIA1$seats+SWEDEN1$seats+SLOVENIA1$seats+SLOVAKIA1$seats+SPAIN1$seats
SEATDIS<-data.frame(GERMANY1$EPgroups,TOTAL)
colnames(SEATDIS)[1]<-"EP_Group"
colnames(SEATDIS)[2]<-"Seats_EP2019"

#Add percentage to SeatDis
percentage<-(SEATDIS$Seats/704)*100
percentage<-round(percentage, digits=2)
TotalSeatDis<-cbind(SEATDIS,percentage)
colnames(TotalSeatDis)[3]<-"% EP2019"

#Add election results from previous years in TotalSeatDis data frame
#Sources: http://www.europe-politique.eu/parlement-europeen.htm / by MS: http://www.europarl.europa.eu/elections2014-results/en/seats-member-state-absolut.html
# order of groups: ALDE, ECR, EFDD, ENF, EPP, GREENS, GUE-NGL, new.far left, new.far right, new.moderate, ni.far left, ni.far right, ni.moderate, S&D
Seats_EP2014<-as.integer(c(67,70,48,36,221,50,52,"NA","NA","NA",2,13,1,191))
percent_EP2014<-as.numeric(c(8.92,9.32,6.39,4.79,29.43,6.66,6.92,"NA","NA","NA",0.26,1.73,0.13,25.43))
#Seats_EP2009<-c(84,55,32,55,35,"NA","NA","NA",26~,26~,26~,184) #source for NIs https://en.wikipedia.org/wiki/European_Parliament_election,_2009
#EP2009percentage<-c(11.4,7.5,4.3,36.0,7.5,4.8,"NA","NA","NA",3.5~,3.5~,3.5~,25.0)
#percentage calculation (x / 751) * 100

SeatDis_Timeline<-cbind(TotalSeatDis,Seats_EP2014,percent_EP2014)
colnames(SeatDis_Timeline)[5]<-c("% EP2014")

#check if the numbers add up
sum(SeatDis_Timeline$Seats_EP2019) # should be 705 since total seats of the EP in 2019 will be 705 after Brexit
```

```
## [1] 705
```

```r
sum(SeatDis_Timeline$"% EP2014", na.rm=TRUE) # adding all % should result in 100. result is not exactly 100 due to rounding errors
```

```
## [1] 99.98
```

```r
sum(SeatDis_Timeline$Seats_EP2014, na.rm=TRUE) # should be 751 since the total seats of the EP since 2014 were 751 before Brexit
```

```
## [1] 751
```

```r
sum(SeatDis_Timeline$"% EP2019") #result is not exactly 100 due to rounding errors
```

```
## [1] 100.14
```

```r
View(SeatDis_Timeline)


## create new df for tableau without distinction between new/old non-aligned
Others_far.left <- c("Others_far.left", SeatDis_Timeline$Seats_EP2019[8] + SeatDis_Timeline$Seats_EP2019[11], SeatDis_Timeline$"% EP2019"[8] + SeatDis_Timeline$"% EP2019"[11],
            SeatDis_Timeline$Seats_EP2014[11], SeatDis_Timeline$"% EP2014"[11])
Others_far.right <- c("Others_far.right", SeatDis_Timeline$Seats_EP2019[9] + SeatDis_Timeline$Seats_EP2019[12], SeatDis_Timeline$"% EP2019"[9] + SeatDis_Timeline$"% EP2019"[12],
            SeatDis_Timeline$Seats_EP2014[12], SeatDis_Timeline$"% EP2014"[12])
Others_moderate <- c("Others_moderate", SeatDis_Timeline$Seats_EP2019[10] + SeatDis_Timeline$Seats_EP2019[13], SeatDis_Timeline$"% EP2019"[10] + SeatDis_Timeline$"% EP2019"[13],
             SeatDis_Timeline$Seats_EP2014[13], SeatDis_Timeline$"% EP2014"[13])
df_others <- rbind(Others_far.left, Others_far.right, Others_moderate) # bind vectors as rows in df ("Rbind")
colnames(df_others) <- c("EP_Group", "Seats_EP2019", "% EP2019", "Seats_EP2014", "% EP2014")

SeatDis_Timeline_v2 <- SeatDis_Timeline[-c(8,9,10,11,12,13), ]
SeatDis_Timeline_v2 <- rbind(SeatDis_Timeline_v2, df_others)
rownames(SeatDis_Timeline_v2) <- c(1:11)

#make variables correct numeric/integer/factor
SeatDis_Timeline_v2$"% EP2019" <- as.numeric(paste(SeatDis_Timeline_v2$"% EP2019"))
SeatDis_Timeline_v2$"% EP2014" <- as.numeric(paste(SeatDis_Timeline_v2$"% EP2014"))
SeatDis_Timeline_v2$"Seats_EP2019" <- as.integer(paste(SeatDis_Timeline_v2$"Seats_EP2019"))
SeatDis_Timeline_v2$"Seats_EP2014" <- as.integer(paste(SeatDis_Timeline_v2$"Seats_EP2014"))
SeatDis_Timeline_v2$EP_Group <- as.factor(paste(SeatDis_Timeline_v2$EP_Group))

#check if the numbers add up
sum(SeatDis_Timeline_v2$Seats_EP2019) # should be 705 since total seats of the EP in 2019 will be 705 after Brexit
```

```
## [1] 705
```

```r
sum(SeatDis_Timeline_v2$"% EP2014") #result is not exactly 100 due to rounding errors
```

```
## [1] 99.98
```

```r
sum(SeatDis_Timeline_v2$Seats_EP2014) # should be 751 since the total seats of the EP since 2014 were 751 before Brexit
```

```
## [1] 751
```

```r
sum(SeatDis_Timeline_v2$"% EP2019") #result is not exactly 100 due to rounding errors
```

```
## [1] 100.14
```

```r
View(SeatDis_Timeline_v2)
```

## European Parliament Seat Projection - data frame 

```r
print(SeatDis_Timeline_v2)
```

```
##            EP_Group Seats_EP2019 % EP2019 Seats_EP2014 % EP2014
## 1              ALDE           76    10.80           67     8.92
## 2               ECR           46     6.53           70     9.32
## 3              EFDD           39     5.54           48     6.39
## 4               ENF           52     7.39           36     4.79
## 5               EPP          182    25.85          221    29.43
## 6            GREENS           35     4.97           50     6.66
## 7           GUE-NGL           61     8.66           52     6.92
## 8               S&D          140    19.89          191    25.43
## 9   Others_far.left            3     0.42            2     0.26
## 10 Others_far.right           38     5.40           13     1.73
## 11  Others_moderate           33     4.69            1     0.13
```


## Upload data frames to google sheets

```r
#### upload data frames to google sheets -------------
# instructions: https://cran.r-project.org/web/packages/googlesheets/ 
# better instructions https://cran.r-project.org/web/packages/googlesheets/vignettes/basic-usage.html#download-sheets-as-csv-pdf-or-xlsx-file
# better tutorial https://www.r-bloggers.com/how-to-use-googlesheets-to-connect-r-to-google-sheets/

if(!is.element('googlesheets', installed.packages()[,1]))
{install.packages('googlesheets')
}else {print("googlesheets library already installed")}
library("googlesheets")

#install.packages("dplyr") # already installed in this script above
#library("dplyr")

gs_auth(new_user = TRUE) # authorize googlesheets package to access the google account

# my google sheets: https://docs.google.com/spreadsheets/u/1/?tgif=d
# link EP_Countries_gs https://docs.google.com/spreadsheets/d/1EGto7bkolmj38NAPifDmt9FDdFTEcgRgUIF1DIqQ08Y/edit#gid=0
# link SeatDis_Timeline_gs https://docs.google.com/spreadsheets/d/1BrlPGeZN7OfJbqXR0sf0K7zA-RlUSqs-HtDADuwiA-0/edit#gid=0
# tableau profile https://public.tableau.com/profile/moritz.l.camille.c#!/

## to upload an entirely new spreadsheet from R into gs
#EP_Countries_gs <- gs_new(title = "EP_Countries_gs", ws_title = "EP_Countries_gs", input = df.countries)
#EP_SeatDis_Timeline_gs <- gs_new(title = "SeatDis_Timeline_gs", ws_title = "SeatDis_Timeline_gs", input = SeatDis_Timeline)
#EP_SeatDis_Timeline_gs_v2 <- gs_new(title = "SeatDis_Timeline_gs_v2", ws_title = "SeatDis_Timeline_gs_v2", input = SeatDis_Timeline_v2)

## to download/register an existing gs into R in order to be able to edit it
EP_Countries_gs <- gs_title("EP_Countries_gs") # downloads/"registers" the spreadsheet (unformatted) by its title from the authorized google account
SeatDis_Timeline_gs <- gs_title("SeatDis_Timeline_gs")
SeatDis_Timeline_gs_v2 <- gs_title("SeatDis_Timeline_gs_v2")

# to update an existing spreadsheet from R
gs_edit_cells(EP_Countries_gs, ws = 1, trim = FALSE, input = df.countries) # update seems to work despite error message
gs_edit_cells(SeatDis_Timeline_gs, ws = 1, trim = FALSE, input = SeatDis_Timeline)
gs_edit_cells(SeatDis_Timeline_gs_v2, ws = 1, trim = FALSE, input = SeatDis_Timeline_v2)
```

