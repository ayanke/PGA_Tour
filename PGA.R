######## DATA PREAMBLE #########

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) devtools::install_github('taiyun/corrplot', build_vignettes = TRUE,dependencies=TRUE)
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")


library(knitr)
library(tidyverse)
library(kableExtra)
library(caret)
library(stringr)
library(corrplot)
library(matrixStats)
library(ggrepel)
library(e1071)

## LOAD DATA ##
source <- read.csv2("PGA.csv")
colnames(source)[1]<-"PLAYER_NAME"
seed<-250

## Fill NA's with previous player's performance or tour averages ##

temp<-colnames(source)[colSums(is.na(source))>0] #columns with NA values
mns <- colMeans(source[temp],na.rm=TRUE) #columns averages (i.e., tour averages)
y<-0

## Find number of NA values in dataset ##
for (x in temp) {
  y<-y+sum(is.na(source[,x]))
}

for (x in temp) {
  
  source<-source %>% group_by(PLAYER_NAME) %>% 
    arrange(PLAYER_NAME,desc(Year)) %>% fill(x,.direction="updown") %>% ungroup()
  
  source[is.na(source[,x]),x]<- as.integer(round(mns[x]))

}

## Define Bins for OWGR data ##

Q<-quantile(source$OWGR,probs=seq(0,1,0.1)) #get quantiles
tags <- as.character(c(10:1)) #create labels
group_tags<-cut(source$OWGR,breaks=Q,include.lowest = TRUE,right=FALSE,labels=tags) #cut up rankings according to quantiles
group_tags<-factor(group_tags,levels = tags,ordered = TRUE)

source <- source %>% mutate(OWGR_pct=group_tags)

idx_gt<-colnames(source)[str_detect(colnames(source),"^(APR_gt(10|20).+_Fwy)|(ARG_.+_RTP)")] #eliminate some redundant columns

source<-source %>% select(-idx_gt)

######## FIND OUTLIERS AND SCALE DATA ########

df<-source %>% select(-c(PLAYER_NAME,Year,OWGR,OWGR_pct,Earnings))

normality<-df %>% 
  apply(.,2,function (x) {
    low<-x<(-3*sd(x)+mean(x))
    high<-x>(3*sd(x)+mean(x))
    round(sum(c(low,high))/1142*100,1)
  }) %>% as.data.frame() %>% 
  set_names("Perc.Outliers") %>% 
  rownames_to_column("Stat") %>%
  mutate(
    category = str_extract(Stat,"^.+?(?=_)|Rounds"),
    Skewness=apply(df,2,function (x) {round(skewness(x),2)}),
    Kurtosis=apply(df,2,function (x) {round(kurtosis(x),2)})) %>% 
  arrange(desc(abs(Kurtosis))) #%>% filter(str_detect(Stat,"^SG")) #filter(str_detect(Stat,"^APR.*Dist$"))

normality %>% ggplot(aes(x=Skewness,y=Kurtosis,size=Perc.Outliers)) + 
  geom_point(aes(fill=category),color="black",pch=21) + 
  geom_vline(xintercept=0,color="red") + 
  geom_hline(yintercept=c(3,7),color=c("red","grey"),size=c(.5,1),linetype=c(1,2)) +
  geom_text_repel(data=top_n(normality,6,Kurtosis),size=4,aes(Skewness,Kurtosis,label=Stat),box.padding=1) +
  geom_text_repel(data=subset(normality,Stat %in% c("G_5.10_3Putt_Avoidance")),size=4,aes(Skewness,Kurtosis,label=Stat),nudge_x = 0.5) + 
  labs(title="Normality Assessment of Features")

## SCALE THE DATA USING A ROBUST SCALER APPROACH ##

scaled<-df %>% as.matrix() %>% 
  scale(center=colMedians(.),scale=colIQRs(.)) %>% 
  as_tibble() %>% cbind(select(source,OWGR_pct))

## SPLIT DATA INTO A TEST AND TRAINING SET ##

set.seed(seed,sample.kind="Rounding")

test_index <- createDataPartition(y = scaled$OWGR_pct, times = 1, p = 0.5, list = FALSE)
pga <- scaled[-test_index,]
test <- scaled[test_index,]

SGs<-colnames(pga)[which(str_detect(colnames(pga),"SG"))]

######## FEATURE CORRELATIONS #########

## Number of Rounds played vs. SGs ## 
# This plot shows how plays get better as they play more rounds, or they have to the opportunity to play more rounds as they get better #

temp<- source %>% group_by(PLAYER_NAME) %>% 
  summarize(Rounds_Played=sum(Rounds),SG_TOT=mean(SG_TOT),
            SG_OTT=mean(SG_OTT),
            SG_APR=mean(SG_APR),SG_ARG=mean(SG_ARG),
            SG_PUTT=mean(SG_PUTT)) %>% 
  gather(key="Category",value="value",-c(Rounds_Played,PLAYER_NAME))

ggplot() + 
  geom_point(subset(temp,Category=="SG_TOT"),mapping=aes(x=Rounds_Played,y=value),size=3,alpha=0.1,color="black") + 
  geom_hline(yintercept=0,linetype=2,color="#666666",size=1.5) +
  geom_smooth(temp,mapping=aes(x=Rounds_Played,y=value,color=Category),method=lm,se=FALSE,size=2) +
  ylim(-1,1) + 
  labs(x="Rounds Played",y="SG",title="Rounds Played vs. Strokes Gained")

# Correlation of Strokes Gained #

corr_SG <- 
  pga %>% mutate(OWGR_pct=as.numeric(OWGR_pct)) %>%
  select(SGs,OWGR_pct,Rounds) %>% 
  cor(method="spearman")

# Correlation of OTT Statistics #
corr_OTT <- 
  pga %>% mutate(OWGR_pct=as.numeric(OWGR_pct)) %>%
  select(colnames(.)[which(str_detect(colnames(.),"^OT.*$"))],OWGR_pct,Rounds,SGs) %>% 
  cor(method="spearman") 

# Correlation of APR Statistics #
corr_APR_Dist <- 
  pga %>% mutate(OWGR_pct=as.numeric(OWGR_pct)) %>%
  select(colnames(.)[which(str_detect(colnames(.),"^APR.*Dist$"))],OWGR_pct,Rounds,SGs) %>% 
  cor(method="spearman") 

corr_APR_RTP <- 
  pga %>% mutate(OWGR_pct=as.numeric(OWGR_pct)) %>%
  select(colnames(.)[which(str_detect(colnames(.),"^APR.*RTP$"))],OWGR_pct,Rounds,SGs) %>% 
  cor(method="spearman")

# Correlation of ARG Statistics #

corr_ARG <- 
  pga %>% mutate(OWGR_pct=as.numeric(OWGR_pct)) %>%
  select(colnames(.)[which(str_detect(colnames(.),"^ARG.*$"))],OWGR_pct,Rounds,SGs) %>% 
  cor(method="spearman")

# Correlation of PUTT Statistics #
corr_PUTT <- 
  pga %>% mutate(OWGR_pct=as.numeric(OWGR_pct)) %>%
  select(colnames(.)[which(str_detect(colnames(.),"^G_.*$"))],OWGR_pct,Rounds,SGs) %>% 
  cor(method="spearman") 

# Correlation of Scoring Statistics #
corr_SC <- 
  pga %>% mutate(OWGR_pct=as.numeric(OWGR_pct)) %>%
  select(colnames(.)[which(str_detect(colnames(.),"^SC.*$"))],OWGR_pct,Rounds,SGs) %>% 
  cor(method="spearman") 

######## RELATIONSHIPS AMONG SGs #########

# Relationship between SG_TOT OWGR # 
p1<-pga %>% select(OWGR_pct,SG_TOT) %>% set_names("OWGR","SG_TOT") %>%
  ggplot(aes(x=OWGR,y=SG_TOT)) + geom_boxplot() + 
  ylim(-2.5,2.5) + ggtitle("SG_TOT") +
  geom_hline(yintercept=0,color="red",linetype=2) +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),axis.title.y = element_blank())

# Relationship between SG_T2G and OWGR # 
p2<-pga %>% select(OWGR_pct,SG_T2G) %>% set_names("OWGR","SG_T2G") %>% 
  ggplot(aes(x=OWGR,y=SG_T2G)) + geom_boxplot() + 
  ylim(-2.5,2.5) + ggtitle("SG_T2G") +
  geom_hline(yintercept=0,color="red",linetype=2) +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),axis.title.y = element_blank())

# Relationship between SG_PUTT and OWGR # 
p3<-pga %>% select(OWGR_pct,SG_PUTT) %>% set_names("OWGR","SG_PUTT") %>%
  ggplot(aes(x=OWGR,y=SG_PUTT)) + geom_boxplot() + 
  ylim(-2.5,2.5) + ggtitle("SG_PUTT") +
  geom_hline(yintercept=0,color="red",linetype=2) +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),axis.title.y = element_blank())

gridExtra::grid.arrange(p1,p2,p3,ncol=3)

### CLOSER LOOK AT SG_T2G BY SEPARATING IT INTO SG_OTT, SG_APR, AND SG_ARG ###


# Relationship between SG_OTT and OWGR # 
p1<-pga %>% select(OWGR_pct,SG_OTT) %>% set_names("OWGR","SG_OTT") %>%
  ggplot(aes(x=OWGR,y=SG_OTT)) + geom_boxplot() + 
  ylim(-2.5,2.5) + ggtitle("SG_OTT") +
  geom_hline(yintercept=0,color="red",linetype=2) +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),axis.title.y = element_blank())

# Relationship between SG_APR and OWGR # 
p2<-pga %>% select(OWGR_pct,SG_APR) %>% set_names("OWGR","SG_APR") %>% 
  ggplot(aes(x=OWGR,y=SG_APR)) + geom_boxplot() + 
  ylim(-2.5,2.5) + ggtitle("SG_APR") +
  geom_hline(yintercept=0,color="red",linetype=2) +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),axis.title.y = element_blank())

# Relationship between SG_ARG and OWGR # 
p3<-pga %>% select(OWGR_pct,SG_ARG) %>% set_names("OWGR","SG_ARG") %>%
  ggplot(aes(x=OWGR,y=SG_ARG)) + geom_boxplot() + 
  ylim(-2.5,2.5) + ggtitle("SG_ARG") +
  geom_hline(yintercept=0,color="red",linetype=2) +
  theme(axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),axis.title.y = element_blank())

gridExtra::grid.arrange(p1,p2,p3,ncol=3)

######## FEATURE CORRELATIONS PLOTS#########

test_sig<-function(x,y){
  p <- c()
  for (c in x) {
    p[c]<-cor.test(x=pga[,c],y=pga[,y],method="spearman",exact=FALSE,alternative="two.sided")$p.value
  }
  r<-cbind(x,p) %>% as_tibble() %>% mutate(across(p,as.numeric)) %>% 
    set_names("Feature","p_val") %>% arrange(desc(p_val)) %>% 
    mutate("H0"=ifelse(p_val>0.05,TRUE,FALSE))
  if (all(r$H0==FALSE)){
    r[1,2:3]<-list(NA,TRUE)
  }
  r
}

### OTT CORRELATIONS ###

cols<-colnames(pga)[str_detect(colnames(pga),"^OT|Rounds")]
p_vals_OTT<-test_sig(cols,"SG_OTT") %>% mutate(Feat=str_extract(Feature,"(?<=OT_).+|Rounds"))


p1<-data.frame(corr_OTT) %>% select(OWGR_pct,SGs) %>% rename(OWGR=OWGR_pct) %>% filter(str_detect(rownames(.),"OT_|Rounds")) %>% 
  mutate(OT=str_extract(rownames(.),"(?<=OT_).+|Rounds")) %>% gather(key="SG",value="value",-OT) %>% 
  group_by(OT) %>% mutate(order=abs(value[SG=="SG_OTT"])) %>% ungroup() %>%
  ggplot(aes(x=reorder(OT,desc(order)),y=SG,fill=value)) + 
  geom_raster() + 
  geom_text(aes(label=round(value,2),fontface="bold"),size=3,show.legend = FALSE) +
  geom_point(data=subset(p_vals_OTT,H0==TRUE),aes(x=Feat,y=ifelse(is.na(p_val),NA,"SG_OTT"),fill=p_val),shape=4,color="red",size=10,show.legend = FALSE) +
  scale_fill_distiller(palette ="RdBu",direction=1,limits = c(-.7,0.7)) + 
  theme(axis.text.x = element_text(face="bold", size=8, angle=25,hjust=1),
        axis.text.y = element_text(face="bold", size=10)) + 
  labs(x=NULL,y=NULL,title="Correlation of OT to SG") + 
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15,title="Corr")) 

df <- data.frame(corr_OTT) %>% select(-c(SGs,OWGR_pct)) %>% filter(str_detect(rownames(.),"OT_|Rounds"))
rownames(df)<-str_extract(rownames(df),"(?<=OT_).+|Rounds")
colnames(df)<-str_extract(colnames(df),"(?<=OT_).+|Rounds")

par(mar = c(0, 0, 0, 0.5),bg="transparent")
df %>% as.matrix() %>% corrplot(type = "upper", order = "alphabet",tl.col = "black", 
           tl.srt = 45,tl.cex=0.7,cl.cex=.6,outline=TRUE, method="color",
           addCoef.col="black",diag=FALSE,cl.ratio=0.3,
           cl.align.text='l',number.cex=0.7)

p2<-recordPlot()
cowplot::plot_grid(p1,p2,rel_heights = c(1,1.4))


### SG CORRELATIONS ###

corrplot(corr_SG, type = "upper", order = "alphabet", hclust.method="median",tl.col = "black", 
           tl.srt = 45,tl.cex=1,cl.cex=1,outline=TRUE,addCoef.col="black",diag=FALSE, method="color",
           cl.ratio=0.3,cl.align.text='l')

### APR CORRELATIONS ###

cols<-colnames(pga)[str_detect(colnames(pga),"^APR_.+(?=_Dist)|Rounds")]
p_vals_APR<-test_sig(cols,"SG_APR") %>% mutate(Feat=str_extract(Feature,"(?<=APR_).+(?=_Dist)|Rounds"))

p3<-data.frame(corr_APR_Dist) %>% select(OWGR_pct,SGs) %>% rename(OWGR=OWGR_pct) %>% filter(str_detect(rownames(.),"APR_|Rounds")) %>% 
  mutate(OT=str_extract(rownames(.),"(?<=APR_).+(?=_Dist)|Rounds")) %>% gather(key="SG",value="value",-OT) %>% 
  group_by(OT) %>% mutate(order=abs(value[SG=="SG_APR"])) %>% ungroup() %>%
  ggplot(aes(x=reorder(OT,desc(order)),y=SG,fill=value)) + 
  geom_raster() + 
  geom_text(aes(label=round(value,2)),size=3,fontface="bold",show.legend = FALSE,angle=-90) +
  geom_point(data=subset(p_vals_APR,H0==TRUE),aes(x=Feat,y=ifelse(is.na(p_val),NA,"SG_APR"),fill=p_val),shape=4,color="red",size=4,show.legend = FALSE) +
  scale_fill_distiller(palette ="RdBu",direction=1,limits = c(-.7,0.7)) + 
  theme(axis.text.x = element_text(face="bold", size=7, angle=35,hjust=1),
        axis.text.y = element_text(face="bold", size=10)) + 
  labs(x=NULL,y=NULL,title="Correlation of APR_Dist to SG") + 
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15,title="Corr")) 

cols<-colnames(pga)[str_detect(colnames(pga),"^APR_.+(?=_RTP)|Rounds")]
p_vals_APR_RTP<-test_sig(cols,"SG_APR") %>% mutate(Feat=str_extract(Feature,"(?<=APR_).+(?=_RTP)|Rounds"))

corr_APR_RTP_SG_plot<- 
  data.frame(corr_APR_RTP) %>% select(OWGR_pct,SGs) %>% rename(OWGR=OWGR_pct) %>% filter(str_detect(rownames(.),"APR_|Rounds")) %>% 
  mutate(OT=str_extract(rownames(.),"(?<=APR_).+(?=_RTP)|Rounds")) %>% gather(key="SG",value="value",-OT) %>% 
  group_by(OT) %>% mutate(order=abs(value[SG=="SG_APR"])) %>% ungroup() %>%
  ggplot(aes(x=reorder(OT,desc(order)),y=SG,fill=value)) + 
  geom_raster() + 
  geom_text(aes(label=round(value,2)),size=3,fontface="bold",show.legend = FALSE,angle=-90) +
  geom_point(data=subset(p_vals_APR_RTP,H0==TRUE),aes(x=Feat,y=ifelse(is.na(p_val),NA,"SG_APR"),fill=p_val),shape=4,color="red",size=4,show.legend = FALSE) +
  scale_fill_distiller(palette ="RdBu",direction=1,limits = c(-.7,0.7)) + 
  theme(axis.text.x = element_text(face="bold", size=7, angle=35,hjust=1),
        axis.text.y = element_text(face="bold", size=10)) + 
  labs(x=NULL,y=NULL,title="Correlation of APR_RTP to SG") + 
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15,title="Corr")) 

df <- data.frame(corr_APR_Dist) %>% select(-c(SGs,OWGR_pct)) %>% filter(str_detect(rownames(.),"APR_|Rounds"))
rownames(df)<-str_extract(rownames(df),"(?<=APR_).+(?=_Dist)|Rounds")
colnames(df)<-str_extract(colnames(df),"(?<=APR_).+(?=_Dist)|Rounds")

par(mar = c(0, 0, 0, 0.5),bg="transparent")
df %>% as.matrix() %>% 
  corrplot(type = "upper", order = "FPC",tl.col = "black", method="color",
           tl.srt = 45,tl.cex=0.65,cl.cex=.6,outline=TRUE,diag=FALSE,
           cl.ratio=0.3,cl.align.text='l')

p4<-recordPlot()
cowplot::plot_grid(p3,p4,rel_widths = c(1.2,1))

### ARG CORRELATIONS ###

cols<-colnames(pga)[str_detect(colnames(pga),"^ARG_|Rounds")]
p_vals_ARG<-test_sig(cols,"SG_ARG") %>% mutate(Feat=str_extract(Feature,"(?<=ARG_).+|Rounds"))

p5<-data.frame(corr_ARG) %>% select(OWGR_pct,SGs) %>% rename(OWGR=OWGR_pct) %>% filter(str_detect(rownames(.),"ARG_|Rounds")) %>% 
  mutate(OT=str_extract(rownames(.),"(?<=ARG_).+|Rounds")) %>% gather(key="SG",value="value",-OT) %>% 
  group_by(OT) %>% mutate(order=abs(value[SG=="SG_ARG"])) %>% ungroup() %>%
  ggplot(aes(x=reorder(OT,desc(order)),y=SG,fill=value)) + 
  geom_raster() + 
  geom_text(aes(label=round(value,2)),size=3,fontface="bold",show.legend = FALSE) +
  geom_point(data=subset(p_vals_ARG,H0==TRUE),aes(x=Feat,y=ifelse(is.na(p_val),NA,"SG_ARG"),fill=p_val),shape=4,color="red",size=10,show.legend = FALSE) +
  scale_fill_distiller(palette ="RdBu",direction=1,limits = c(-.8,0.8)) + 
  theme(axis.text.x = element_text(face="bold", size=8, angle=35,hjust=1),
        axis.text.y = element_text(face="bold", size=10)) + 
  labs(x=NULL,y=NULL,title="Correlation of ARG to SG") + 
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15,title="Corr"))

df <- data.frame(corr_ARG) %>% select(-c(SGs,OWGR_pct)) %>% filter(str_detect(rownames(.),"ARG_|Rounds"))
rownames(df)<-str_extract(rownames(df),"(?<=ARG_).+|Rounds")
colnames(df)<-str_extract(colnames(df),"(?<=ARG_).+|Rounds")

par(mar = c(0, 0, 0, 0.5),bg="transparent")
df %>% as.matrix() %>%
  corrplot(type = "upper", order = "FPC",tl.col = "black", method="color",
           tl.srt = 45,tl.cex=0.7,cl.cex=.6,outline=TRUE,addCoef.col="black",diag=FALSE,
           cl.ratio=0.3,cl.align.text='l',number.cex=0.7)

p6<-recordPlot()
cowplot::plot_grid(p5,p6,rel_widths = c(1.2,1))

### PUTT CORRELATIONS ###

cols<-colnames(pga)[str_detect(colnames(pga),"^G_|Rounds")]
p_vals_PUTT<-test_sig(cols,"SG_PUTT") %>% mutate(Feat=str_extract(Feature,"(?<=G_).+|Rounds"))

p7<-data.frame(corr_PUTT) %>% select(OWGR_pct,SGs) %>% rename(OWGR=OWGR_pct) %>% filter(str_detect(rownames(.),"^G_|Rounds")) %>% 
  mutate(OT=str_extract(rownames(.),"(?<=G_).+|Rounds")) %>% gather(key="SG",value="value",-OT) %>% 
  group_by(OT) %>% mutate(order=abs(value[SG=="SG_PUTT"])) %>% ungroup() %>%
  ggplot(aes(x=reorder(OT,desc(order)),y=SG,fill=value)) + 
  geom_raster() + 
  geom_text(aes(label=round(value,2)),size=3,fontface="bold",show.legend = FALSE,angle=-90) +
  geom_point(data=subset(p_vals_PUTT,H0==TRUE),aes(x=Feat,y=ifelse(is.na(p_val),NA,"SG_PUTT"),fill=p_val),shape=4,color="red",size=8,show.legend = FALSE) +
  scale_fill_distiller(palette ="RdBu",direction=1,limits = c(-.8,0.8)) + 
  theme(axis.text.x = element_text(face="bold", size=8, angle=35,hjust=1),
        axis.text.y = element_text(face="bold", size=10)) + 
  labs(x=NULL,y=NULL,title="Correlation of PUTT to SG") + 
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15,title="Corr"))

df <- data.frame(corr_PUTT) %>% select(-c(SGs,OWGR_pct)) %>% filter(str_detect(rownames(.),"^G_|Rounds"))
rownames(df)<-str_extract(rownames(df),"(?<=G_).+|Rounds")
colnames(df)<-str_extract(colnames(df),"(?<=G_).+|Rounds")

par(mar = c(0, 0, 0, 0.5),bg="transparent")
df %>% as.matrix() %>%
  corrplot(type = "upper", order = "FPC",tl.col = "black", method="color",
           tl.srt = 45,tl.cex=0.7,cl.cex=.6,outline=TRUE,addCoef.col="black",diag=FALSE,
           cl.ratio=0.1,cl.align.text='l',number.cex=0.55)

p8<-recordPlot()
cowplot::plot_grid(p7,p8,rel_widths = c(1,1))

### SCORE CORRELATIONS ###

cols<-colnames(pga)[str_detect(colnames(pga),"^SC_|Rounds")]
p_vals_SCORE<-test_sig(cols,"SG_TOT") %>% mutate(Feat=str_extract(Feature,"(?<=SC_).+|Rounds"))

p9<- 
  data.frame(corr_SC) %>% select(OWGR_pct,SGs) %>% rename(OWGR=OWGR_pct) %>% filter(str_detect(rownames(.),"SC_|Rounds")) %>% 
  mutate(OT=str_extract(rownames(.),"(?<=SC_).+|Rounds")) %>% gather(key="SG",value="value",-OT) %>% 
  group_by(OT) %>% mutate(order=abs(value[SG=="OWGR"])) %>% ungroup() %>%
  ggplot(aes(x=reorder(OT,desc(order)),y=SG,fill=value)) + 
  geom_raster() + 
  geom_text(aes(label=round(value,2)),size=3,fontface="bold",show.legend = FALSE) +
  geom_point(data=subset(p_vals_SCORE,H0==TRUE),aes(x=Feat,y=ifelse(is.na(p_val),NA,"SG_TOT"),fill=p_val),shape=4,color="red",size=10,show.legend = FALSE) +
  scale_fill_distiller(palette ="RdBu",direction=1,limits = c(-1,1)) + 
  theme(axis.text.x = element_text(face="bold", size=9, angle=35,hjust=1),
        axis.text.y = element_text(face="bold", size=10)) + 
  labs(x=NULL,y=NULL,title="Correlation of SC to SG & OWGR") + 
  guides(fill = guide_colourbar(barwidth = 1, barheight = 15,title="Corr"))

df <- data.frame(corr_SC) %>% select(-c(SGs,OWGR_pct)) %>% filter(str_detect(rownames(.),"SC_|Rounds"))
rownames(df)<-str_extract(rownames(df),"(?<=SC_).+|Rounds")
colnames(df)<-str_extract(colnames(df),"(?<=SC_).+|Rounds")

par(mar = c(0, 0, 0, 0.5),bg="transparent")
df %>% as.matrix() %>%
  corrplot(type = "upper", order = "FPC",tl.col = "black", method="color",
           tl.srt = 45,tl.cex=0.7,cl.cex=.6,outline=TRUE,addCoef.col="black",diag=FALSE,
           cl.ratio=0.1,cl.align.text='l',number.cex=0.7)

p10<-recordPlot()
cowplot::plot_grid(p9,p10,rel_widths = c(1,1.1))


###### Compare Correlations between Distance and RTP metrics and use one for training #######
## We can see that distance metrics are more predictive of strokes-gained than RTP metrics ##

t1<-as.data.frame(corr_APR_RTP) %>% mutate("APR"=str_replace(row.names(.),"_RTP","")) %>% 
  select(APR,SG_APR) %>% as_tibble() %>% mutate("RTP.Rank"=rank(SG_APR)) %>% rename("RTP.SG_APR"=SG_APR)
t2<-as.data.frame(corr_APR_Dist) %>% mutate("APR"=str_replace(row.names(.),"_Dist","")) %>% 
  select(APR,SG_APR) %>% as_tibble() %>% mutate("Dist.Rank"=rank(SG_APR)) %>% rename("Dist.SG_APR"=SG_APR)

full_join(t1,t2,by="APR") %>% arrange(RTP.SG_APR) %>% print(n=15)

corr_APR <- corr_APR_Dist

######## CHOOSE FEATURES BASED ON CORRELATIONS AND/OR P-VALUES #########

choose_feats <- function(corr,cor_lim=0.15,feat_lim=0.85,SG,p_vals) {
  ## corr: corerlation matrix
  ## cor_lim: cut off correlation beneath which features will be eliminated
  ## feat_lim: Maximum correlation among features
  ## SG: The strokes gained metric (as a string) that you want to compare feature correlations to
  ## p_vals: tibble of p_values (two-sided) where two columns are named "Feature" 
  # and "p_val" containing the corresponding data
  ## WARNING: p_value cutoff is set to 0.05. 
  
  # If no p-values are supplied, then use cut-offs only
  if (missing(p_vals)) { 
    temp<-as.data.frame(corr) %>% filter(abs(.[SG])>cor_lim) %>% 
      select(SG) %>% mutate(Feature=rownames(.)) %>% 
      filter(str_detect(Feature,"^(SG|SC_Score|G_[BP]|OWGR).*|(_RTP)$")==FALSE)
  } 
  #If p-values are supplied, then use them to identify statistically significant features in addition to the cutoffs. 
  else {
    
    p_vals <- p_vals %>% filter(p_val<0.05)
    
    temp<-as.data.frame(corr) %>% filter(abs(.[SG])>cor_lim) %>% 
      select(SG) %>% mutate(Feature=rownames(.)) %>% semi_join(p_vals,by="Feature") %>%
      filter(str_detect(Feature,"^(SG|SC_Score|G_[BP]|OWGR).*|(_RTP)$")==FALSE)
  }
  
  Features<-temp$Feature
  corr_n<- pga %>% select(Features) %>% cor(method="pearson") #create correlation matrix among features, themselves
  
  ## Eliminate features that have greater than 0.85 correlation among themselves ##
  nonFeatures <- as.data.frame(corr_n) %>% gather(key="Feature",value="Correlation") %>%
    filter(Correlation<1 & abs(Correlation)>feat_lim) %>% left_join(temp, by="Feature") %>%
    mutate(SG=abs(.[SG])) %>% group_by(Correlation)
  
  feats<-Features
  
  # Failsafe in case all features are relevant in the data
  if (length(nonFeatures$Feature)>0) {
    nonFeatures<-nonFeatures %>% filter(SG==min(SG)) %>% pull(Feature)
    feats <- Features[!Features%in%nonFeatures]
  }
  feats
}

########## APPLY FUNCTION ############

feats<-c(choose_feats(corr_OTT,0.1,0.85,"SG_OTT",p_vals_OTT),
         choose_feats(corr_APR_Dist,0.1,0.85,"SG_APR",p_vals_APR),
         choose_feats(corr_ARG,0.15,0.85,"SG_ARG",p_vals_ARG),
         choose_feats(corr_PUTT,0.15,0.85,"SG_PUTT",p_vals_PUTT)) %>% unique()

## Eliminate features with little variance 
temp<-nearZeroVar(pga,uniqueCut = 20,names=TRUE) 
feats <- feats[!feats %in% temp]

###### TRAINING THE MODEL ########

get_scores<-function(cm,beta=0.5){
  b<-1/(1+beta^2)
  cm$byClass %>% cbind("Class"=rownames(.)) %>% 
    as_tibble() %>% mutate(across(-Class,as.numeric)) %>%
    mutate(Recall=beta^2*b*1/Recall,
           Precision=b*1/Precision,
           F1=1/(Recall+Precision)) %>% 
    select(Class,Sensitivity,Specificity,F1,'Balanced Accuracy') %>% 
    arrange(as.numeric(str_extract(Class,"\\d+$")))
}

###### Random Forests ######

control<-trainControl(method="cv",number=5,p=0.2)
idx<-colnames(pga)[str_detect(colnames(pga),"^SG|SC.*|G_[BP]|(RTP)$")] 
pga_nofeats<- pga %>% select(-idx)
test_nofeats<- test %>% select(-idx)

## Without Feature Selection ##

set.seed(seed,sample.kind="Rounding")

temp_rf <- train(OWGR_pct~.,method="rf",trControl=control,
                  tuneGrid=data.frame(mtry=seq(3,21,3)),
                  nTree=1500,
                  data=pga_nofeats)

set.seed(seed,sample.kind="Rounding")

train_rf <- train(OWGR_pct~.,method="rf",
                 tuneGrid=temp_rf$bestTune,
                 nTree=1500,
                 data=pga_nofeats)

y_hat_rf <- predict(train_rf,test_nofeats)
varimp_nofeats <- varImp(train_rf)
cm_nofeats <- confusionMatrix(y_hat_rf,test_nofeats$OWGR_pct)
Scores_rf_nofeats<-get_scores(cm_nofeats,1.75)

## With Feature Selection and more parameter tuning ##

pga_feats <- pga %>% select(feats,OWGR_pct)

set.seed(seed,sample.kind="Rounding")

## Seperate Test Set into another validation set ##

test_index <- createDataPartition(y = test$OWGR_pct, times = 1, p = 0.8, list = FALSE)
validation <- test[-test_index,]
test_2 <- test[test_index,]

set.seed(seed,sample.kind="Rounding")

###### Trial Parameter Tuning Methods ######

# task <- tuneRanger::makeClassifTask(data = pga_feats, target = "OWGR_pct")
# res <- tuneRanger::tuneRanger(task, num.trees = 1500,
#                   num.threads = 12, iters = 70, iters.warmup = 30)

# t<-tune.randomForest(
#   subset(pga_feats,select=-OWGR_pct),
#   pga_feats$OWGR_pct,
#   mtry=seq(3,18,3),
#   ntree=1500)

# ## Get the best parameters from testing ##
# top_performers<-t$performances %>% 
#   mutate(minerror=error-dispersion, 
#          error_round=as.numeric(substr(error, 1, 4))) %>% 
#   arrange(error_round,minerror) %>% head(4) 

#best<-top_performers %>% head(1) %>% select(mtry,ntree)

# train_rf <- train(OWGR_pct~.,method="rf",
#                   tuneGrid=best["mtry"],
#                   nTree=best[["ntree"]],
#                   data=pga_feats)

##############################################

## Train the model and optimize mtry ##

set.seed(seed,sample.kind="Rounding")

temp_rf <- train(OWGR_pct~.,method="rf",
                 trControl=control,
                 tuneGrid=data.frame(mtry=seq(3,21,3)),
                 nTree=1500,
                 data=pga_feats)

## Optimize value of nodesize ##

nodesizes <- seq(1, 161, 10)

sens <- sapply(nodesizes, function(ns){
  set.seed(seed,sample.kind="Rounding")
  train_rf<-train(OWGR_pct~., method = "rf", data = pga_feats,
        tuneGrid = temp_rf$bestTune,nTree=1500,
        nodesize = ns)
  y_hat_rf <- predict(train_rf,test_2[,feats])
  cm_feats <- confusionMatrix(y_hat_rf,test_2$OWGR_pct)
  cm_feats$byClass["Class: 1","Sensitivity"]
})

qplot(nodesizes, sens,size=I(2)) + 
  labs(x="Node Size",y="Sensitivity",
       title="Trial Node Size vs. Sensitivity for the Top Decile of Players")

set.seed(seed,sample.kind="Rounding")

## Train final RF Model using optimal values ##

train_rf <- train(OWGR_pct~.,method="rf",
                  tuneGrid=temp_rf$bestTune,
                  nodesize=41,
                  nTree=1500,
                  data=pga_feats)

y_hat_rf <- predict(train_rf,validation[,feats])
varimp_feats <- varImp(train_rf)
cm_feats <- confusionMatrix(y_hat_rf,validation$OWGR_pct)

Scores_rf<-get_scores(cm_feats,1.75)

#### Aggregation of middle classes to get better picture of overall random forests performance ####

levels(y_hat_rf) <- c("10",rep("2-9",8),"1")
val_agg<-validation$OWGR_pct
levels(val_agg)<-c("10",rep("2-9",8),"1")
cm_feats <- confusionMatrix(y_hat_rf,val_agg)

Scores_rf<-get_scores(cm_feats,1.75)

####### Principal Component Analysis #######

pga_sub<-subset(pga_nofeats,select=-OWGR_pct)
pca <- prcomp(pga_sub)

#Get summary statistics from Pincipal Componenet Analysis
summary(pca) 

#create dataframe to contain the cumulative variance explained by adding principal components
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2)) %>% as_tibble() %>% 
  rownames_to_column() %>% set_names("PCA","Variance_Explained") %>% 
  mutate(PCA=as.numeric(PCA)) 

#specify a variance threshold that defines which principal components we eliminate in the training model.
threshold <- 0.95
PCA_thresh<- var_explained %>% filter(Variance_Explained<threshold) %>% 
  summarize(m=max(PCA)) %>% pull(m)

#plot of the variance explained by successive principal components
var_explained %>% ggplot(aes(PCA,Variance_Explained)) + geom_point() + 
  geom_text_repel(data=subset(var_explained,PCA==PCA_thresh),
                  aes(PCA,Variance_Explained,
                      label=sprintf("PCA %d @ %.2f%%",PCA,Variance_Explained*100),size=7),
                  box.padding = 4,direction="y",color="red",show.legend = FALSE) + 
  ggtitle("Cumalitive Variance Explained by PCs")


#scatterplot showing the breakout of the top 10% of players from just the first two principal components
data.frame(pca$x[,1:2], Rank=pga_nofeats$OWGR_pct) %>% 
  ggplot(aes(PC1,PC2, fill = Rank))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

####### KNN MODEL with PCA #######
## Train KNN Model using Principal Components ##

# Rotate the test and validation sets correctly for predictions #

test_2_nofeats<- test_2 %>% select(-idx)
test_set<-subset(test_2_nofeats,select=-OWGR_pct)
col_means <- colMeans(test_set)
test_set <- as.matrix(sweep(test_set, 2, col_means)) %*% pca$rotation
test_set<-test_set[,1:PCA_thresh]

validation_nofeats<- validation %>% select(-idx)
validation_set<-subset(validation_nofeats,select=-OWGR_pct)
col_means <- colMeans(validation_set)
validation_set <- as.matrix(sweep(validation_set, 2, col_means)) %*% pca$rotation
validation_set<-validation_set[,1:PCA_thresh]

## Optimize value of K by maximizing sensitivity of the top player decile ##
K <- seq(5,455,10)

sens <- sapply(K, function(kn){
  set.seed(seed,sample.kind="Rounding")
  train_knn<- train(pca$x[,1:PCA_thresh],
                   pga$OWGR_pct,
                   method="knn",
                   tuneGrid=data.frame(k=kn))

  y_hat_knn <- predict(train_knn,test_set)
  mean(confusionMatrix(y_hat_knn,test_2_nofeats$OWGR_pct)$byClass[c("Class: 1"),"Sensitivity"])
}) %>% as_tibble() %>%  mutate(K=K)

sens %>% ggplot(aes(K,value)) + geom_point(size=2) + geom_line() +
  geom_text_repel(data=head(subset(sens,value==max(value)),1),
                  aes(K,value,label=sprintf("K=%d @ %.2f%%",K,value*100),size=7),
                  box.padding = 4,direction="x",color="red",show.legend = FALSE) +
  ylab("Sensitivity") +
  ggtitle("Trial K values vs. Sensitivity of the Top Decile of Players")

## Train final KNN model using optimized K ##

set.seed(seed,sample.kind="Rounding")

train_knn <- train(pca$x[,1:PCA_thresh],
                   pga$OWGR_pct,
                   method="knn",
                   tuneGrid=data.frame(k=K[which.max(sens$value)]))

y_hat_knn <- predict(train_knn,validation_set)
cm_knn<-confusionMatrix(y_hat_knn,validation_nofeats$OWGR_pct)
Scores_knn_pca<-get_scores(cm_knn,1.75)

## Aggregrate middle class rankings to obtain overall picture of KNN performance ##

levels(y_hat_knn) <- c("10",rep("2-9",8),"1")
cm <- confusionMatrix(y_hat_knn,val_agg)

Scores_knn_pca<-get_scores(cm,1.75)

########### KNN APPROACH USING CROSS VALIDATION ###############

# set.seed(seed,sample.kind="Rounding")
# 
# temp_knn<- train(pca$x[,1:PCA_thresh],
#                  pga$OWGR_pct,
#                  method="knn", trControl=control,
#                  tuneGrid=data.frame(k=seq(5,405,10)))
# 
# ggplot(temp_knn,highlight=TRUE)
# 
# set.seed(seed,sample.kind="Rounding")
# 
# train_knn <- train(pca$x[,1:PCA_thresh],
#                    pga$OWGR_pct,
#                    method="knn",
#                    tuneGrid=temp_knn$bestTune)
# 
# #fit <- knn3(pca$x[,1:PCA_thresh],pga$OWGR_pct)
# 
# test_set<-subset(test_nofeats,select=-OWGR_pct)
# col_means <- colMeans(test_set)
# test_set <- as.matrix(sweep(test_set, 2, col_means)) %*% pca$rotation
# test_set<-test_set[,1:PCA_thresh]
# 
# #y_hat_knn <- predict(fit,test_set,type="class")
# y_hat_knn <- predict(train_knn,test_set)
# cm_knn<-confusionMatrix(y_hat_knn,test_nofeats$OWGR_pct)
# Scores_knn<-get_scores(cm_knn,1.75)

## Comparison to KNN using same Features as Random Forests ##

# set.seed(seed,sample.kind="Rounding")
# 
# temp_knn<- train(OWGR_pct~.,
#                  method="knn",
#                  tuneGrid=data.frame(k=seq(5,100,3)),
#                  data=pga_feats)
# 
# set.seed(seed,sample.kind="Rounding")
# 
# train_knn<- train(OWGR_pct~.,
#                   method="knn",
#                   tuneGrid=temp_knn$bestTune,
#                   data=pga_feats)
# 
# y_hat_knn <- predict(train_knn,test[,feats])
# cm_knn<-confusionMatrix(y_hat_knn,test$OWGR_pct)
# Scores_knn_feats<-get_scores(cm_knn,1.75)

##### KNN MODEL with Feature Extraction from Correlations ######
K <- seq(5,455,10)

sens <- sapply(K, function(kn){
  set.seed(seed,sample.kind="Rounding")
  train_knn<- train(OWGR_pct~., method="knn",tuneGrid=data.frame(k=kn),data=pga_feats)
  y_hat_knn <- predict(train_knn,test_2[,feats])
  mean(confusionMatrix(y_hat_knn,test_2$OWGR_pct)$byClass[c("Class: 1"),"Sensitivity"]) 
}) %>% as_tibble() %>%  mutate(K=K)

# sens %>% ggplot(aes(K,value)) + geom_point(size=2) + geom_line() +
#   geom_text_repel(data=head(subset(sens,value==max(value)),1),
#                   aes(K,value,label=sprintf("K=%d @ %.2f%%",K,value*100),size=7),
#                   box.padding = 4,direction="x",color="red",show.legend = FALSE) +
#   ylab("Sensitivity") +
#   ggtitle("Trial K values vs. Sensitivity of the Top Decile of Players")

set.seed(seed,sample.kind="Rounding")

train_knn <- train(OWGR_pct~.,
                   method="knn",
                   tuneGrid=data.frame(k=K[which.max(sens$value)]),
                   data=pga_feats)

y_hat_knn <- predict(train_knn,validation[,feats])
cm_knn<-confusionMatrix(y_hat_knn,validation$OWGR_pct)
Scores_knn_feats<-get_scores(cm_knn,1.75)

## Aggregrate middle class rankings to obtain overall picture of KNN performance ##

levels(y_hat_knn) <- c("10",rep("2-9",8),"1")
cm <- confusionMatrix(y_hat_knn,val_agg)

Scores_knn_feats<-get_scores(cm,1.75)

Scores_knn <- left_join(Scores_knn_feats,Scores_knn_pca,by="Class") %>% 
  select(Class,Sensitivity.x,Sensitivity.y,Specificity.y,F1.y,"Balanced Accuracy.y") %>% 
  set_names("Class","Sensitivity w/o PCA","Sensitivity","Specificity","F1","Balanced Accuracy")


