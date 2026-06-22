# Diagnosis figure (EN): years of schooling by household discipline.
# (Panel A — harsh discipline by area — removed; now covered on slide 8.)
# EDSA women only (discipline module not asked of men) -> footnoted on slide.
suppressPackageStartupMessages({ library(haven); library(dplyr); library(srvyr); library(ggplot2); library(here) })
options(survey.lonely.psu="adjust"); zl<-function(x) as.numeric(haven::zap_labels(x))
dir.create(here::here("output","figures"), showWarnings=FALSE, recursive=TRUE)
m   <- read_sav("data/EDS_2023/EDSA2023_Mujer.sav")
viv <- read_sav("data/EDS_2023/EDSA2023_Vivienda.sav") |> transmute(folio, qriqueza=as.numeric(zap_labels(qriqueza)))
m <- left_join(m, viv, by="folio")
dc<-function(a){o<-c();for(b in c("ms11_1147","ms11_1148"))for(ad in c("A","B","C"))for(x in a)o<-c(o,paste0(b,"_",ad,x));o[o%in%names(m)]}
ap<-c("A","B","O","I"); asked<-rowSums(!is.na(m[dc(c(ap,"C","D","K","L","M","N"))]))>0
any1<-function(c_){c_<-c_[c_%in%names(m)]; as.integer(rowSums(sapply(m[c_],function(x) zl(x)==1&!is.na(x)),na.rm=TRUE)>0)}
m <- m |> mutate(harsh=ifelse(asked,any1(dc(ap)),NA_integer_), rural=as.integer(zl(area)==2),
                 aestudio=zl(aestudio))
des <- m |> as_survey_design(ids=upm, strata=estrato, weights=ponderadorm, nest=TRUE)
blue<-"#1CABE2"; pink<-"#E2007A"; ink<-"#00377C"; grey<-"#7A8487"
base <- theme_minimal(base_size=14)+theme(plot.title=element_text(face="bold",color=ink,size=14),
  plot.subtitle=element_text(color=grey,size=11), axis.text.x=element_text(face="bold",size=13),
  panel.grid.major.x=element_blank(), legend.position="none")

# Years of schooling by harsh-discipline home (single panel)
b <- des |> filter(!is.na(harsh),!is.na(aestudio)) |> group_by(harsh) |>
  summarise(est=survey_mean(aestudio,na.rm=TRUE,vartype="se")) |>
  filter(!is.na(harsh)) |>
  mutate(grp=ifelse(harsh==1,"Hogar con\ndisciplina severa","Otro hogar"),
         hi = est + 1.96*est_se)
b$grp <- factor(b$grp, levels=c("Otro hogar","Hogar con\ndisciplina severa"))

pB <- ggplot(b, aes(grp,est,fill=grp))+geom_col(width=0.6)+
  geom_errorbar(aes(ymin=est-1.96*est_se,ymax=hi),width=0.15,color=ink)+
  # label lifted ABOVE the upper CI whisker (item 2)
  geom_text(aes(y=hi+0.4,label=sprintf("%.1f años",est)),vjust=0,fontface="bold",color=ink,size=5)+
  scale_fill_manual(values=c("Hogar con\ndisciplina severa"=pink,"Otro hogar"=blue))+
  scale_y_continuous(limits=c(0,13),expand=expansion(mult=c(0,0.05)))+
  labs(title="Años de escolaridad según disciplina en el hogar",
       subtitle="Mujeres 12–49 · EDSA 2023, ponderado", x=NULL,y=NULL)+base

ggsave(here::here("output","figures","f7b_diagnosis_panel_es.png"), pB, width=6.4, height=4.8, dpi=150, bg="white")
cat("wrote f7b_diagnosis_panel_es.png (single panel)\n")
