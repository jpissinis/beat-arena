#Load packages
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)

#Determine player parameters

p1_phys_dmg<-100
p1_dmg_prec<-0.75
p1_dmg_prec_bonus<-1.25
p1_ar<-
p1_ar_prec
p1_ar_prec_bonus
p1_block_chance
p1_crit_chance
p1_crit_mult
p1_health

p2_phys_dmg
p2_dmg_prec
p2_dmg_prec_bonus
p2_ar<-100
p2_ar_prec<-0.8
p2_ar_prec_bonus<-1
p2_block_chance
p2_crit_chance
p2_crit_mult
p2_health

#Simulate
B<-10000
prec_module<-15

p1_attack<-replicate(B,{
  p1_precision<-sum(sample(c(0:1),prec_module,replace=T,prob=c(1-p1_dmg_prec,p1_dmg_prec)))/prec_module
  p2_precision<-sum(sample(c(0:1),prec_module,replace=T,prob=c(1-p2_ar_prec,p2_ar_prec)))/prec_module
  p1_phys_dmg*p1_precision*p1_dmg_prec_bonus-p2_ar*p2_precision*p2_ar_prec_bonus})
p1_attack<-as.data.frame(p1_attack)
p1_attack%>%ggplot(aes(p1_attack))+geom_density()

