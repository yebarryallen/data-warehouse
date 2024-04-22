library(esc)
library(tidyverse)
library(readxl)
library(bruceR)
library(meta)
library(metafor)
science_meta <- read_excel("science_meta.xlsx") %>%
  filter(ourdata == 1)

newww<-escalc(measure="SMD", m1i=restore, m2i=control,n1i=n_restore, n2i=n_control,sd1i=resSD,sd2i = controlSD, data=science_meta)



rma(yi,vi,data = newww,subset = (Control_group_category  =="vs Natural"))%>%  trimfill()%>%funnel()
rma(yi,vi,data = newww) %>%  trimfill() %>%funnel()


rma(yi,vi,data = newww)->res1
trimfill(res1)
regtest(res1)

influence(res1)->influencial

influencial %>% plot()

newww<-newww%>%filter(native!="NA")
newww<-newww%>%filter(mix!="NA")
rma(yi,vi,data = newww,mods=~ age +native+Region+Control_group_category+mix)


############################
SP_calc <- pmap_dfr(science_meta,
                    function(restore, resSD, n_restore, control,
                             controlSD, n_control, arthor, ...){
                      esc_mean_sd(grp1m = restore,
                                  grp1sd = resSD,
                                  grp1n = n_restore,
                                  grp2m = control,
                                  grp2sd = controlSD,
                                  grp2n = n_control,
                                  study = arthor,
                                  es.type = "d") %>%
                        as.data.frame()})

# Let us catch a glimpse of the data
# The data set contains Hedges' g ("es") and standard error ("se")
glimpse(SP_calc)

SP_calc<-na.omit(SP_calc)
SP_calc$w <- 1/SP_calc$se^2
pooled_effect <- sum(SP_calc$w*SP_calc$es)/sum(SP_calc$w)
pooled_effect


#########我们自己的plot#########
m.gen <- metagen(TE = es,
                 seTE = se,
                 studlab = study,
                 data = SP_calc,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 title = "Summary")
forest(m.gen,
       sortvar = TE,
       prediction = TRUE,
       print.tau2 = FALSE,
       leftlabs = c("Study", "Effect Size", "Standard Error"))

##############亚组森林图##############

library(meta)

SP_calc <- pmap_dfr(science_meta,
                    function(restore, resSD, n_restore, control,
                             controlSD, n_control, Control_group_category, ...){
                      esc_mean_sd(grp1m = restore,
                                  grp1sd = resSD,
                                  grp1n = n_restore,
                                  grp2m = control,
                                  grp2sd = controlSD,
                                  grp2n = n_control,
                                  study = Control_group_category,
                                  es.type = "g") %>%
                        as.data.frame()})
SP_calc<-na.omit(SP_calc)
SP_calc$w <- 1/SP_calc$se^2

study_summary <- SP_calc %>%
  group_by(study) %>%
  summarise(es = mean(es),
            se = sqrt(sum(se^2)),
            weight = sum(w))

# 计算汇总效应大小和权重
m.gen <- metagen(TE = study_summary$es,
                 seTE = study_summary$se,
                 studlab = study_summary$study,
                 data = study_summary,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 title = "Summary")

# 绘制森林图
forest(m.gen,
       sortvar = TE,
       prediction = TRUE,
       print.tau2 = FALSE,
       leftlabs = c("Study", "Effect Size", "Standard Error"))

##############文章森林图##############


library(meta)

SP_calc <- pmap_dfr(science_meta,
                    function(restore, resSD, n_restore, control,
                             controlSD, n_control, arthor, ...){
                      esc_mean_sd(grp1m = restore,
                                  grp1sd = resSD,
                                  grp1n = n_restore,
                                  grp2m = control,
                                  grp2sd = controlSD,
                                  grp2n = n_control,
                                  study = arthor,
                                  es.type = "g") %>%
                        as.data.frame()})

SP_calc$w <- 1/SP_calc$se^2

study_summary <- SP_calc %>%
  group_by(study) %>%
  summarise(es = mean(es),
            se = sqrt(sum(se^2)),
            weight = sum(w))

# 计算汇总效应大小和权重
m.gen <- metagen(TE = study_summary$es,
                 seTE = study_summary$se,
                 studlab = study_summary$study,
                 data = study_summary,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 title = "Summary")

# 绘制森林图
forest(m.gen,
       sortvar = TE,
       prediction = TRUE,
       print.tau2 = FALSE,
       leftlabs = c("Study", "Effect Size", "Standard Error"))


#################新的亚组以及整体森林图###############
science_meta <- read_excel("science_meta.xlsx")


#因为sample样本量大部分都很小，所以我们采用Hedges' g
SP_calc <- pmap_dfr(science_meta,
                    function(restore, resSD, n_restore, control,
                             controlSD, n_control, arthor, ...){
                      esc_mean_sd(grp1m = restore,
                                  grp1sd = resSD,
                                  grp1n = n_restore,
                                  grp2m = control,
                                  grp2sd = controlSD,
                                  grp2n = n_control,
                                  study = arthor,
                                  es.type = "g") %>%
                        as.data.frame()})

SP_calc$idf<-1:nrow(SP_calc)




merged_data <-science_meta[, c("Publication", "category", "outcome", "Control_group_category", "variable", "idf","yi","vi")] %>%
  merge(SP_calc, by = "idf", all.x = TRUE)



merged_data1<-merged_data[merged_data$Control_group_category=="vs degraded",]
merged_data1<-na.omit(merged_data1)
merged_data1$w <- 1/merged_data1$se^2
pooled_effect <- sum(merged_data1$w*merged_data1$es)/sum(merged_data1$w)
pooled_effect
SE_pooled <- sqrt(1 / sum(merged_data1$w))

# 设置置信水平（例如95%置信水平）
alpha <- 0.05

# 计算Z分位数
Z_alpha_over_2 <- qnorm(1 - alpha / 2)

# 计算置信区间的上限和下限
CI_lower <- pooled_effect - Z_alpha_over_2 * SE_pooled
CI_upper <- pooled_effect + Z_alpha_over_2 * SE_pooled

# 打印结果
print(paste("Pooled effect size:", pooled_effect))
print(paste("Lower limit of confidence interval:", CI_lower))
print(paste("Upper limit of confidence interval:", CI_upper))

merged_data2<-merged_data[merged_data$Control_group_category=="vs UTF",]
#删除es 中值为-Inf
merged_data2<-merged_data2[merged_data2$es!=-Inf,]
merged_data3<-merged_data[merged_data$Control_group_category=="vs naturally- regenerated",]
merged_data4<-merged_data[merged_data$Control_group_category=="vs Natural",]



subsave<-c()
subup<-c()
sublow<-c()
subname<-c()
for (i in unique(merged_data1$category)){
  tp<-rma.mv(es ,
             se ,                 # sampling variances
             random = list(~ 1 | study/idf), # indicate level 3
             tdist = TRUE, # Knapp-Hartung adjustment
             subset = (category==i),
             data = merged_data2,
             method = "REML",
             slab = study)
  subsave<-c(subsave,tp$b[1,1])
  subname<-c(subname,i)
  sublow<-c(sublow,tp$ci.lb)
  subup<-c(subup,tp$ci.ub)
}




subres<-data.frame(typeex=subname,subvalue=subsave,Low=sublow,Up=subup)




subres_import<-subres%>%
  mutate(Cate='Subgroup')%>%
  bind_rows(tibble(typeex='Oveall effect size',subvalue=0.1937253,
                   Low=0.0074398268,Up=0.3800107,Cate='Main'))



subres_import%>%
  mutate(typeex = fct_reorder(typeex,subvalue,.desc=T)) %>%
  ggplot()+
  #Add data points and color them black
  #Add 'special' points for the summary estimates, by making them diamond shaped
  geom_point(aes(y=typeex, x=subvalue,color =Cate),
             shape=18, size=4)+
  #add the CI error bars
  geom_errorbarh(aes(y=typeex,xmin=Low, xmax=Up, color =Cate),
                 height=.1)+
  #Specify the limits of the x-axis and relabel it to something more meaningful
  scale_x_continuous(limits=c(-0.25,5), name="Effect Size (Cohen's d)")+
  scale_color_manual(values=c("#8E0C24", "#16335F"))+
  #Give y-axis a meaningful label
  ylab('Intervention Types')+
  #Add a vertical dashed line indicating an effect size of zero, for reference
  geom_vline(xintercept=0, linetype='dashed')+
  facet_grid(Cate~., scales= 'free', space='free')+
  theme_bw()+
  theme(strip.text.y = element_text(angle = 0),legend.position='none',
        axis.title.y = element_blank(),text = element_text(size=13),
        plot.caption = element_text(hjust = 0,size=9),plot.caption.position = "plot")
