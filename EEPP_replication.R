
library(tidyverse)
library(ggplot2)
library(sf)
library(bruceR)
# Import your data
library(haven)
library(modelsummary)
library(plm)
Data <- read_dta("Data.dta")
policy <- read_dta("D_Total low-carbon policy intensity aggregated to prefecture-level.dta")








# Import the shapefile for china provinces
shape <- st_read("map/市（等积投影）.shp")
# Merge your data with the shapefile
mapdata <- left_join(shape, policy, by = c("市" = "City_name_CN"))
# Create the choropleth map

##########绘制Command and control policy intensity的地图############
agr_mapdata_policy <- mapdata %>%
  group_by(City_name_EN) %>%
  summarize(avg_PI_all_city_CU = mean(PI_all_city_CC, na.rm = TRUE))


#PI_all_city_CU
ggplot() +
  geom_sf(data = agr_mapdata_policy, aes(fill = avg_PI_all_city_CU)) +
  scale_fill_gradientn(colors = c("#440154FF","#472D7BFF","#3B528BFF","#2C728EFF","#21908CFF","#27AD81FF","#5DC863FF","#AADC32FF","#FDE725FF"), na.value = "gray50",
                       limits = c(min(agr_mapdata_policy$avg_PI_all_city_CU), max(agr_mapdata_policy$avg_PI_all_city_CU))) +
  labs(fill = "Command and control policy intensity")

##########绘制PI_all_city_MB的地图############
agr_mapdata_policy <- mapdata %>%
  group_by(City_name_EN) %>%
  summarize(avg_PI_all_city_MB = mean(PI_all_city_MB, na.rm = TRUE))

ggplot() +
  geom_sf(data = agr_mapdata_policy, aes(fill = avg_PI_all_city_MB)) +
  scale_fill_gradientn(colors = c("#440154FF","#472D7BFF","#3B528BFF","#2C728EFF","#21908CFF","#27AD81FF","#5DC863FF","#AADC32FF","#FDE725FF"), na.value = "gray50",
                       limits = c(min(agr_mapdata_policy$avg_PI_all_city_MB), max(agr_mapdata_policy$avg_PI_all_city_MB))) +
  labs(fill = "Market-based policy intensity")

#######

# Create the choropleth map
agr_mapdata_labor  <- Data %>%
  group_by(city) %>%
  summarize(avg_labor = mean(labour, na.rm = TRUE))

agr_mapdata_labor <-left_join(shape, agr_mapdata_labor, by = c("市" = "city"))


ggplot() +
  geom_sf(data = agr_mapdata_labor, aes(fill = avg_labor)) +
  scale_fill_gradientn(colors = c("#440154FF","#472D7BFF","#3B528BFF","#2C728EFF","#21908CFF","#27AD81FF","#5DC863FF","#AADC32FF","#FDE725FF" ), na.value  = "gray50",
                       limits = c(min(agr_mapdata_labor$avg_labor), max(agr_mapdata_labor$avg_labor))) +
  labs(fill = "Number of employed laborers")

###########回归结果############

merged_data <- left_join(Data, policy, by = c("city" = "City_name_CN", "year" = "Year"))%>%
  mutate(ln_age= log(2023-Ipoyear)) %>%
  mutate(Lev=TD/TA) %>%
  mutate(ln_labor= labour/1000)

pollutionlist =list("B06", "B07", "B08", "B09", "B10", "B11", "B12",
                    "C25", "C26", "C28", "C29", "C30", "C31", "C32", "C33", "C39", "C40", "C42", "C43",
                    "D44",
                    "E47", "E48")
test <- merged_data %>%  filter(Sicda %in% c("C31", "C32", "D44"))
# 将 merged_data 转换为面板数据



# 构建多层次面板回归模型
model0 <- plm(ln_labor ~ PI_all_city_CC, data = merged_data,effect = "twoways", model = "within", index = c("id_org", "year"))
summary(model0)

model1 <- plm(ln_labor ~ PI_all_city_MB, data = merged_data,effect = "twoways", model = "within", index = c("id_org", "year"))
summary(model1)

model0.1 <- plm(ln_labor ~PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age, data = merged_data,effect = "twoways", model = "within", index = c("id_org", "year"))
summary(model0.1)

model1.1 <- plm(ln_labor ~PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age, data = merged_data,effect = "twoways", model = "within", index = c("id_org", "year"))
summary(model1.1)

"model2 <- lmer(ln_labor ~ PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age+(1 | id_org)+(1 | year)+(PI_all_city_CC | province), data = merged_data)
summary(model2)"

# 显示回归结果摘要
summary(model)

models <- list("command-and-control " = model0,
               "command-and-control " = model0.1,
               "market-based" = model1,
               "market-based" = model1.1)

modelsummary(models,stars = TRUE, statistic = "std.error",output="basice_Model.docx")  # 置信区间,output="Multi_Model.docx"


#############机理解释 PI_all_city_CC############
model3 <- plm(EG ~ PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = merged_data)
model4 <- plm(FG ~ PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = merged_data)
model5 <- plm(RDlabour ~ PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = merged_data)
model6 <- plm(profit ~ PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = merged_data)

models1 <- list("EG" = model3,
               "FG" = model4,
               "RDlabour" = model5,
               "profit" = model6)

modelsummary(models1,stars = TRUE, statistic = "std.error",output="机理解释 PI_all_city_CC.docx")  # 置信区间,output="Multi_Model.docx"

#############机理解释 PI_all_city_MB############
model3 <- plm(EG ~ PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = merged_data)
model4 <- plm(FG ~ PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = merged_data)
model5 <- plm(RDlabour ~ PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = merged_data)
model6 <- plm(profit ~ PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = merged_data)

models1 <- list("EG" = model3,
                "FG" = model4,
                "RDlabour" = model5,
                "profit" = model6)

modelsummary(models1,stars = TRUE, statistic = "p.value",output="机理解释 PI_all_city_MB.docx")  # 置信区间,output="Multi_Model.docx"

#######################异质性分析 less_pollution#######################
less_pollution <- merged_data %>%  filter(!Sicda %in% c("C31", "C32", "D44"))
model0.1 <- plm(ln_labor ~PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age, data = less_pollution,effect = "twoways", model = "within", index = c("id_org", "year"))
model0.2 <- plm(EG ~ PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = less_pollution)
model0.3 <- plm(FG ~ PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = less_pollution)

model1.1 <- plm(ln_labor ~PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age, data = less_pollution,effect = "twoways", model = "within", index = c("id_org", "year"))
model1.2 <- plm(EG ~ PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = less_pollution)
model1.3 <- plm(FG ~ PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = less_pollution)

heterogeneity1  <- list("ln_labor" = model0.1,
                "EG " = model0.2,
                "FG" = model0.3,
                "ln_labor" = model1.1,
                "EG" = model1.2,
                "FG" = model1.3)
modelsummary(heterogeneity1,stars = TRUE, statistic = "std.error", output="heterogeneity1_Model_less_pollution.docx")  # 置信区间,output="Multi_Model.docx"

#######################异质性分析 heavy_pollution#######################

heavy_pollution <- merged_data %>%  filter(Sicda %in% c("C31", "C32", "D44"))

model0.1 <- plm(ln_labor ~PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age, data = heavy_pollution,effect = "twoways", model = "within", index = c("id_org", "year"))
model0.2 <- plm(EG ~ PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = heavy_pollution)
model0.3 <- plm(FG ~ PI_all_city_CC +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = heavy_pollution)

model1.1 <- plm(ln_labor ~PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age, data = heavy_pollution,effect = "twoways", model = "within", index = c("id_org", "year"))
model1.2 <- plm(EG ~ PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = heavy_pollution)
model1.3 <- plm(FG ~ PI_all_city_MB +ln_Size+Roa+ln_Pgdp+Lev+IS+ln_age,effect = "twoways", model = "within", index = c("id_org", "year"), data = heavy_pollution)

heterogeneity2  <- list("ln_labor" = model0.1,
                        "EG " = model0.2,
                        "FG" = model0.3,
                        "ln_labor" = model1.1,
                        "EG" = model1.2,
                        "FG" = model1.3)
modelsummary(heterogeneity2,stars = TRUE, statistic = "std.error", output="heterogeneity1_Model_heavy_pollution.docx")  # 置信区间,output="Multi_Model.docx"


##########观察省份的差异############
p1 <- ggplot(merged_data, aes(PI_all_city_CU, ln_labor)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

p2 <- p1 +
  facet_wrap(vars(Pro_name_EN), labeller = label_both)

p1; p2

