mydata <- read.csv("ld_zjw1.csv")
# 计算年龄 step1:剥离出生年份
year <- read.csv("dep.csv")
dates <- as.Date(year$years, "%m/%d/%Y")

myformat <- "%Y"
age <- format(dates, myformat)
n_age <- as.numeric(age)
n1_age <- 2020-n_age
year$age <- n1_age
write.csv(year, file='age.csv')

#________________________________

data.first <- read.csv("try.csv")

# 1.处理缺失值
na.del.dat <- na.omit(data.first)
# 2.描述性统计分析
# 2.1 性别分布
gender.table <- with(data.first, table(性别))
gender.table
prop.table(gender.table)

# 3. 社会支持分布
library(psych)
social_support <- c('朋友支持得分', '家庭支持得分', '其它支持得分',  '社会支持总分')
describe(na.del.dat$social_support)
# 3.1 社会支持在不同初中生群体中的分布
library(multcomp)
attach(na.del.dat)
var_1 <- table(地区)
var_1
prop.table(var_1)
aggregate(社会支持总分, by = list(地区), FUN = mean)
aggregate(社会支持总分, by = list(地区), FUN = sd)
var.test(社会支持总分 ~ 地区)
t.test(社会支持总分 ~ 地区)

var_2 <- table(性别)
var_2
prop.table(var_2)
aggregate(社会支持总分, by = list(性别), FUN = mean)
aggregate(社会支持总分, by = list(性别), FUN = sd)
var.test(社会支持总分 ~ 性别)
t.test(社会支持总分 ~ 性别, var.equal = TRUE)


var_3 <- table(年级)
var_3
prop.table(var_3)
aggregate(社会支持总分, by = list(年级), FUN = mean)
aggregate(社会支持总分, by = list(年级), FUN = sd)
var.test(社会支持总分 ~ 年级)
t.test(社会支持总分 ~ 年级)

var_4 <- table(家庭住址.二分类.)
var_4
prop.table(var_4)
aggregate(社会支持总分, by = list(家庭住址.二分类.), FUN = mean)
aggregate(社会支持总分, by = list(家庭住址.二分类.), FUN = sd)
var.test(社会支持总分 ~ 家庭住址.二分类.)
t.test(社会支持总分 ~ 家庭住址.二分类., var.equal = TRUE)
aov_add <- aov(社会支持总分 ~ 家庭住址.二分类.)
summary(aov_add)

a <- table(是否独生子女)
a
prop.table(a)
aggregate(社会支持总分, by = list(是否独生子女), FUN = mean)
aggregate(社会支持总分, by = list(是否独生子女), FUN = sd)
var.test(社会支持总分 ~ 是否独生子女)
t.test(社会支持总分 ~ 是否独生子女, var.equal = TRUE)
aov_独生子女 <- aov(社会支持总分 ~ 是否独生子女)
summary(aov_独生子女)

b <- table(是否留守)
b
prop.table(b)
aggregate(社会支持总分, by = list(是否留守), FUN = mean)
aggregate(社会支持总分, by = list(是否留守), FUN = sd)
var.test(社会支持总分 ~ 是否留守)
t.test(社会支持总分 ~ 是否留守, var.equal = TRUE)
aov_leftbehind <- aov(社会支持总分 ~ 是否留守)
summary(aov_leftbehind)

table(学校心理健康服务获得2分类)
aggregate(社会支持总分, by = list(学校心理健康服务获得2分类), FUN = mean)
aggregate(社会支持总分, by = list(学校心理健康服务获得2分类), FUN = sd)
aov_学校心理健康服务获得2分类<- aov(社会支持总分 ~ 学校心理健康服务获得2分类)
summary(aov_学校心理健康服务获得2分类)

table(学校活动2分类)
aggregate(社会支持总分, by = list(学校活动2分类), FUN = mean)
aggregate(社会支持总分, by = list(学校活动2分类), FUN = sd)
aov_学校活动2分类 <- aov(社会支持总分 ~ 学校活动2分类)
summary(aov_学校活动2分类)

table(社区心理健康服务获得2分类)
aggregate(社会支持总分, by = list(社区心理健康服务获得2分类), FUN = mean)
aggregate(社会支持总分, by = list(社区心理健康服务获得2分类), FUN = sd)
aov_社区心理健康服务获得2分类<- aov(社会支持总分 ~ 社区心理健康服务获得2分类)
summary(aov_社区心理健康服务获得2分类)

table(社区活动2分类)
aggregate(社会支持总分, by = list(社区活动2分类), FUN = mean)
aggregate(社会支持总分, by = list(社区活动2分类), FUN = sd)
aov_社区活动2分类 <- aov(社会支持总分 ~ 社区活动2分类)
summary(aov_社区活动2分类)

table(学校心理健康服务获得5分类)
aggregate(社会支持总分, by = list(学校心理健康服务获得5分类), FUN = mean)
aggregate(社会支持总分, by = list(学校心理健康服务获得5分类), FUN = sd)
aov_学校心理健康服务获得5分类<- aov(社会支持总分 ~ 学校心理健康服务获得5分类)
summary(aov_学校心理健康服务获得5分类)

table(学校友好氛围3分类)
aggregate(社会支持总分, by = list(学校友好氛围3分类), FUN = mean)
aggregate(社会支持总分, by = list(学校友好氛围3分类), FUN = sd)
aov_学校友好氛围3分类 <- aov(社会支持总分 ~ 学校友好氛围3分类)
summary(aov_学校友好氛围3分类)

table(学校友好氛围3分类)
aggregate(社会支持总分, by = list(学校社团活动5分类), FUN = mean)
aggregate(社会支持总分, by = list(学校社团活动5分类), FUN = sd)
aov_学校社团活动5分类 <- aov(社会支持总分 ~ 学校社团活动5分类)
summary(aov_学校社团活动5分类)

table(社区心理健康服务获得5分类)
aggregate(社会支持总分, by = list(社区心理健康服务获得5分类), FUN = mean)
aggregate(社会支持总分, by = list(社区心理健康服务获得5分类), FUN = sd)
aov_社区心理健康服务获得5分类<- aov(社会支持总分 ~ 社区心理健康服务获得5分类)
summary(aov_社区心理健康服务获得5分类)

table(社区友好氛围3分类)
aggregate(社会支持总分, by = list(社区友好氛围3分类), FUN = mean)
aggregate(社会支持总分, by = list(社区友好氛围3分类), FUN = sd)
aov_社区友好氛围3分类 <- aov(社会支持总分 ~ 社区友好氛围3分类)
summary(aov_社区友好氛围3分类)

table(社区活动5分类)
aggregate(社会支持总分, by = list(社区活动5分类), FUN = mean)
aggregate(社会支持总分, by = list(社区活动5分类), FUN = sd)
aov_社区活动5分类 <- aov(社会支持总分 ~ 社区活动5分类)
summary(aov_社区活动5分类)

# 尝试简化

var_names <- c("地区", "性别", "民族", "年级", "家庭住址.二分类.", "是否独生子女",
               "是否留守", "学校心理健康服务获得2分类", "学校活动2分类",
               "社区心理健康服务获得2分类", "社区活动2分类",
               "学校心理健康服务获得5分类", "学校友好氛围3分类",
               "学校友好氛围3分类", "社区心理健康服务获得5分类", 
               "社区友好氛围3分类", "社区活动5分类")
for (i in 1:11){
  var_i <- na.del.dat[, var_names[i]]
  tab_i <- table(var_i)
  print(tab_i)
  print(prop.table(tab_i))
  if (var.test(社会支持总分 ~ var_i)$p.value > 0.01)
    print(t.test(社会支持总分 ~ var_i, var.equal = TRUE)$p.value)
  else
    print(t.test(社会支持总分 ~ var_i)$p.value)
  
}

for (i in 12:17){
  var_i <- na.del.dat[, var_names[i]]
  tab_i <- table(var_i)
  print(tab_i)
  print(prop.table(tab_i))
  aov_i <- aov(社会支持总分 ~ var_i)
  print(summary(aov_i)$p.value)
}


