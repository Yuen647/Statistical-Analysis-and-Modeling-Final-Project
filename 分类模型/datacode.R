library(readr)
library(tidyverse)
library(ggplot2)
employee <- read_csv('F:/dataAnalysisModel/employee.csv')
# 检查是否有任何缺失值
any_missing <- any(is.na(employee))
cat("是否存在任何缺失值：", ifelse(any_missing, "是", "否"), "\n")

# 检查是否有空字符串
has_empty_strings <- sapply(employee, function(x) any(x == ""))
cat("是否存在任何空字符串：", ifelse(any(has_empty_strings), "是", "否"), "\n")

# 1. 员工的学历背景分布
education_distribution <- employee %>%
  group_by(Education) %>%
  summarise(Count = n())

ggplot(education_distribution, aes(x = Education, y = Count, fill = Education)) +
  geom_bar(stat = "identity") +
  ggtitle("员工学历背景分布") +
  xlab("学历") +
  ylab("人数")

# 2. 不同城市员工服务年限差异
employee$ServiceYears <- 2023 - employee$JoiningYear
# 显著性检验：ANOVA
anova_result <- aov(ServiceYears ~ City, data = employee)
summary(anova_result)
# 可视化不同城市员工服务年限
ggplot(employee, aes(x = City, y = ServiceYears, fill = City)) +
  geom_boxplot() +
  ggtitle("不同城市员工服务年限分布") +
  xlab("城市") +
  ylab("服务年限")

# 3. 薪资等级与当前领域经验的关联性
cor_test <- cor.test(employee$PaymentTier, employee$ExperienceInCurrentDomain)
print(cor_test)

library(e1071) 
library(caret)  

# 数据预处理：将分类变量转换为因子
employee$Education <- as.factor(employee$Education)
employee$City <- as.factor(employee$City)
employee$Gender <- as.factor(employee$Gender)
employee$EverBenched <- as.factor(employee$EverBenched)
employee$LeaveOrNot <- as.factor(employee$LeaveOrNot)

# 划分数据集为训练集与测试集
set.seed(123)
train_index <- createDataPartition(employee$LeaveOrNot, p = 0.7, list = FALSE)
train_data <- employee[train_index, ]
test_data <- employee[-train_index, ]

naive_bayes_model <- naiveBayes(LeaveOrNot ~ ., data = train_data)
predictions <- predict(naive_bayes_model, test_data)

conf_matrix <- confusionMatrix(predictions, test_data$LeaveOrNot)
print(conf_matrix)

accuracy <- conf_matrix$overall['Accuracy']
print(paste("模型准确率为:", round(accuracy, 4)))


library(caret)
library(dplyr)
library(broom)
set.seed(123)  # 设置随机种子
train_index <- createDataPartition(employee$LeaveOrNot, p = 0.7, list = FALSE)
train_data <- employee[train_index, ]
test_data <- employee[-train_index, ]
# 构建逻辑回归模型
logit_model <- glm(LeaveOrNot ~ .-ServiceYears, data = train_data, family = binomial)
summary(logit_model)

# 预测测试集
test_data$pred_prob <- predict(logit_model, test_data, type = "response")
test_data$pred_class <- ifelse(test_data$pred_prob > 0.5, 1, 0)
test_data$pred_class <- as.factor(test_data$pred_class)

# 生成混淆矩阵
confusion_matrix <- table(Predicted = test_data$pred_class, Actual = test_data$LeaveOrNot)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("模型准确率:", round(accuracy, 4)))

# 可视化重要变量（回归系数）
coefficients <- tidy(logit_model)
coefficients <- coefficients %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(abs(estimate)))

ggplot(coefficients, aes(x = reorder(term, abs(estimate)), y = estimate, fill = estimate > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("逻辑回归模型中的重要变量") +
  xlab("变量") +
  ylab("回归系数") +
  theme_minimal()


