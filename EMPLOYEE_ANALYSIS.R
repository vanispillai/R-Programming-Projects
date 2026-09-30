employees <- read.csv("employees_analysis.csv")
library(tidyverse)

head(employees)
str(employees)
summary(employees)
dim(employees)
colSums(is.na(employees))
employees <- employees %>%
distinct()

#Average monthly income of employees
#Highest average monthly income by department
employees %>%
group_by(department) %>%
summarise(Average_income = mean(monthly_income)
)%>%
arrange(desc(Average_income))
#no ofemployees in each department
employees %>%
group_by(department) %>%
summarise(total_employee = n()
)%>%
arrange(desc(total_employee))
#employees earning more than 10000
employees %>%
filter(monthly_income>10000)%>%
count(job_role,sort=TRUE)
#age distribution of employees
summary(employees$age)
table(employees$age)
employees$age_group <- cut(employees$age, 
                           breaks = c(20, 30, 40, 50, 60,70),
                           labels = c("20-29", "30-39", "40-49", "50-59", 
                                      "60-69"),right = FALSE)
table(employees$age_group)
#visualise age distribution
#Histogram of age distribution
ggplot(employees,aes(x=age))+
  geom_histogram(binwidth = 5, fill="steelblue", color="black")+
  labs(title="Age Distribution of Employees",x="Age",y="Count")
#bar chart of age group distribution
ggplot(employees,aes(x=age_group))+
    geom_bar(fill="darkorange")+
    labs(title="Age Group Distribution of Employees",x="Age Group",y="Count")

# Department and job-role attrition rate
attrition_by_role <- employees %>%
  group_by(department, job_role) %>%
  summarise(
    total_employees = n(),
    attrition_count = sum(attrition == "Yes"),
    attrition_rate = attrition_count / total_employees,
    .groups = "drop"
  )

# Visualisation of attrition by department and job role
ggplot(attrition_by_role, aes(x = reorder(job_role, attrition_rate),
                              y = attrition_rate)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ department, scales = "free_y") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  labs(title = "Attrition Rate by Department and Job Role",
       x = "Job Role",
       y = "Attrition Rate") +
  theme_minimal()
