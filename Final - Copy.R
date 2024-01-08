#Load and Describe Data 
Data<-read.csv('Covid Liver.csv')
head(Data)
str(Data)
dim(Data)

# Count occurrences of each gender value
Year_counts <- table(Data$Year)
Status_Count <- table(Data$Alive_Dead)

# Display the counts
print(Year_counts)
print(Status_Count)
summary(Data)

# Calculate the percentage of missing values in each column
missing_percentage <- (colSums(is.na(Data)) / nrow(Data)) * 100

# Create a bar plot to show the percentage of missing values
barplot(missing_percentage, 
        main = "Percentage of Missing Values in Each Column",
        xlab = "Columns", ylab = "Percentage Missing",
        names.arg = names(missing_percentage),
        col = "blue")

#Drop columns with more than 20% of missing data
sapply(Data, function(x) sum(is.na(x)))
columns_to_drop <- c("ICC_TNM_Stage","Type_of_incidental_finding" , "Date_incident_surveillance_scan","Surveillance_effectiveness", "Mode_of_surveillance_detection","Surveillance_programme" ,"HCC_TNM_Stage","Cirrhosis","Bleed","Etiology","Time_MDM_1st_treatment","Time_diagnosis_1st_Tx","Time_decisiontotreat_1st_treatment","Months_from_last_surveillance")
column_indices_to_drop <- which(names(Data) %in% columns_to_drop)
My_Data <- Data[, -column_indices_to_drop]
summary(My_Data)
str(My_Data)

#Bar Plot

ggplot(My_Data, aes(fill=Alive_Dead, y=Survival_fromMDM, x=Treatment_grps)) +
  geom_bar(position='dodge', stat='identity') +
  ggtitle('Survival_fromMDM by Alive_Dead') +
  xlab('Alive_Dead') +
  ylab('Survival_fromMDM (in Months)') +
  scale_fill_manual('Event', values=c('coral2','blue'))

# Filter out rows with NA values
filtered_data <- My_Data[complete.cases(My_Data), ]

# Create a violin plot using ggplot2
ggplot(filtered_data, aes(x = as.factor(Year), y = Size, fill = HCC_BCLC_Stage)) +
  geom_violin() +
  labs(title = "Violin Plot of Tumor Size Distribution by Year and HCC BCLC Stage",
       x = "Year", y = "Tumor Size", fill = "HCC BCLC Stage") +
  theme_minimal()


# Create a histogram using ggplot2, fill by Tumor Size
ggplot(My_Data, aes(x = Month, fill = Mode_Presentation)) +
  geom_histogram(stat = "count", position = "dodge", color = "blue") +
  labs(title = "Histogram of Diagnoses by Month and Mode of Presentation",
       x = "Month", y = "Count", fill = "Mode of Presentation") +
  theme_minimal()

# Extract particular numerical columns into a new data frame
Numerical_columns <- My_Data[, c("Month", "Age","Size","Survival_fromMDM","PS")]
str(Numerical_columns)

# Extract particular categorical columns into a new data frame
ctd<-c("Month","Age","Size","Survival_fromMDM","PS")
col_into_drop <- which(names(My_Data) %in% ctd)
My_Data1 <- My_Data[, -col_into_drop]
str(My_Data1)


# Function to perform mode imputation for categorical variables
mode_impute <- function(data) {
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      mode_value <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
      data[[col]] <- ifelse(is.na(data[[col]]), mode_value, data[[col]])
    }
  }
  return(data)
}

# Call the function to perform mode imputation
imputed_data_framecat <- mode_impute(My_Data1)
sapply(imputed_data_framecat, function(x) sum(is.na(x)))

# Function to perform mean imputation for numerical variables
mean_impute <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      mean_value <- mean(data[[col]], na.rm = TRUE)
      data[[col]] <- ifelse(is.na(data[[col]]), mean_value, data[[col]])
    }
  }
  return(data)
}

# Call the function to perform mean imputation
imputed_data_framenumeric <- mean_impute(Numerical_columns)
sapply(imputed_data_framenumeric, function(x) sum(is.na(x)))
boxplot(imputed_data_framenumeric, col = "blue")

#Concatenating pre-processed data
# Combine dataframes horizontally
df <- cbind(imputed_data_framenumeric,imputed_data_framecat)
str(df)





pie_data <- df %>%
  group_by(Year, Alive_Dead) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
# Define custom colors
custom_colors <- c("Dead" = "coral", "Alive" = "blue")

# Plot pie charts
pie_charts <- list()
for (year in unique(df$Year)) {
  plot_data <- pie_data %>%
    filter(Year == year)
  
  pie_charts[[year]] <- ggplot(plot_data, aes(x = "", y = percent, fill = Alive_Dead)) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    labs(title = paste("Pie Chart for Year:", year)) +
    scale_fill_discrete(labels = c("Dead", "Alive")) + scale_fill_manual(values = custom_colors) +  # Set custom colors # Customize the legend labels
    theme_void() +
    theme(legend.position = "bottom")
}

# Arrange the pie charts
gridExtra::grid.arrange(grobs = pie_charts, ncol = 2)


#Convert categorical variables to factors
df$Cancer<- as.numeric(factor(df$Cancer))
df$Year<-as.numeric(factor(df$Year))
df$Mode_Presentation<-as.numeric(factor(df$Mode_Presentation))
df$Gender<-as.numeric(factor(df$Gender))
df$HCC_BCLC_Stage<-as.numeric(factor(df$HCC_BCLC_Stage))
df$Treatment_grps<-as.numeric(factor(df$Treatment_grps))
df$Alive_Dead<-as.numeric(factor(df$Alive_Dead))
df$Prev_known_cirrhosis<-as.numeric(factor(df$Prev_known_cirrhosis))
summary(df)



#Scatter Plot to Relationship between Age and Size by Cancer Status
# Create a scatter plot
ggplot(data = df, aes(x = Age, y = Size, color = Cancer)) +
  geom_point() +
  labs(title = "Relationship between Age and Size by Cancer Status",
       x = "Age", y = "Size") +
  theme_minimal()

# Create a grouped bar plot with different colors for Mode_Presentation and Treatment_grps
ggplot(data = df, aes(x = Month, fill = Mode_Presentation)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Treatment_grps) +
  labs(title = "Relationship between Month, Mode of Presentation, and Treatment Groups",
       x = "Month", y = "Count") +
  theme_minimal() +
  theme(legend.position = "top")

boxplot(df, col = "blue")


#Correlation Analysis

cor_matrix<-cor(df)
corrplot(cor_matrix,tl.pos ='lt', tl.cex=0.55,method='circle')
#Feature Engineering

# Prepare the survival data
surv_data <- with(df, Surv(Survival_fromMDM ,Alive_Dead))

#Question 1
#to study the long-term impact of COVID on patients with liver cancer. 
#Survival analysis on Pandemic Data
#surv_dataPandemic<- with(data_pandemic, Surv(Survival_fromMDM ,Alive_Dead))
# Fit a Cox proportional hazards regression model
cox_model <- coxph(surv_data ~ Month + Year + Age + Size + PS + Cancer + Mode_Presentation + Gender + HCC_BCLC_Stage + Treatment_grps + Prev_known_cirrhosis, data = df)

# Print the summary of the Cox model
summary(cox_model)
survival_curve <- survfit(cox_model)

# Plot the survival curve
ggplot(survival_curve, aes(time, surv)) +
  geom_step() +
  labs(title = "Survival Curve",
       x = "Time", y = "Survival Probability") +
  theme_minimal()


# Assuming you have the 'cox_model' fitted already

# Create a summary of significant variables
significant_vars <- c("Size","Month", "PS", "Cancer", "HCC_BCLC_Stage", "Treatment_grps")

# Create a data frame to store results
results <- data.frame(Variable = character(0), Coefficient = numeric(0), Hazard_Ratio = numeric(0))

# Loop through significant variables and extract information
for (var in significant_vars) {
  coef_value <- coef(cox_model)[var]
  hazard_ratio <- exp(coef_value)
  results <- rbind(results, data.frame(Variable = var, Coefficient = coef_value, Hazard_Ratio = hazard_ratio))
}

library(ggplot2)

# Define custom blue color variations
blue_colors <- c("#1f77b4", "#377eb8", "#6baed6", "#9ecae1", "#c6dbef", "#deebf7")

# Create bar plot to visualize hazard ratios
ggplot(results, aes(x = Variable, y = Hazard_Ratio, fill = Variable)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = blue_colors) +  # Set custom blue colors
  labs(title = "Impact of Significant Variables on Hazard Ratio",
       x = "Variable", y = "Hazard Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Model Validation
# Predict survival probabilities
survival_prob <- survfit(cox_model)

# Plot actual vs. predicted survival curves with different colors
plot(survival_prob, xlab = "Time", ylab = "Survival Probability",
     main = "Estimated  vs. Predicted Survival Curves", col = c("blue", "red"))

# Add legend
legend("topright", legend = c("Estimatd", "Predicted"), col = c("blue", "red"), lwd = 1)





# Fit the Kaplan-Meier estimator
surv_fit <- survfit(surv_data ~ 1)

# Create a data frame for the survival probabilities
surv_df <- data.frame(time = surv_fit$time, survival = surv_fit$surv)

# Plot the Kaplan-Meier survival curve using ggplot2
ggplot(data = surv_df, aes(x = time, y = survival)) +
  geom_step() +
  labs(x = "Time", y = "Survival Probability", title = "Kaplan-Meier Survival Curve") +
  theme_minimal()


#survival curve for first question
# Define custom blue color
blue_color <- "#1f77b4"
  
# Plot the survival curve using ggplot2 with custom blue color
ggplot(surv_df, aes(x = time, y = survival)) +
  geom_step(color = blue_color) +  # Set the line color to blue
  labs(x = "Time", y = "Survival Probability", title = "Survival Curve") +
  theme_minimal()

# Define the survival object
survival_object <- Surv(time = df$Survival_fromMDM, event = df$Alive_Dead)

# Create Kaplan-Meier survival curves for different factors
# Define custom blue color variations
blue_colors <- c("#1f77b4", "#FF6B6B")

# Example: Year
km_year <- survfit(survival_object ~ Year, data = df)
ggsurvplot(km_year, data = df, title = "Kaplan-Meier Survival Curves by Year",palette = blue_colors) 
# Define custom blue color variations

# Example: Cancer
km_cancer <- survfit(survival_object ~ Cancer, data = df)
ggsurvplot(km_cancer, data = df, title = "Kaplan-Meier Survival Curves by Cancer Status")


# Example: Mode_Presentation
km_bleed <- survfit(survival_object ~ Mode_Presentation, data = df)
ggsurvplot(km_bleed, data = df, title = "Kaplan-Meier Survival Curves by Mode of Presentation")

# Example: Gender
km_bleed <- survfit(survival_object ~ Gender, data = df)
ggsurvplot(km_bleed, data = df, title = "Kaplan-Meier Survival Curves by Gender")


# Example: Cirrhosis
km_bleed <- survfit(survival_object ~ Cirrhosis, data = df)
ggsurvplot(km_bleed, data = df, title = "Kaplan-Meier Survival Curves by Cirrhosis")




#Log Rank Test for checking statistical significant

# Example: Log-rank test for Year
logrank_test_result_year <- survdiff(survival_object ~ df$Year)
print(logrank_test_result_year)

# Log-rank test for other categorical variables (e.g., Cancer, Bleed)

# Example: Log-rank test for Cancer
logrank_test_result_cancer <- survdiff(survival_object ~ df$Cancer)
print(logrank_test_result_cancer)




# Example: Log-rank test for Mode_Presentation
logrank_test_result_Mode_Presentation <- survdiff(survival_object ~ df$Mode_Presentation)
print(logrank_test_result_Mode_Presentation)


# Example: Log-rank test for Treatment_grps 
logrank_test_result_Treatment_grps  <- survdiff(survival_object ~ df$Treatment_grps)
print(logrank_test_result_Treatment_grps)




#2.Study of  how different patient characteristics and circumstances linked to these patients' survival results during the pandemic and pre-pandemic periods.


#Cox proportional hazards regression model with interaction terms


# Fit a Cox proportional hazards regression model with interaction terms
cox_model_interaction <- coxph(surv_data ~ Year*Age + Year*Size + Year*Cancer + Year*HCC_BCLC_Stage + Year*PS + Year*Treatment_grps + Year*Mode_Presentation + Year*Prev_known_cirrhosis + Year*Gender, data = df)

# Print the summary of the model with interaction terms
summary(cox_model_interaction)

# Load the required packages
library(survival)

# Prepare the survival data
surv_data <- with(df, Surv(Survival_fromMDM, Alive_Dead))

# Fit a Cox proportional hazards regression model with interaction terms
cox_model_interaction <- coxph(surv_data ~ Year*Age + Year*Size + Year*Cancer + Year*HCC_BCLC_Stage + Year*PS + Year*Treatment_grps + Year*Mode_Presentation + Year*Prev_known_cirrhosis + Year*Gender, data = df)

# Create a survival curve object
survival_curve <- survfit(cox_model_interaction, newdata = df)

# Calculate the cumulative hazard
cumulative_hazard <- -log(survival_curve$surv)

# Calculate the average cumulative hazard
average_cumulative_hazard <- rowMeans(cumulative_hazard)

# Convert average cumulative hazard back to survival probabilities
average_survival_prob <- exp(-average_cumulative_hazard)

# Create a data frame for plotting
average_data <- data.frame(time = survival_curve$time, surv = average_survival_prob)

# Plot the average survival curve with interaction terms
plot(average_data$time, average_data$surv, type = "l", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Survival Probability",
     main = " Survival Curve For COX Model with Interaction Terms")






# Create a data frame with the covariate values you want to plot
newdata <- df  # Replace this with appropriate data if necessary

# Predict the hazard ratio for each individual using the coxph model
predicted_hazards <- predict(cox_model_interaction, newdata, type = "risk")

# Convert predicted hazard ratios to survival probabilities
predicted_survival <- exp(-predicted_hazards)

# Add the predicted survival probabilities to the newdata data frame
newdata$Predicted_Survival <- predicted_survival

# Calculate average predicted survival probabilities for each time point
avg_survival <- aggregate(Predicted_Survival ~ Survival_fromMDM + Year, data = newdata, FUN = mean)

# Plot the survival curves using ggplot2
ggplot(newdata, aes(x = Survival_fromMDM, y = Predicted_Survival, color = as.factor(Year))) +
  geom_step() +
  geom_smooth(data = avg_survival, aes(x = Survival_fromMDM, y = Predicted_Survival), 
              method = "loess", se = FALSE, linetype = "dashed", size = 1) +  # Add average survival curve
  labs(x = "Time", y = "Predicted Survival Probability", color = "Year") +
  scale_color_manual(values = c("1" = "blue", "2" = "coral")) +
  theme_minimal()






# Tidy up the Cox model summary using broom
tidy_summary <- tidy(cox_model_interaction, exponentiate = TRUE)

# Calculate confidence intervals
tidy_summary$lower_ci <- tidy_summary$estimate - 1.96 * tidy_summary$std.error
tidy_summary$upper_ci <- tidy_summary$estimate + 1.96 * tidy_summary$std.error

# Plot the dot plot using ggplot2
ggplot(tidy_summary, aes(x = reorder(term, p.value), y = p.value)) +
  geom_point(size = 3, color = "blue") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = "Covariate", y = "p-value", title = "Dot Plot of p-values") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))






# Tidy up the Cox model summary using broom
tidy_summary <- tidy(cox_model_interaction, exponentiate = TRUE)

# Calculate confidence intervals
tidy_summary$lower_ci <- tidy_summary$estimate - 1.96 * tidy_summary$std.error
tidy_summary$upper_ci <- tidy_summary$estimate + 1.96 * tidy_summary$std.error

# Reorder terms for a clear forest plot
tidy_summary$term <- factor(tidy_summary$term, levels = tidy_summary$term[order(tidy_summary$p.value)])

# Create a forest plot using ggplot2
ggplot(tidy_summary, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  geom_point(aes(x = lower_ci), size = 3, color = "blue") +
  geom_point(aes(x = upper_ci), size = 3, color = "coral") +
  geom_segment(aes(x = lower_ci, xend = upper_ci, yend = term), color = "blue") +
  coord_flip() +
  labs(x = "Hazard Ratio", y = "Covariate", title = "Forest Plot of Hazard Ratios") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))

