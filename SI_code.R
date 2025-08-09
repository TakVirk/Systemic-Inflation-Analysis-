
## EDA Visuals 

# Summary Statistics 

# CRP Distribution 

ggplot(Sarc_df, aes(x = CRP_Level)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of CRP Levels", x = "CRP (mg/L)", y = "Count")+ theme_bw()

# check log transformed data 

ggplot(Sarc_df, aes(x = log_CRP)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(
    title = "Distribution of Log-Transformed CRP",
    x = "log(CRP Level + 0.1)",
    y = "Count"
  ) + theme_bw()

## 2. boxplots 

# Set layout: 2 rows x 2 columns
library(gridExtra)

p1 <- ggplot(Sarc_df, aes(x = Gender, y = log_CRP)) +
  geom_boxplot(fill = "lightblue") + ggtitle("CRP by Gender") + theme_bw()

p2 <- ggplot(Sarc_df, aes(x = Smoker, y = log_CRP)) +
  geom_boxplot(fill = "salmon") + ggtitle("CRP by Smoking") + theme_bw()

p3 <- ggplot(Sarc_df, aes(x = Diabetes, y = log_CRP)) +
  geom_boxplot(fill = "khaki") + ggtitle("CRP by Diabetes") + theme_bw()

p4 <- ggplot(Sarc_df, aes(x = Race, y = log_CRP)) +
  geom_boxplot(fill = "orchid") +
  ggtitle("CRP by Race") +
  labs(x = "Race", y = "log_CRP") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10 * 0.7)  # 0.7 relative to default 10
  )

grid.arrange(p1, p2, p3, p4, ncol = 2)
par(mfrow = c(1, 1))  # Go back to default layout

# 3. Corr plot 

library(corrplot)

# Select only numeric predictors (excluding ID and CRP variables)
num_vars <- Sarc_df[, c("Age", "BMI", "WaistCircumference",
                        "ProteinIntake", "CalorieIntake",
                        "ModeratePA_Minutes", "VigorousPA_Minutes",
                        "SedentaryMinutes")]

# Correlation matrix
cor_matrix <- cor(num_vars, use = "complete.obs")

# Plot
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.8)

## 4. variable summary table

install.packages("kableExtra")
library(knitr)
library(kableExtra)

# Create a data frame with variable summaries
var_summary <- data.frame(
  Variable = c("log_CRP", "Age", "Gender", "Race", "IncomeRatio", "BMI",
               "WaistCircumference", "ProteinIntake", "CalorieIntake",
               "ModeratePA_Minutes", "VigorousPA_Minutes", "SedentaryMinutes",
               "Smoker", "Diabetes"),
  Type = c("Continuous", "Continuous", "Categorical", "Categorical", "Continuous", "Continuous",
           "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous",
           "Categorical", "Categorical"),
  Description = c("Log-transformed C-reactive protein (CRP)",
                  "Participant's age",
                  "Biological sex",
                  "Self-identified race/ethnicity",
                  "Family income-to-poverty ratio",
                  "Body Mass Index",
                  "Waist circumference",
                  "Daily protein intake (24-hr recall)",
                  "Daily calorie intake (24-hr recall)",
                  "Moderate physical activity (weekly)",
                  "Vigorous physical activity (weekly)",
                  "Sedentary behavior (daily)",
                  "Smoked ≥100 cigarettes in life",
                  "Diagnosed with diabetes"),
  Units = c("log(mg/L)", "Years", "Male / Female", "6 race categories",
                      "Ratio", "kg/m²", "cm", "Grams", "kcal", "Minutes/week",
                      "Minutes/week", "Minutes/day", "Yes / No", "Yes / No / Borderline")
)

# Create a styled table
kable(var_summary, "html", caption = "Summary of Variables Used in Analysis") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "center") %>%
  column_spec(1:4, width = "7em")  # Optional: adjust column width

## Analysis 

#### 1. lr model 

Sarc_df$Gender <- factor(Sarc_df$Gender)
Sarc_df$Race <- factor(Sarc_df$Race)
Sarc_df$Smoker <- factor(Sarc_df$Smoker)
Sarc_df$Diabetes <- factor(Sarc_df$Diabetes)

model <- lm(log_CRP ~ Age + Gender + Race + IncomeRatio +
              Weight + Height + ProteinIntake + CalorieIntake +
              ModeratePA_Minutes + VigorousPA_Minutes + SedentaryMinutes +
              Smoker + Diabetes,
            data = Sarc_df)

## Visual 

install.packages("broom")
library(broom)
library(ggplot2)

model_tidy <- broom::tidy(model, conf.int = TRUE)

# Optional: clean up labels
model_tidy$term <- gsub("Gender", "Gender: ", model_tidy$term)
model_tidy$term <- gsub("Race", "Race: ", model_tidy$term)
model_tidy$term <- gsub("Smoker", "Smoker: ", model_tidy$term)
model_tidy$term <- gsub("Diabetes", "Diabetes: ", model_tidy$term)

ggplot(model_tidy[-1, ], aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  labs(
    title = "Predictors of log(CRP)",
    x = "Estimated Effect (β)",
    y = "Predictor"
  ) +
  theme_bw()

# model assumptions 
# first oiginal lm model 

model_raw <- lm(CRP_Level ~ Age + Gender + Race + IncomeRatio +
                  Weight + Height + ProteinIntake + CalorieIntake +
                  ModeratePA_Minutes + VigorousPA_Minutes + SedentaryMinutes +
                  Smoker + Diabetes,
                data = Sarc_df)

# 2x2 plot layout
par(mfrow = c(2, 2))
# Base R diagnostic plots
plot(model_raw)
# Reset layout
par(mfrow = c(1, 1))

# new model 
model_log <- lm(log_CRP ~ Age + Gender + Race + IncomeRatio +
                  Weight + Height + ProteinIntake + CalorieIntake +
                  ModeratePA_Minutes + VigorousPA_Minutes + SedentaryMinutes +
                  Smoker + Diabetes,
                data = Sarc_df)

# Set 2x2 layout
par(mfrow = c(2, 2))
# Plot diagnostics
plot(model_log)
# Reset layout
par(mfrow = c(1, 1))

# Random Forest 
install.packages("randomForest")
library(randomForest)

rf_model <- randomForest(log_CRP ~ Age + Gender + Race + IncomeRatio +
                           Weight + Height + ProteinIntake + CalorieIntake +
                           ModeratePA_Minutes + VigorousPA_Minutes + SedentaryMinutes +
                           Smoker + Diabetes,
                         data = Sarc_df,
                         ntree = 500,
                         importance = TRUE)
importance(rf_model)

varImpPlot(rf_model,
           main = "Random Forest Variable Importance",
           type = 1,            # Mean decrease in accuracy
           pch = 19,
           col = "red")








