# Modeling-Patient-Hospital-Choice-for-Respiratory-Diseases-in-Austin-TX
Modeling patient hospital choice to understand if predictors changed before and during the pandemic and guage the ability to determine what hospital patients will go to. 

# Hospital Choice Model Comparison

This project aims to compare the performance of predictive models for patient hospital choice, focusing on hospitals in Austin, TX. The models tested include Random Forest and Multinomial Logistic Regression.

## Introduction

The provided R code implements functions for data preparation, model training, evaluation, and visualization. The goal is to analyze the effectiveness of different algorithms in predicting patient hospital choice based on various features such as race, socioeconomic vulnerability index, drive time, and patient age.

## Usage

1. **Data Preparation**: The code reads the input data from a CSV file and prepares it for modeling. Make sure to have the necessary libraries installed.

2. **Run Models**: Execute the provided functions to run Random Forest and Multinomial Logistic Regression models. The `perform_cross_validation` function performs cross-validation and evaluates model performance.

3. **Plot Results**: Use the generated plots to visualize model accuracy, precision, and other performance metrics. The code includes plots for comparing model accuracies across cross-validation folds and visualizing precision by class and model.

## Contributing

Contributions to the project are welcome! Feel free to submit pull requests or open issues for any suggestions or improvements.

## License

This project is licensed under the [MIT License](LICENSE).
