# Decision-Tree Body Fat Calculator

An interpretable statistical modeling project that turns anthropometric measurements into an interactive **Shiny body-fat estimator**.

The workflow starts from a dataset of **252 men** with measured body-fat percentage and body/circumference variables, compares multiple regression approaches, reduces the predictor set to a small group of informative measurements, and deploys the final two-variable decision-tree model as a browser-based calculator.

## Project pipeline

```text
raw anthropometric data
        |
        v
cleaning + outlier handling
        |
        v
correlation / multicollinearity analysis
        |
        v
model comparison
PCR | Random Forest | Decision Tree | Linear | Lasso
        |
        v
parsimonious predictor selection
        |
        v
Shiny calculator using abdomen + chest circumference
```

## Modeling approach

The analysis compares five regression methods on an 80/20 train-test split:

- Principal Component Regression (PCR)
- Random Forest
- Decision Tree
- Linear Regression
- Lasso Regression

Models are evaluated with **MAE, MSE, and R²**.

Variable-selection analysis identifies **abdomen, adiposity, and chest measurements** as among the strongest predictors of body-fat percentage. The deployed Shiny calculator intentionally uses only **abdomen and chest circumference**, trading some model complexity for a much simpler user input experience.

## Interactive calculator

`code/shinyApp.R` trains the decision-tree model from the included data and launches an interactive app where a user enters:

- abdomen circumference (cm)
- chest circumference (cm)

The app returns the model's estimated body-fat percentage immediately.

Run it from the repository root with:

```r
shiny::runApp("code")
```

## Reproduce the analysis

Install the required R packages:

```r
install.packages(c(
  "pls", "randomForest", "rpart", "glmnet", "Metrics",
  "rpart.plot", "ggplot2", "tidyr", "reshape2", "car", "shiny", "dplyr"
))
```

Then run:

```bash
Rscript code/group8.R
Rscript code/variableSelection.R
```

The scripts support execution from the repository root or from inside `code/`.

## Repository structure

- `data/BodyFat.csv` — original 252-observation dataset
- `data/cleaned_data.csv` — cleaned dataset retained as a reference artifact
- `code/group8.R` — cleaning, model comparison, evaluation, and visualization
- `code/variableSelection.R` — correlation analysis and multicollinearity diagnostics
- `code/shinyApp.R` — interactive two-variable decision-tree calculator
- `image/` — generated analysis figures
- `STAT628 Module 2.pdf` — project report
- `STAT628 Module 2.pptx` — presentation deck

## Why the project is useful

The project demonstrates more than fitting a single predictive model. It connects **data cleaning, feature selection, comparative modeling, interpretability, and deployment** into one compact statistical workflow, then turns the result into a usable interactive application.

## Scope

This calculator is a statistical estimation prototype built from the project dataset. It is intended to demonstrate predictive modeling and interactive deployment rather than serve as a clinical body-composition measurement.

Developed as a UW–Madison STAT 628 team project.
