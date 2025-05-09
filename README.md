# Shark Tank Prediction

### Overview
This project analyzes historical data from the TV show "Shark Tank" (first 6 seasons, 495 pitches) to predict whether a startup will successfully secure a deal with an investor ("shark"). The analysis involves comprehensive exploratory data analysis (EDA), feature engineering (including text analysis of pitch descriptions and clustering of business categories), and the application of various classification machine learning models in R to identify key predictors of funding success.

### Directory layout

   ```bash
.
├── README.md
├── data
│   └── shark_tank.csv
├── dictionaries
│   ├── Hedonometer.csv               
│   └── Loughran-McDonald_MasterDictionary_1993-2023.csv   
├── R                                          
│   └── Shark Tank Prediction.R               
├── reports
│   └── Shark Tank Prediction Report.pdf
└── docs
    └── Shark tank dictionary.xlsx
 ```

### Problem Statement
Securing investment is a critical hurdle for startups. On "Shark Tank," entrepreneurs pitch their businesses to a panel of investors. Understanding the factors that influence whether a pitch results in a deal can provide valuable insights for entrepreneurs preparing their pitches and for understanding investor decision-making. This project aims to build a predictive model to identify these factors and forecast deal outcomes.

### Data Sources
The dataset was collected from Kaggle, covering 495 pitches across the first 6 seasons of Shark Tank. It initially contained 19 variables, including:
*   **Business Info:** `title`, `description`, `category`, `location`, `website`
*   **Outcome:** `deal` (True/False - target variable)
*   **Entrepreneur Info:** `entrepreneurs`, `Multiple.Entreprenuers`
*   **Financials:** `askedFor`, `exchangeForStake`, `valuation`
*   **Show Metadata:** `episode`, `season`, `episode-season`
*   **Sharks Present:** `shark1` through `shark5`

### Methodology / Approach
1.  **Data Cleaning & EDA:**
    *   Handled missing values (e.g., `website`, `entrepreneurs` were initially excluded or imputed where appropriate).
    *   Conducted extensive EDA on deal outcomes, location (grouped into Census Divisions), business categories, seasonality, financial aspects, and shark participation/success rates. Visualizations heavily used ggplot2.
2.  **Feature Engineering:**
    *   **Location & Season:** Grouped states into Census Divisions. Explored interaction effects (e.g., location & season).
    *   **Category Clustering:** Due to high dimensionality of `category`, categories were grouped using Hierarchical Clustering and PCA based on engineered metrics (success rate, pitch frequency, regional distribution, shark involvement, financial averages). Hierarchical clustering groups were chosen for the final model.
    *   **Text Analysis (Description):** Performed text preprocessing (lowercasing, punctuation/stopword removal, lemmatization). Extracted `Word_Count` and sentiment valence scores (`Happiness_Valence`, `Morality_Valence`, `Sociability_Valence`) using NRC and Hedonometer lexicons.
    *   **Shark Information:** Differentiated main vs. guest sharks. Shark names were converted into dummy variables. Some sharks were removed to address multicollinearity (e.g., Kevin Harrington due to Mark Cuban's entry).
    *   **Interaction Terms:** Identified significant interaction terms (e.g., `cluster_category * location_West_Pacific`) using Gini importance from an initial Random Forest.
3.  **Data Preparation for Modeling:**
    *   Applied one-hot encoding to categorical variables.
    *   Used min-max standardization for correlation analysis and as input for some models.
    *   Split data into 80% training and 20% testing sets using stratified sampling.
4.  **Model Selection & Evaluation:**
    *   **Baseline:** Logistic Regression.
    *   **Advanced Models:** Classification Trees, Random Forest, XGBoost.
    *   **Hyperparameter Tuning:** Employed 10-fold cross-validation and Grid Search.
    *   **Feature Selection:** Used Gini importance from Random Forest to select a subset of impactful features.
    *   **Metrics:** Accuracy, Precision, Recall, AUC.
    *   **Final Model:** XGBoost with selected features and interaction terms.

### Key Results & Visualizations
*   **Best Model:** XGBoost (with interaction terms: `cluster_2*location_West Pacific`, `cluster_3*location_West Pacific`).
    *   Test Set Accuracy: 56.12%
    *   Test Set Precision: 58.00%
    *   Test Set Recall: 56.86%
    *   Test Set AUC: (Obtain from your pROC results for XGBoost)
*   **Top Gini Importance Features:** `Happiness_Valence`, `Word_Count`, `askedFor`, `exchangeForStake`, `Sociability_Valence`, `Morality_Valence`, `Multiple.Entreprenuers`, `location_West Pacific`, `cluster_2`, `cluster_3`.
*   **Key EDA Findings:**
    *   Specialty Food is a leading category. Pacific West is a highly represented region.
    *   Season 2 showed high success despite fewer pitches.
    *   Shark success rates are relatively similar despite varying participation.
    *   Positive correlation between `valuation` and `askedFor`.
*   Visualizations: Deal distributions by location/category/season, shark performance metrics, dendrograms for category clustering, PCA biplots, correlation heatmaps, feature importance plots. (Refer to `SharkTank_Report.pdf` and code outputs for figures).

### Key Learnings & Challenges
*   **Feature Engineering is Key:** Grouping high-cardinality categorical variables (like `category` and `location`) and extracting insights from text (`description`) were crucial for model performance.
*   **Model Complexity vs. Performance:** While Random Forest showed near-perfect training accuracy (overfitting), XGBoost provided better generalization to the test set.
*   **Interpretability:** Selected features and models (even complex ones like XGBoost can provide feature importance) aimed to offer actionable insights for entrepreneurs.
*   **Data Limitations:** The dataset covers early seasons; trends might evolve. Limited sample size for some specific categories or shark appearances.

### Next Steps / Future Enhancements
*   Incorporate more recent seasons of Shark Tank to capture evolving trends.
*   Expand text analysis to include topic modeling or more advanced sentiment features.
*   Explore external data, such as economic indicators or social media buzz related to pitches/products.
*   Investigate specific shark-entrepreneur dynamics or negotiation patterns if more granular data were available.
