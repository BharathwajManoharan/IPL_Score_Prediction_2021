## IPL First Inning Score Prediction
> This project is about predicting the score of the batting first team in IPL 2021. It's a web application built using Flask, a popular Python-based web framework, and machine learning algorithms.

## Contributions:
* [Bharathwaj](https://github.com/BharathwajManoharan)
* [Hariharan](https://github.com/hariharan0509)
  
## Technologies and Tools
* Python (numpy,scipy,scilit-learn,matplotlib,pandas)
* Flask,Request,Werkzeug,gunicorn
* Jinja2 (Template Engine)

## Dataset Preparation

*	Gathered data on the teams, players, and matches of IPL 2021 from various sources, including scraping websites that provide IPL data.
*	Preprocessed the dataset by cleaning the data, handling missing values, and encoding categorical variables.
*	Used exploratory data analysis (EDA) to find patterns and trends in the data.
*	Performed feature engineering to extract relevant features for our model, such as team rankings, batting averages, and home ground advantage.

## Model Building and Tuning

*	Used machine learning algorithms to build our model, including linear regression, decision trees, and neural networks.
*	Trained the model on the IPL 2021 dataset and tested it to ensure accuracy.
*	Experimented with different hyperparameters, such as learning rate, regularization, and number of hidden layers, to optimize the model's performance.
*	Evaluated the model's performance using metrics such as mean absolute error (MAE), mean squared error (MSE), and R-squared.

## Building Flask API

*	Created a Flask application that uses the trained machine learning model to predict the score of the batting first team.
*	Created a simple user interface where users can input the team name and the number of overs played, and get the predicted score.
*	Used JSON to exchange data between the frontend and the backend of the web application.
*	Deployed the application on a local server to test it before deploying it to a cloud hosting platform.

## Pushing Code to Github

*	Created a Github repository and added our code, including the dataset, model, and Flask application.
*	Added documentation and instructions on how to run the application.
*	Used Git to version control our code and track changes over time.
*	Collaborated with team members by creating branches, merging changes, and resolving conflicts.

## Connecting to PythonAnywhere

*	Deployed our Flask application on PythonAnywhere, a cloud hosting platform for web applications.
*	Configured the environment variables and installed the necessary dependencies.
*	Connected our Github repository to PythonAnywhere and set up automatic deployments.
*	Used the PythonAnywhere console and file editor to manage the application and troubleshoot issues.
*	Made the application available to anyone with an internet connection, and monitored the application's performance and usage using logs and metrics.

## Code Snipet

````
# Creating feature matrix X and target vector y
X = df2.drop(columns="final_total_runs") # 38 features
y = df2.final_total_runs

# Splitting the data into train and test parts
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

# Scaling the data using MinMaxScaler
from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

# Defining the algorithms to be used for regression
algo3 = {
    "Random Forest": {
        "model": RandomForestRegressor(),
        "params": {
            "n_estimators": [100, 200, 300],
            "criterion": ["mse", "friedman_mse"],
            "max_depth": [1, 3, 5, 7, 9, 10, 11, 12, 14, 15, 18, 20, 25, 28, 30, 33, 38, 40],
            "min_samples_split": [2, 4, 6, 8, 10, 15, 20],
            "min_samples_leaf": [i for i in range(1, 11)],
            "max_leaf_nodes": [None] + [i for i in range(10, 91, 10)],
            "max_features": ["auto", "log2", "sqrt", None]
        }
    }
}

# Finding the best model using RandomizedSearchCV
from sklearn.model_selection import RandomizedSearchCV
best_model = {}
for model_name, values in algo3.items():
    rscv = RandomizedSearchCV(values["model"], values["params"], cv=5, n_iter=15, n_jobs=-1, verbose=2, random_state=4)
    rscv.fit(X_train, y_train)
    best_model[model_name] = rscv

# Calculating error using MSE, RMSE and MAE for the best model
from sklearn.metrics import mean_absolute_error, mean_squared_error
for model_name, model in best_model.items():
    y_pred_train = model.predict(X_train)
    y_pred_test = model.predict(X_test)
    train_mae = mean_absolute_error(y_train, y_pred_train)
    train_mse = mean_squared_error(y_train, y_pred_train)
    train_rmse = np.sqrt(train_mse)
    test_mae = mean_absolute_error(y_test, y_pred_test)
    test_mse = mean_squared_error(y_test, y_pred_test)
    test_rmse = np.sqrt(test_mse)

````
## Status
Project is: _finished_.
