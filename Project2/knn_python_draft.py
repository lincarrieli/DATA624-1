import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

from sklearn import neighbors as KNN
from sklearn.model_selection import train_test_split as tts

df = pd.read_csv("https://raw.githubusercontent.com/dmoste/DATA620/main/knn_processed.csv")

# Load the data and target values into variables X and y
X = df.iloc[:,1:33].to_numpy()
y = df.iloc[:,0].to_numpy()

# Split the data in testing and training sets
X_train, X_test, y_train, y_test = tts(X, y, test_size = 0.25, random_state = 20)

# Setup containers to store MAPE values
neighbors = np.arange(1, 21)

mape_scores = pd.DataFrame(index = neighbors,
                           columns = ["uniform", "inverse", "inverse squared"])

# Define a function for inverse squared weights
def inverse_squared_weights(weights):
    return 1/(weights**2)

col = 0
  
# Loop over different weight options
for w in ["uniform", "distance", inverse_squared_weights]:
    
    # Loop over different values of k
    for k in neighbors:
        # Setup a k-NN Classifier with k neighbors
        knn = KNN.KNeighborsRegressor(n_neighbors = k,
                                      weights = w,
                                      metric = "manhattan",
                                      algorithm = "brute")
    
        # Fit the classifier to the training data
        knn.fit(X_train, y_train)
        y_pred = knn.predict(X_test)
        
        # Compute the MAPE score
        perc_error = np.empty(len(y_pred))
        for j in range(0,len(y_pred)):
            perc_error[j] = abs(y_test[j] - y_pred[j])/y_test[j]
            
        mape_scores.iloc[k-1, col] = perc_error.mean()*100
            
    col += 1

# Generate plot
plt.title('k-NN: Varying Number of Neighbors and Weights')
plt.plot(neighbors, mape_scores.iloc[:,0], label = "Uniform Weights")
plt.plot(neighbors, mape_scores.iloc[:,1], label = "Distance (Inverse) Weights")
plt.plot(neighbors, mape_scores.iloc[:,2], label = "Inverse Squared Weights")
plt.legend()
plt.xlabel('Number of Neighbors')
plt.ylabel('MAPE')
plt.xticks(range(1,21,2))
plt.show()

# Honing the k value by training on multiple splits

# Create container for MAPE scores
invertd_s_mape = pd.DataFrame(index = np.arange(1,201),
                              columns = ["k", "MAPE", "seed"])

row = 0

# Loop through different train/test splits
for i in range(20,30):
    # Split the data in testing and training sets
    X_train, X_test, y_train, y_test = tts(X, y, test_size = 0.25, random_state = i)

    # Loop over different values of k
    for k in neighbors:
        # Setup a k-NN Classifier with k neighbors
        knn = KNN.KNeighborsRegressor(n_neighbors = k,
                                      weights = inverse_squared_weights,
                                      metric = "manhattan",
                                      algorithm = "brute")
    
        # Fit the classifier to the training data
        knn.fit(X_train, y_train)
        y_pred = knn.predict(X_test)
        
        # Compute the MAPE score
        perc_error = np.empty(len(y_pred))
        for j in range(0,len(y_pred)):
            perc_error[j] = abs(y_test[j] - y_pred[j])/y_test[j]
        
        invertd_s_mape.iloc[row, 0] = k    
        invertd_s_mape.iloc[row, 1] = perc_error.mean()*100
        invertd_s_mape.iloc[row, 2] = i
        
        row += 1

# Aggregate data to find best k value and get the standard deviations
invertd_s_mape["MAPE"] = invertd_s_mape["MAPE"].astype(float)

mean = invertd_s_mape.groupby("k").mean().to_numpy().flatten()
sd = invertd_s_mape.groupby("k").std().to_numpy().flatten()
lb = np.empty(20)
ub = np.empty(20)

for i in range(0,len(mean)):
    lb[i] = mean[i] - sd[i]
    ub[i] = mean[i] + sd[i]

best_k = pd.DataFrame({"Mean": mean, "SD": sd, "LB": lb, "UB": ub},
                      columns = ["Mean", "SD", "LB", "UB"])

# Generate plot
plt.title('k-NN: Varying Number of Neighbors Aggregate')
plt.plot(neighbors, best_k.loc[:,"Mean"])
plt.fill_between(neighbors, best_k.loc[:,"LB"], best_k.loc[:,"UB"], color='b', alpha=.1)
plt.xlabel('Number of Neighbors')
plt.ylabel('MAPE')
plt.xticks(range(1,21,2))
plt.show()
