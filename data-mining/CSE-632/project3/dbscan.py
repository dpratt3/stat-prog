import pandas as pd
from sklearn.cluster import DBSCAN
from sklearn.neighbors import NearestNeighbors
import numpy as np
from joblib import Parallel, delayed

data = pd.read_csv("sparse_pca_transformed_data.csv")
data = data.iloc[:, :10]

std_deviation = data.std().mean()

print(std_deviation)

std_dev_mult = np.arange(0.1, 5, 0.1)
min_samp_mult = np.arange(1, 30, 1)

def perform_dbscan(row, col, std_dev_mult, std_deviation, min_samp_mult, data):
    dbscan_result = DBSCAN(eps=std_dev_mult[row] * std_deviation, min_samples=min_samp_mult[col], metric="manhattan").fit(data)
    labels = dbscan_result.labels_
    
    # Count the number of clusters excluding noise (-1)
    num_clusters = len(set(labels)) - (1 if -1 in labels else 0)
    return num_clusters


df = pd.DataFrame(index=std_dev_mult, columns=min_samp_mult)

# Define a function to run DBSCAN in parallel
results = Parallel(n_jobs=-1)(delayed(perform_dbscan)(row, col, std_dev_mult, std_deviation, min_samp_mult, data) 
                              for row in range(len(std_dev_mult)) 
                              for col in range(len(min_samp_mult)))

# Populate the DataFrame with the results
for col in range(len(min_samp_mult)):
    for row in range(len(std_dev_mult)):
        df.loc[std_dev_mult[row], min_samp_mult[col]] = results[row * len(min_samp_mult) + col]

print(df)

# # Retrieve labels
# labels = clustering.labels_

# # Print the clusters
# print("Cluster labels:\n", labels)

# n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
# print("\nNumber of clusters:", n_clusters_)

