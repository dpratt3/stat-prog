import pandas as pd
from sklearn.cluster import DBSCAN
from sklearn.neighbors import NearestNeighbors
import numpy as np
from scipy import ndimage
from joblib import Parallel, delayed
from sklearn.metrics import silhouette_score

data = pd.read_csv("sparse_pca_transformed_data.csv")
data = data.iloc[:, :10]

std_deviation = data.std().mean()

print(std_deviation)

std_dev_mult = np.arange(4.8, 10.8, 0.1)
min_samp_mult = np.arange(4, 8, 1)

def perform_dbscan(row, col, std_dev_mult, std_deviation, min_samp_mult, data):
    dbscan_result = DBSCAN(eps=std_dev_mult[row] * std_deviation, min_samples=min_samp_mult[col], metric="euclidean").fit(data)
    labels = dbscan_result.labels_
    
    # Count the number of clusters excluding noise (-1)
    num_clusters = len(set(labels)) - (1 if -1 in labels else 0)
    return num_clusters
    
    # unique_labels = set(labels)
    # if len(unique_labels) == 1 and -1 in unique_labels:
    #     # Handle case where there are no actual clusters
    #     return -1  # Or any other value you want to return in this case
    
    # silhouette = silhouette_score(data, labels)
    # return silhouette

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

# Find the largest island

# Convert DataFrame to NumPy array with counts and values
matrix = df.to_numpy()
unique_vals, counts = np.unique(matrix, return_counts=True)
unique_val_count = pd.Series(counts, index=unique_vals, name='Counts')
unique_val_count_df = pd.DataFrame(unique_val_count)
unique_val_count_df_sorted = unique_val_count_df.sort_values(by='Counts', ascending=False)

# values of zero and one 
unique_val_count_df_filtered = unique_val_count_df_sorted[~unique_val_count_df_sorted.index.isin([0, 1]) & (unique_val_count_df_sorted['Counts'] > 4) ]

binary_matrix = np.where(matrix == 3, 1, 0)

print(binary_matrix)

# Label connected components
labeled_matrix, num_features = ndimage.label(binary_matrix)

# Count the sizes of the different islands
sizes = [np.sum(labeled_matrix == i) for i in range(1, num_features+1)]

# Find the size of the largest island
largest_island_size = max(sizes)
print("Size of the largest island:", largest_island_size)

df.to_csv("grid_search.csv", index = True)

# Plug in optimal values
optimal_cluster = DBSCAN(eps=2.8 * std_deviation, min_samples=14, metric="euclidean").fit(data)
optimal_labels = optimal_cluster.labels_
np.unique(optimal_labels) # inspect labels: -1, 0, 1

# Align labels with response variable to assess fit of gaussian mixture model
train = pd.read_csv("~/Downloads/QSAR-data/OX2_training_disguised.csv")
test = pd.read_csv("~/Downloads/QSAR-data/OX2_test_disguised.csv")

train_cols = set(train.columns)
test_cols = set(test.columns)

if train_cols == test_cols:
    print("Train and test sets have the same columns.")
else:
    print("Train and test sets have different columns.")

df = pd.concat([train, test], ignore_index=True, axis=0)

# Label the data:
labeled_response = pd.DataFrame({'Act': df['Act'].values}, index=optimal_labels)
labeled_response.to_csv("labeled_reponse.csv", index = True)

