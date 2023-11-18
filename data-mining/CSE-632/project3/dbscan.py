import pandas as pd
from sklearn.cluster import DBSCAN

data = pd.read_csv("sparse_pca_transformed_data.csv")
std_deviation = data.std().mean()

print(std_deviation)

clustering = DBSCAN(eps= 0.5 * std_deviation, min_samples=10).fit(data)

# Retrieve labels
labels = clustering.labels_

# Print the clusters
print("Cluster labels:\n", labels)

n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
print("\nNumber of clusters:", n_clusters_)

print(clustering)

import matplotlib.pyplot as plt
import numpy as np

# Assuming 'data' is your dataset and 'labels' contains the cluster labels assigned by DBSCAN

labels = clustering.labels_

# Plot clusters (assuming you have at least 2 columns in your DataFrame)
plt.scatter(data.iloc[:, 0], data.iloc[:, 7], c=labels, cmap='viridis')
plt.title('DBSCAN Clustering')
plt.xlabel('Feature 1')
plt.ylabel('Feature 2')
plt.colorbar(label='Cluster')
plt.show()