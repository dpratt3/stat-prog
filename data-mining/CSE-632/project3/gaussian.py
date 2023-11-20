import pandas as pd
from sklearn.cluster import DBSCAN
from sklearn.neighbors import NearestNeighbors
import numpy as np
from scipy import ndimage
from joblib import Parallel, delayed
from sklearn.metrics import silhouette_score
from sklearn.mixture import GaussianMixture
import matplotlib.pyplot as plt

data = pd.read_csv("sparse_pca_transformed_data.csv")
data = data.iloc[:, :10]

# Try different numbers of clusters
n_clusters = range(1, 11)
bics = []
for n in n_clusters:
    gmm = GaussianMixture(n_components=n, random_state=42)
    gmm.fit(data)
    bics.append(gmm.bic(data))

# Plotting BIC scores (elbow appears to occur at 6)
plt.plot(n_clusters, bics, marker='o')
plt.xlabel('Number of Clusters')
plt.ylabel('BIC Score')
plt.title('BIC Score vs Number of Clusters')
plt.show()

# Optimal cluster number
num_clusters = 6

# Fit GMM model to the data
gmm = GaussianMixture(n_components=num_clusters, random_state=42)
labels = gmm.fit_predict(data)

# Count the number of data points in each cluster
cluster_sizes = []
for cluster_label in range(num_clusters):
    cluster_size = sum(labels == cluster_label)
    cluster_sizes.append(cluster_size)

# Display the sizes of the clusters
for i, size in enumerate(cluster_sizes):
    print(f"Cluster {i+1}: {size} data points")
