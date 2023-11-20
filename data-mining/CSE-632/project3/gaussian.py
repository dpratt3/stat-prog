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

# Fit GMM model to the data
gmm = GaussianMixture(n_components=num_clusters, random_state=42)
labels = gmm.fit_predict(data)

# Plot histograms for each cluster for all 10 features
for cluster_label in range(num_clusters):
    cluster_data = data[labels == cluster_label]  # Data points belonging to the cluster

    # Plot histograms for each feature
    plt.figure(figsize=(12, 8))
    for feature_idx in range(cluster_data.shape[1]):
        plt.subplot(2, 5, feature_idx + 1)  # Adjust the subplot grid as per the number of features
        plt.hist(cluster_data.iloc[:, feature_idx], bins=30, alpha=0.7)
        plt.title(f'Feature {feature_idx + 1} - Cluster {cluster_label+1}')
        plt.xlabel('Values')
        plt.ylabel('Frequency')

    plt.suptitle(f'Histograms for Cluster {cluster_label+1}')
    plt.tight_layout()
    plt.show()

# Look at clusters in terms of response variable
data['Cluster_Labels'] = labels

# Import raw data

# Import both target and non-target variables, join them, and eliminate the 'Act' column
train = pd.read_csv("~/Downloads/QSAR-data/OX2_training_disguised.csv")
test = pd.read_csv("~/Downloads/QSAR-data/OX2_test_disguised.csv")

train_cols = set(train.columns)
test_cols = set(test.columns)

if train_cols == test_cols:
    print("Train and test sets have the same columns.")
else:
    print("Train and test sets have different columns.")

df = pd.concat([train, test], ignore_index=True, axis=0)

clustered_response = pd.DataFrame(index = data['Cluster_Labels'] columns = df['Act'])
print(clustered_response)