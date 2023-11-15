import pandas as pd
import plotly.express as px  # Import Plotly Express for plotting
from sklearn.decomposition import SparsePCA
from sklearn.preprocessing import StandardScaler
from joblib import Parallel, delayed
import numpy as np

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
df.fillna(0, inplace=True)
df.drop(columns=['Act'], inplace=True)

duplicates = df[df.duplicated(subset='MOLECULE', keep=False)]

if duplicates.empty:
    print("Each molecule is unique.")
else:
    print("There are duplicates in the 'MOLECULE' column.")

df.set_index('MOLECULE', inplace=True)

colsums = df.sum(axis=0)
sorted_colsums = colsums.sort_values(ascending=False)
print(sorted_colsums)
print((sorted_colsums <= 10).sum())

fig = px.histogram(x=colsums.values, labels={'x': 'Column Sums'}, title='Histogram of Column Sums')
fig.show()

# Define the number of cores to use
num_cores = 15

# Reduce dimension for benchmarking
# df = df.iloc[:, :1000]

chunk_size = len(df) // num_cores
chunks = [df.iloc[i:i + chunk_size, :] for i in range(0, len(df), chunk_size)]

def scale_data(chunk):
    scaler = StandardScaler()
    return scaler.fit_transform(chunk)

scaled_chunks = Parallel(n_jobs=num_cores)(delayed(scale_data)(chunk) for chunk in chunks)

# Convert scaled chunks (NumPy arrays) into DataFrames
scaled_dfs = [pd.DataFrame(chunk, columns=df.columns) for chunk in scaled_chunks]

# Concatenate the scaled DataFrames back into a single DataFrame
df_scaled = pd.concat(scaled_dfs)

def perform_sparse_pca(df):
    sparse_pca = SparsePCA(n_components=10, alpha=0.1)
    sparse_pca.fit(df)
    transformed_data = sparse_pca.transform(df)
    return transformed_data

transformed_data_chunks = Parallel(n_jobs=num_cores)(delayed(perform_sparse_pca)(chunk) for chunk in np.array_split(df_scaled.values, num_cores))

transformed_data = pd.DataFrame(np.vstack(transformed_data_chunks))

csv_file_path = 'sparse_pca_transformed_data.csv'
transformed_data.to_csv(csv_file_path, index=False)
print(f"Transformed data saved to '{csv_file_path}'")
