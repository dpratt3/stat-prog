import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

raw_data = pd.read_csv("sparse_pca_transformed_data.csv")

# Create a 5x2 grid for subplots
fig, axs = plt.subplots(5, 2, figsize=(12, 18))  # Adjust the figsize as needed

# Flatten the axs array to easily iterate through subplots
axs = axs.flatten()

# Plot each column in a separate subplot
for i, column in enumerate(raw_data.columns):
    axs[i].hist(raw_data[column], bins=100, range=(-30, 30), color = "green")  
    #axs[i].set_title(f'Column {i+1}', fontsize = 10)  
    # axs[i].set_xlabel('Values')
    axs[i].set_ylim(ymin=0.001)  # Set y-axis limit to start from a small value above zero  
    axs[i].set_ylabel('Frequency')  
    axs[i].grid(True)
    axs[i].text(0.5, 0.9, f'Column {i+1}', fontsize=10, ha='left', transform=axs[i].transAxes)  

# Adjust layout and display the plot
plt.tight_layout()
plt.show()

# Quantify sparsity
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


sparse_cols = 0

for i in range(140):
    sparse_cols += np.sum(colsums == i)

# Of approximately 
print(sparse_cols)