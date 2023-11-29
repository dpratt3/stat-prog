import numpy as np

traversal_log = np.array(['X', 'Y', 'Z', 'W', 'Y', 'A', 'B', 'C', 
                            'D', 'Y', 'C', 'D', 'E', 'F', 'D', 'E', 
                                'X', 'Y', 'A', 'B', 'M', 'N'])

keys = np.unique(traversal_log)

# store indices
positions = {}

# Return the maximal traversal length between occurences
def max_diff(arr):
    if len(arr) == 0 or len(arr) == 1:
        return 0
    else:
        return np.max(np.diff(arr))

for key in keys:
    indices = np.where(traversal_log == key)[0]
    positions[key] = max_diff(indices)

for key, value in positions.items():
    print(f"{key}:{value}")
