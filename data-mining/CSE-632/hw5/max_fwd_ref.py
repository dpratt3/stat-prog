import numpy as np

traversal_log = np.array(['A', 'Z', 'B', 'C', 'D', 'E', 'D', 'C', 'B', 
                            'X', 'Y', 'Z', 'Y', 'X', 'K', 'L', 'K', 'X', 
                                'B', 'M', 'N', 'O', 'N', 'M', 'B', 'Z', 'A', 
                                    'G', 'H', 'I'])

keys = np.unique(traversal_log)

# store indices
positions = {}

# Return the maximal traversal length between occurences
def max_diff(arr):
    if len(arr) == 0 or len(arr) == 1:
        return 0
    else:
        return np.max(np.diff(arr)) - 1 

for key in keys:
    indices = np.where(traversal_log == key)[0]
    positions[key] = max_diff(indices)

for key, value in positions.items():
    print(f"{key}:{value}")
