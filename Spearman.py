import pandas as pd
from scipy.stats import spearmanr
import numpy as np

#create DataFrame
df = pd.DataFrame({'Exist': [0.376681614,0.301075269,0.456140351,0.230769231,0.308571429,0.260869565,0.333333333,0.322580645],
'Rwa': [0.369668246,0.367816092,0.398148148,0.305882353,0.282208589,0.357798165,0.375,0.356321839]})

#calculate Spearman Rank correlation and corresponding p-value
rho, p = spearmanr(df['Exist'], df['Rwa'])

#print Spearman rank correlation and p-value
print(rho)

print(p)
