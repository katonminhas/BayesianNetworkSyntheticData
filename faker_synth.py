#%%
import pandas as pd
import numpy as np

from sdv.tabular import GaussianCopula




#%%
path = "C:/Users/Katon/Documents/JHU/ReasoningUnderUncertainty/ResearchPaper/data/NON_PHI_sample_claims.csv"
claims = pd.read_csv(path)

#%%

model = GaussianCopula()
model.fit(claims)

synth_data = model.sample(num_rows=500000)

