import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import requests
import json
import io
import glob
from io import BytesIO

# import DNA methylation data as a dataframe
dnam_df = pd.read_csv("Data/DraftData/gdc_download_20220119_070852.100966/22d1cb3c-8ea3-4fe5-860d-aa68a2fb039b/jhu-usc.edu_LUAD.HumanMethylation450.14.lvl-3.TCGA-J2-A4AD-01A-11D-A24I-05.gdc_hg38.txt", 
sep = "\t")

print(dnam_df.columns)

# specify endpoints for JSON connection
cases_endpt = "https://api.gdc.cancer.gov/cases"
files_endpt = "https://api.gdc.cancer.gov/files"

def getFileNames():
    """gets the file names of everything in a specified directory"""
    file_names = glob.glob("Data/DraftData/**/*.txt", recursive= True)
    file_manifests = glob.glob("Data/DraftData/**/MANIFEST.txt", recursive= True)
    all_files = [pd.read_csv(file, sep = "\t", header=None) 
    for file in file_manifests]
    data_files = []
    for i in all_files:
        data_files.append(i.iloc[1,0])
    return data_files

def fileToDataType(fileUUID):
    """converts a file ID to its corresponding data category (e.g. DNA methylation
    Transcriptome Profiling, Clinical, etc."""
    filters = {
    "op": "in",
    "content": {
            "field":"file_id",
            "value":[fileUUID]
        }
    }
    fields = [
       "data_category"
    ]
    params = {
    "filters": json.dumps(filters),
    "fields": fields,
    "format": "TSV",
    "size":100
    }
    t = requests.get(files_endpt, params = params).content
    tdf = pd.read_csv(BytesIO(t), sep = "\t")
    return tdf.iloc[0,0]

def caseToFiles(caseUUID):
    """converts a case ID to an array of corresponding files"""
    filters = {
    "op": "in",
    "content": {
            "field":"case_id",
            "value":[caseUUID]
        }
    }
    fields = [
       "files.file_id"
    ]
    params = {
    "filters": json.dumps(filters),
    "fields": fields,
    "format": "TSV",
    "size":100
    }
    t = requests.get(cases_endpt, params = params).content
    tdf = pd.read_csv(BytesIO(t), sep = "\t")
    tdf = tdf.iloc[0,:].to_numpy()
    tdf = np.delete(tdf, np.where(tdf == caseUUID))
    return tdf

def caseHasAllData(caseUUID, datacats):
    """for a set of data categories datacats, determines if a particular case has
    all the data categories (way to filter out cases with missing genomic, transcriptomic,
    and epigenetic data """
    caseFiles = caseToFiles(caseUUID)
    types = []
    for i in caseFiles:
        types.append(fileToDataType(i))
    types = np.unique(types)
    j=0
    while j < len(datacats):
        if datacats[j] in types:
            j = j+1
        else:
            return False
    return True
        
# Example        
datacats = ["DNA Methylation", "Clinical"]
print(caseHasAllData("35cb7841-9b09-465a-90c5-e3b8a9faad49", datacats))
