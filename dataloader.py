import os 
import polars as pl
import torch
from torch.utils.data import Dataset
from torchaudio.transforms import Resample
import librosa
import soundfile as sf

params = {
    'clipDur':3,
    'outSR': 16000,
    'fmin':0
}

class LocalBirdnetDataset(Dataset):
    def __init__(self, annotations_file='ds/b05_onlykw.csv', audio_dir='./ds/', transform=None):
        self.labels = pl.read_csv(annotations_file)
        self.audio_dir = audio_dir
        self.transform = transform
    
    def __len__(self):
        return self.labels.shape[0]

    def __getitem__(self, idx):
        row = self.labels[:][idx]
        file_path = row['FilePath']
        start_time = row['FileBeginSec']
        end_time = row['FileEndSec']
        dep = row['Dataset']
        provider = row['Provider']
        speciesFolder = row['Labels']
        soundfile = row['Soundfile']
        CT = row['CalltypeCategory']
        audio_path = os.path.join(self.audio_dir, self.labels['pos_FilePath'][idx])
        file_duration = librosa.get_duration(path=audio_path)
        duration = end_time - start_time
        center_time = start_time + duration / 2.0
        new_start_time = max(0, center_time - params['clipDur'] / 2.0)
        new_end_time = new_start_time + params['clipDur']
        if new_end_time > file_duration:
            new_start_time = max(0, file_duration - params['clipDur'])
            new_end_time = file_duration

        audio_data, sample_rate = librosa.load(
            audio_path,
            sr = params['outSR'],
            offset=new_start_time,
            duration=params['clipDur'],
            mono=False
        )
        #apply pre-processing to audio_path
        label = self.labels['Ecotype'][idx]
        

