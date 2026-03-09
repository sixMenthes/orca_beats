import os 
import polars as pl
import torchaudio
from torch.utils.data import Dataset
from torchaudio.transforms import Resample
from torchcodec.decoders import AudioDecoder

import warnings
warnings.filterwarnings("ignore", message=".*has been deprecated.*")

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
        
        audio_path = os.path.join(self.audio_dir, self.labels['pos_FilePath'][idx])
        new_start_frame, new_end_frame = self.get_start(audio_path, idx)
        audio_data, sample_rate = torchaudio.load(
            audio_path,
            frame_offset=new_start_frame,
            num_frames=new_end_frame - new_start_frame)
        transform = Resample(sample_rate, params['outSr'])
        audio_data = transform(audio_data)
        label = self.labels['Ecotype'][idx]
        return audio_data, label
        
    def get_start(self, audio_path, idx):
        row = self.labels[:][idx]
        start_time = row['FileBeginSec']
        end_time = row['FileEndSec']
        decoder = AudioDecoder(audio_path)
        metadata = decoder.metadata
        file_duration = metadata.duration_seconds
        sample_rate = metadata.sample_rate
        duration = end_time - start_time
        center_time = start_time + duration / 2.0
        new_start_time = max(0, center_time - params['clipDur'] / 2.0)
        new_end_time = new_start_time + params['clipDur']
        if new_end_time > file_duration:
            new_start_time = max(0, file_duration - params['clipDur'])
            new_end_time = file_duration
        return (new_start_time*sample_rate, new_end_time*sample_rate)


