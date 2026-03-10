from Tokenizers import TokenizersConfig, Tokenizers
import torch
from torch.utils.data import DataLoader
import dataset
from tqdm import tqdm

device = torch.device("cuda")
checkpoint = torch.load('../ds/Tokenizer_iter3.pt')  
cfg = TokenizersConfig(checkpoint['cfg'])
BEATs_tokenizer = Tokenizers(cfg)
BEATs_tokenizer.load_state_dict(checkpoint['model'])
BEATs_tokenizer.eval()
BEATs_tokenizer.to(device)

ds = dataset.LocalBirdnetDataset()
dataloader = DataLoader(ds, batch_size=32)

labels_dict = {}

with torch.no_grad():
    for batch in tqdm(dataloader):
        audio, padding_mask, file_ids = batch
        audio = audio.squeeze(1)
        audio = audio.to(device)
        padding_mask = padding_mask.to(device)
        
        labels = BEATs_tokenizer.extract_labels(audio)
        labels = labels.view(audio.shape[0], -1)
        
        for file_id, label in zip(file_ids, labels):
            labels_dict[file_id.item()] = label.cpu()

# Save everything at once
torch.save(labels_dict, "labels.pt")