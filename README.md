### Summary of work

This is a small study on the potential performance of a BEATs pipeline to classify and study killer whale calls. 
We make use of the training dataset made available in (Palmer et al., 2025)[^1], and further curated and used in (Palmer et al., 2026)[^2]. Specifically, we use the dataset versioned Birdnet_05, with a call-type balancing of 10% for both Southern Resident Killer Whales and Transient Killer Whales. 
We also draw inspiration from (Cauzinille et al., 2024)[^3], an article showing that general purpose Self-Supervised Learning models can be leveraged
for bio-acoustic tasks.    
     
First we embed the audio samples by means of BEATs' pre-trained acoustic tokenizer (Chen et al., 2023)[^4]. Each sample ends up being represented by a list of 144 indices that reference the fixed vectors in the acoustic tokenizer's embedding codebook.
It is worth noting that at this step a non-neglectable amount of information is lost: as an input to the tokenizer, the soundwave is downsampled from 48kHZ to 16kHz, which is well bellow the recommended Nyquist frequency to capture the full range of KW's pulsed calls. 
Furthermore, the pre-processing embedded in the tokenizer is a mel filterbank to imitate human aural perception.  

As a second step, we fit a logistic regression on the counts of the indices that embed the calls. That is, each call is represented as a vector counting how many times each codebook entry appears among the 144 tokens, discarding temporal structure.  

Despite the tremendous loss in information at this stage, we report an accuracy on out-of-sample test set of 0.669, well above the 0.53 baseline accuracy of a classifier that would always predict SRKW. The recall associated with Transient Killer Whale detection remains, however, at 0.53.    

### Work in progress

To take advantage of the explainability that logistic regressions offer, we extract the coefficients related to the maximum change in odds, positive and negative. We extract the spectrogram indices that relate to those coefficients. We note that the maximum positive change in odds relates to background noise, indicating that the logistic regression captures environmental queues that help in the classification. We're still exploring the index related to the maximum negative change in odds, in the hopes that it is a semantic acoustic feature related to TKW calls. The purpose is to localise it and plot it in the spectrograms.

### References

[^1] Palmer, K.J. et al. (2025) “A Public Dataset of Annotated Orcinus orca Acoustic Signals for Detection and Ecotype Classification,” Scientific Data, 12(1), p. 1137. Available at: https://doi.org/10.1038/s41597-025-05281-5.
[^2] Palmer, K.J. et al. (no date) “Population‐Level Acoustic Classification of Salish Sea Killer Whales: Integrating Biologically Informed Call Type Balancing to Build Robust Models for Conservation Monitoring.” Available at: https://doi.org/10.1111/mms.70126.
[^3] Cauzinille, J. et al. (2024) “Investigating self-supervised speech models’ ability to classify animal vocalizations: The case of gibbon’s vocal signatures,” Interspeech 2024. Interspeech 2024, ISCA, pp. 132–136. Available at: https://doi.org/10.21437/Interspeech.2024-1096.
[^4] Chen, S. et al. (2022) “BEATs: Audio Pre-Training with Acoustic Tokenizers.” arXiv. Available at: https://doi.org/10.48550/arXiv.2212.09058.
 
### Custom Scripts

embed.py and dataset.py in beats/.   
gen_dl_data.py and extract_paths.py in ds/    
eda.ipynb has the logistic regression on frequency of tokens. 


### Why BEATs

![](./beats/Evaluation_Results/Tokenizer.png "BEATs pipeline")

In the BEATs framework  (Chen et al.,  2023), an acoustic tokenizer and an audio SSL model are iteratively trained. In each iteration
of the training, first the acoustic tokenizer is leveraged to generate discrete labels for the audio patches on the basis of a codebook of embeddings (quantization step).
Here the SSL model must predict the labels the tokenizer generates, so the training target is discrete label prediction loss.
After convergence, on the second part of the iteration, the SSL model is leveraged to act as a teacher for the acoustic tokenizer, who must generate similar labels to those 
of SSL model. In this part, the loss flows down the network, with a trick on the quantization step ("straight-through gradients") and the codebook of embeddings is 
learned/optimized by Exponential Moving Average.

By means of the quantization step, this framework intends to make the models learn meaningful representations of the soundwave and discard potential noise, 
which sounds ideal for a classification task.

What we find interesting in relation with linguistics, is that the resulting acoustic tokenizer (which is usually discarded when training is done)
allows us to discretise and simply encode the patches on the spectrogram, which opens the gates to high-level studies on the phonological structure of
killer whale calls. It's like a massive and fine-grained spectrogram tagger.
