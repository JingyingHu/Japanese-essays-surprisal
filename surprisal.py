# set up environment
import torch
device = "cuda" if torch.cuda.is_available() else "cpu"
print("Using device:", device)

# Hugging Face login
from huggingface_hub import login
login("your_huggingface_token")

# Enable progress_apply()
from tqdm import tqdm
tqdm.pandas()



# ------------------------------------------------------------------
# 1. Example sentences:

# (1) 
# 私は本を 読みました。(を’s surprisal:1.93)
# watashi-wa hon-o yomi-mashi-ta
# I-TOP book-ACC read-POL-PAST
# ‘I read a book.’

# (2) 
# * 私は本が 読みました。(が’s surprisal:4.43)
# watashi-wa hon-ga yomi-mashi-ta
# I-TOP book-NOM read-POL-PAST
# ‘I read a book.’
# ------------------------------------------------------------------

sentences = ["私は本を読みました。","私は本が読みました。"]
# grammatical: を
# ungrammatical: が

from minicons import scorer
model = scorer.IncrementalLMScorer("meta-llama/Llama-3.1-8B", device)

token_scores = model.token_score(
    sentences,
    bos_token=True,   
    surprisal=True)  # −log P(token | context)

tokz = model.tokenizer  

SPECIAL = {"<|begin_of_text|>", "<|end_of_text|>", "<s>", "</s>"} 

for sent, scores in zip(sentences, token_scores):
    print("\nSentence:", sent)
    for tok, surp in scores:
        if tok in SPECIAL:
            continue
        pretty = tokz.convert_tokens_to_string([tok]).strip()
        print(f"{pretty}\t{round(surp, 3)}")





# ------------------------------------------------------------
# 2. Calculate essay-level mean surprisal using each language model
# ------------------------------------------------------------

# define utility function  or calculating mean surprisal
import numpy as np
def calculate_mean_surprisal(text, model):
    try:
        score = model.sequence_score(text,
                                     bos_token=True, # ensure a consistent conditioning context for the first token
                                     reduction=lambda x: (-x).mean().item())
        return round(score[0],3)
    except Exception as e:
        print(f"Error processing text: {text}, Error: {e}")
        return np.nan


# Load essay data
import pandas as pd
df = pd.read_csv(file + 'file_name.csv')

# Calculate mean surprisal for each essay using each language model
tokyollm8b = scorer.IncrementalLMScorer('tokyotech-llm/Llama-3.1-Swallow-8B-v0.5', device)
df['tokyollm8b_surp'] = df['text'].progress_apply(lambda x: calculate_mean_surprisal(x, tokyollm8b))

tokyollm8b_ins = scorer.IncrementalLMScorer('tokyotech-llm/Llama-3.1-Swallow-8B-Instruct-v0.5', device)
df['tokyollm8b_ins_surp'] = df['text'].progress_apply(lambda x: calculate_mean_surprisal(x, tokyollm8b_ins))

jpllm37b = scorer.IncrementalLMScorer('llm-jp/llm-jp-3-3.7b', device)
df['jpllm37b_surp'] = df['text'].progress_apply(lambda x: calculate_mean_surprisal(x, jpllm37b))

jpllm37b_ins = scorer.IncrementalLMScorer('llm-jp/llm-jp-3-3.7b-instruct', device)
df['jpllm37b_ins_surp'] = df['text'].progress_apply(lambda x: calculate_mean_surprisal(x, jpllm37b_ins))

llama8b = scorer.IncrementalLMScorer('meta-llama/Llama-3.1-8B', device)
df['llama8b_surp'] = df['text'].progress_apply(lambda x: calculate_mean_surprisal(x, llama8b))

llama8b_ins = scorer.IncrementalLMScorer('meta-llama/Llama-3.1-8B-Instruct', device)
df['llama8b_ins_surp'] = df['text'].progress_apply(lambda x: calculate_mean_surprisal(x, llama8b_ins))

# save the result
df.to_csv(file + 'file_name.csv')