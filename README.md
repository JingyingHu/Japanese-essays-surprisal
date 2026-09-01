# LLM Surprisal Analysis

This repository contains the code and processed data for the analyses reported in the following paper:

**Examining LLM-Surprisal as an Indicator of Naturalness for Japanese Automated Essay Scoring**  
Akari Osumi*, Jingying Hu*, Yan Cong, Atsushi Fukada (* = equal contribution)  
*Accepted at The Artificial Intelligence in Measurement and Education Conference (AIME–Con), 2026*

## Files

- `initial_datacleaning.py`: Processes data to unify punctuation types and remove annotators' notes.
- `classic_dindices.py`: Calculates essay-level classic indices using the MeCab version 0.996 morphological analyser (Kudo, 2005), in conjunction with the IPADIC version 1.0.102 dictionary, and by cross-referencing a Japanese educational vocabulary database (Sunakawa et al., 2012).
- `surprisal.py`: Calculates essay-level mean surprisal using six language models.
- `data_analysis.R`: Performs statistical analyses and L2 proficiency classification.
- `essay_data_with_stats_surprisal_final.csv`: Processed data containing linguistic measures and surprisal scores.
