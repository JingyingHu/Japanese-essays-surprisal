# LLM Surprisal Analysis

This repository contains the code and processed data for the analyses reported in the following paper:
```bibtex
@inproceedings{osumi-etal-2026-examining,
  title     = {Examining {LLM}-Surprisal as an Indicator of Naturalness for {J}apanese Automated Essay Scoring},
  author    = {Osumi, Akari and Hu, Jingying and Cong, Yan and Fukada, Atsushi},
  booktitle = {Proceedings of the Artificial Intelligence in Measurement and Education Conference ({AIME}-Con): Full Papers},
  year      = {2026},
  month     = {October},
  pages     = {221--229},
  publisher = {National Council on Measurement in Education (NCME)},
  url       = {https://aclanthology.org/2026.aimecon-main.24/}
}

## Files

- `initial_datacleaning.py`: Processes data to unify punctuation types and remove annotators' notes.
- `classic_dindices.py`: Calculates essay-level classic indices by cross-referencing a Japanese educational vocabulary database (Sunakawa et al., 2012).
- `surprisal.py`: Calculates essay-level mean surprisal using six language models.
- `data_analysis.R`: Performs statistical analyses and L2 proficiency classification.
- `essay_data_with_stats_surprisal_final.csv`: Processed data containing linguistic measures and surprisal scores.


