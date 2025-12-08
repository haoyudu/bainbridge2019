# Replication of Drawing Match from Bainbridge et al. (2019, Nature Communications)

This repository contains the anonymized data, code, and report of a replication of the drawing match experiment in Bainbridge et al. (2019). The study is a part of UCSD's PSYC 201A with [Dr. Bria Long](brialong.com). 

`data/` contains the experimental stimuli. `data/stim/` contains images from the SUN dataset. `data/drawings` contains drawings collected by Brainbridge et al. (2019). 

`experiment/` contains the jsPsych experiment (`experiment/index.html`) that was run on Prolific. Raw data collected in the main experiment hosted on OSF: [https://osf.io/njtas](https://osf.io/njtas)

`output/` contains the cleaned dataset with the data dictionary. `output/results` is where the figures in the report lives. 

`utils/` contains helper functions that were used during the construction of the experiment and the preprocessing script used to generate the cleaned dataset.

`_freeze/`, `.github/`, `.pixi/`, are infrastructural directories for the write up (`index.html`). The environment setup for the analyses is specified by `pixi.toml`.




