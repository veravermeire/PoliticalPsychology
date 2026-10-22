# Political Psychology: Simulating Opinion Dynamics

An interactive R Shiny app and statistical analysis that simulate how individual opinions and social interactions can lead to political division or convergence.

## Quick start
1. Clone the repository: `git clone <repo-url>`
2. Open the project folder in RStudio
3. Install the required packages: `source("install_packages.R")`
4. Run the app: open `main/app.R` and click **Run App**

## Why study political polarization?
Political disagreement is a normal part of democracy, but persistent divisions can make communication and cooperation across political groups more difficult. Research on affective polarization, which refers to negative attitudes toward political outgroups, highlights how political identity, perceived intergroup threat, and media environments may contribute to negative attitudes toward political opponents (Kogel & Zinnatullin, 2026; Renström et al., 2021). Other research suggests that openness to opposing views and the strength of partisan identification matter for cross-group engagement (Porter & Schumann, 2018; Thomsen & Thomsen, 2023).

While affective polarization concerns attitudes toward political outgroups, opinion-based polarization refers to differences in political opinions between groups. In our simulation, **depolarization** occurs when the average political opinions of two groups move closer together over time. Conversely, **polarization increases** when the distance between their average opinions grows.

Building on these perspectives, this project explores the following question: **How can individual opinions and social interactions produce broader patterns of political division or convergence?** We use computational simulations to make these processes visible and examine how outcomes depend on the assumptions built into the model.

## Project components

### 1. Main simulation: political identity, perceived polarization, and congenial media
Our R Shiny app models people as agents connected through a social network, with political opinions represented between −1 and +1. The existing app illustrates how network connections, openness to other opinions, and social influence shape opinion change.

The main simulation is being adapted to incorporate the same three psychological variables investigated in the statistical analysis: political identity strength, perceived polarization, and congenial media exposure. The goal is for the interactive simulation and the factorial analysis to examine the same theoretical mechanisms. Until that adaptation is complete, the current app uses its existing openness- and network-based rules.

### 2. Separate ANES 2024 real-data mode
The app also includes an exploratory mode based on respondents from the 2024 American National Election Studies (ANES, 2025). Instead of generating all starting opinions artificially, it uses respondents' reported liberal–conservative self-placement, rescaled from 1–7 to −1 to +1.

The ANES mode also uses reported party identity importance to set each agent's openness to differing views, and selected congenial TV news exposure to influence the probability of connecting with like-minded agents. Connections and subsequent opinion changes are still simulated, not observed in ANES. This is an empirically informed illustration, not a validation of the model.

### 3. Statistical analysis of the main theoretical model
The statistical analysis investigates the same three variables that the main interactive simulation is being adapted to include: political identity strength, perceived polarization, and congenial media exposure. Unlike the ANES mode, this is a controlled theoretical simulation experiment.

We used a 2 × 2 × 2 factorial design, comparing low and high parameter settings across 30 random seeds, with 100 agents and 200 simulation steps. Analyses examined the change in distance between the two political groups' mean opinions, trajectories over time, and the number of steps required for outcomes to stabilize. We also conducted sensitivity checks using alternative stabilization thresholds and network density.

The main finding was an interaction between identity strength and congenial media exposure: stronger identity was associated with greater depolarization when congenial media exposure was low, but less depolarization when exposure was high. Perceived polarization had little effect on final opinion distance, although it was associated with slower early depolarization and stabilization.

## Repository structure
```
PoliticalPsychology/
├── README.md
├── install_packages.R       # installs the required R packages
├── main/
│   └── app.R                # interactive Shiny app (main deliverable)
├── data/
│   ├── simulation.r         # core opinion-dynamics model
│   ├── anes_data.R          # reads and prepares the ANES respondents
│   └── anes_dataset/        # ANES 2024 Time Series CSV and guidebook
└── analysis/
    ├── analysis.R           # factorial and statistical analyses, figures
    └── README.md            # details on the analysis
```

## How to run the project
The interactive application is in `main/app.R`, and its core model is in `data/simulation.R`. Open `main/app.R` in RStudio and click **Run App**, or run `shiny::runApp("main")` from the project folder. The separate ANES mode uses `data/anes_data.R` and reads the ANES 2024 Time Series CSV from `data/anes_dataset/`. The factorial and statistical analyses, and the visualizations of opinion dynamics and polarization outcomes, are in `analysis/analysis.R` (see `analysis/README.md`).

## Requirements
R (version 4.0 or newer) and the packages `shiny`, `ggplot2`, and `here`. Run `source("install_packages.R")` once to install them. File paths are built with `here`, so the project runs from any folder on any computer as long as the repository structure above is kept.

## Data
The ANES mode uses the ANES 2024 Time Series Study Full Release (American National Election Studies, 2025). The CSV file (its name starts with `anes_timeseries`) and the ANES guidebook are in `data/anes_dataset/`. If the CSV is missing, the app still runs in simulation mode and shows a message in the ANES view. The data can be downloaded from https://electionstudies.org/data-center/2024-time-series-study/.

`data/anes_data.R` reads only the survey columns we need and turns them into one row per respondent:
- **Opinion:** liberal–conservative self-placement, rescaled from 1–7 to −1 to +1.
- **Side:** party identification, with Democrats and leaners as one group and Republicans and leaners as the other. Pure independents have no side and are left out.
- **Identity strength:** importance of party identity, rescaled to 0–1.
- **Congenial media:** the number of same-side TV news shows (Fox shows for Republicans, MSNBC shows for Democrats) a respondent mentioned, capped at 3 and rescaled to 0–1.
- **Perceived polarization:** the difference between where respondents place the Republican and Democratic parties.

Only respondents with an opinion, an identity score, and a media score are kept. The same script also contains the helpers the app uses to compare the real respondents with the simulated population.

## Limitations
This project is exploratory. Social connections and opinion-update rules are simplified modeling assumptions, and the ANES data do not provide observed social ties or longitudinal opinion changes. Our primary outcome is distance between political opinions, which is different from affective polarization (negative feelings toward political outgroups). The simulation results describe the behavior of these models, not causal effects demonstrated among real voters.

## References
- American National Election Studies. (2025). *ANES 2024 Time Series Study Full Release* [Dataset and documentation]. August 8, 2025 version. https://electionstudies.org/data-center/2024-time-series-study/
- Kogel, M. J., & Zinnatullin, A. (2026). What drives affective polarization? A systematic literature review. European Political Science. https://doi.org/10.1017/S1682098326100642
- McMahon, M. J. (2025). The need for cognitive closure and uncertainty-identity theory: Psychological mechanisms for evaluating extremism [Master's thesis, Naval Postgraduate School].
- Porter, T., & Schumann, K. (2018). Intellectual humility and openness to the opposing view. Self and Identity, 17(2), 139–162. https://doi.org/10.1080/15298868.2017.1361861
- Renström, E. A., Bäck, H., & Carroll, R. (2021). Intergroup threat and affective polarization in a multi-party system. Journal of Social and Political Psychology, 9(2), 553–576. https://doi.org/10.5964/jspp.7539
- Thomsen, J. P. F., & Thomsen, A. H. (2023). Intergroup contact reduces affective polarization but not among strong party identifiers. Scandinavian Political Studies, 46(4), 241–263. https://doi.org/10.1111/scps.12242
