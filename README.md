# Political Psychology: Simulating Opinion Dynamics
## Why study political polarization?
Political disagreement is a normal part of democracy, but persistent divisions can make communication and cooperation across political groups more difficult. Research on affective polarization, which refers to negative attitudes toward political outgroups, highlights how political identity, perceived intergroup threat, and media environments may contribute to negative attitudes toward political opponents (Kogel & Zinnatullin, 2026; Renström et al., 2021). Other research suggests that openness to opposing views and the strength of partisan identification matter for cross-group engagement (Porter & Schumann, 2018; Thomsen & Thomsen, 2023).

While affective polarization concerns attitudes toward political outgroups, opinion-based polarization refers to differences in political opinions between groups. In our simulation, **depolarization** occurs when the average political opinions of two groups move closer together over time. Conversely, **polarization increases** when the distance between their average opinions grows.

Building on these perspectives, this project explores the following question: **How can individual opinions and social interactions produce broader patterns of political division or convergence?** We use computational simulations to make these processes visible and examine how outcomes depend on the assumptions built into the model.

## Project components
### 1. Main simulation: political identity, perceived polarization, and congenial media
Our R Shiny app models people as agents connected through a social network, with political opinions represented between −1 and +1. The existing app illustrates how network connections, openness to other opinions, and social influence shape opinion change.

The main simulation is being adapted to incorporate the same three psychological variables investigated in the statistical analysis: political identity strength, perceived polarization, and congenial media exposure. The goal is for the interactive simulation and the factorial analysis to examine the same theoretical mechanisms. Until that adaptation is complete, the current app uses its existing openness- and network-based rules.
### 2. Separate ANES 2024 real-data mode
The app also includes an exploratory mode based on respondents from the 2024 American National Election Studies (ANES). Instead of generating all starting opinions artificially, it uses respondents' reported liberal–conservative self-placement, rescaled from 1–7 to −1 to +1.

The ANES mode also uses reported party identity importance to set each agent's openness to differing views, and selected congenial TV news exposure to influence the probability of connecting with like-minded agents. Connections and subsequent opinion changes are still simulated, not observed in ANES. This is an empirically informed illustration, not a validation of the model.
### 3. Statistical analysis of the main theoretical model
The statistical analysis investigates the same three variables that the main interactive simulation is being adapted to include: political identity strength, perceived polarization, and congenial media exposure. Unlike the ANES mode, this is a controlled theoretical simulation experiment.

We used a 2 × 2 × 2 factorial design, comparing low and high parameter settings across 30 random seeds, with 100 agents and 200 simulation steps. Analyses examined the change in distance between the two political groups' mean opinions, trajectories over time, and the number of steps required for outcomes to stabilize. We also conducted sensitivity checks using alternative stabilization thresholds and network density.

The main finding was an interaction between identity strength and congenial media exposure: stronger identity was associated with greater depolarization when congenial media exposure was low, but less depolarization when exposure was high. Perceived polarization had little effect on final opinion distance, although it was associated with slower early depolarization and stabilization.

## How to run the project
The interactive application is in PP_app/app.R, and its core model is in PP_app/simulation.R. Open the app in RStudio and select Run App. The analysis script contains the factorial simulation experiment and visualizations. The separate ANES mode uses PP_app/anes_network.R and requires the ANES 2024 Time Series CSV in the project's data/ folder.
## Limitations
This project is exploratory. Social connections and opinion-update rules are simplified modeling assumptions, and the ANES data do not provide observed social ties or longitudinal opinion changes. Our principal outcome is distance between political opinions, which is different from affective polarization (negative feelings toward political outgroups). The simulation results describe the behavior of these models, not causal effects demonstrated among real voters.
## References
- Kogel, M. J., & Zinnatullin, A. (2026). What drives affective polarization? A systematic literature review. European Political Science. https://doi.org/10.1017/S1682098326100642
- McMahon, M. J. (2025). The need for cognitive closure and uncertainty-identity theory: Psychological mechanisms for evaluating extremism [Master's thesis, Naval Postgraduate School].
- Porter, T., & Schumann, K. (2018). Intellectual humility and openness to the opposing view. Self and Identity, 17(2), 139–162. https://doi.org/10.1080/15298868.2017.1361861
- Renström, E. A., Bäck, H., & Carroll, R. (2021). Intergroup threat and affective polarization in a multi-party system. Journal of Social and Political Psychology, 9(2), 553–576. https://doi.org/10.5964/jspp.7539
- Thomsen, J. P. F., & Thomsen, A. H. (2023). Intergroup contact reduces affective polarization but not among strong party identifiers. Scandinavian Political Studies, 46(4), 241–263. https://doi.org/10.1111/scps.12242
