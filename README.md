# PoliticalPsychology

Political Psychology: Simulating Opinion Dynamics
Why study political polarization?
Political disagreements are a normal part of democracy. However, when political divisions become entrenched, people may become less willing to engage with opposing viewpoints, making constructive discussion and cooperation more difficult. Research on affective polarization—negative feelings toward political outgroups—highlights the roles of social identity, perceived threat, and information environments (Kogel & Zinnatullin, 2026; Renström et al., 2021). Evidence also suggests that contact across political groups may be less effective among people with stronger partisan identities (Thomsen & Thomsen, 2023).

These findings raise a broader question: How do everyday social interactions shape the development or persistence of political differences? Our project uses a computational simulation to explore this question. Rather than predicting real elections or voters' behavior, it illustrates how simple rules of social influence can produce different patterns of opinion change across a network.
About the project
We developed an agent-based opinion dynamics model in R, with an interactive Shiny application. Each agent represents a person with a political opinion between −1 and +1. Agents are connected through a social network, representing opportunities to influence one another.

At each simulation step, an agent considers the opinions of connected neighbors who fall within an adjustable openness threshold. The agent then moves partway toward those neighbors' average opinion. People with no eligible neighbors keep their current opinion. All opinions update simultaneously.

The application lets users explore how outcomes depend on:
- Starting opinions: a broad distribution, opinions concentrated near the center, or two opposing groups.
- Network structure: the number of connections and the tendency to connect with like-minded people (homophily).
- Openness to disagreement: how different a neighbor's opinion can be before influence stops.
- Strength of influence: how far an agent moves toward eligible neighbors at each step.
The interface visualizes the network, opinion distribution, and trajectories over time. Users can pause, advance or reverse steps, change settings, and export opinion histories.
Why use a simulation?
A simulation makes it possible to change one social-influence rule at a time and observe how collective patterns emerge. For example, a network may contain many connections yet show little convergence if agents disregard neighbors with substantially different opinions. The model therefore offers a way to think about the relationship between individual openness, social connections, and group-level divisions.

This approach is informed by research on polarization and openness to opposing views (Kogel & Zinnatullin, 2026; Porter & Schumann, 2018). Importantly, the model tracks opinion differences, not hostility toward political outgroups. Its rules are simplified assumptions, so its outputs should be interpreted as exploratory demonstrations rather than evidence of causal effects in real populations.
Running the app
Open the PP_app folder in RStudio, install the shiny package if needed, and run app.R. The core simulation functions are in simulation.R.
References
- Kogel, M. J., & Zinnatullin, A. (2026). What drives affective polarization? A systematic literature review. European Political Science. https://doi.org/10.1017/S1682098326100642
- McMahon, M. J. (2025). The need for cognitive closure and uncertainty-identity theory: Psychological mechanisms for evaluating extremism [Master's thesis, Naval Postgraduate School].
- Porter, T., & Schumann, K. (2018). Intellectual humility and openness to the opposing view. Self and Identity, 17(2), 139–162. https://doi.org/10.1080/15298868.2017.1361861
- Renström, E. A., Bäck, H., & Carroll, R. (2021). Intergroup threat and affective polarization in a multi-party system. Journal of Social and Political Psychology, 9(2), 553–576. https://doi.org/10.5964/jspp.7539
- Thomsen, J. P. F., & Thomsen, A. H. (2023). Intergroup contact reduces affective polarization but not among strong party identifiers. Scandinavian Political Studies, 46(4), 241–263. https://doi.org/10.1111/scps.12242
