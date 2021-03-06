SAS code for Template Matching in VA
By Brenda Vincent
date: January 10, 2019
---

Overview

This repository contains code necessary to perform the simulations and data analysis described in the paper "Template Matching for Benchmarking Hospital Performance in the Veterans Affairs Healthcare System". 
This paper evaluated a template matching algorithm for benchmarking hospital performance in the VA healthcare system compared to standard regression approaches. Simulations were used to understand the effect 
of patient case-mix, hospital case-volume and hospital performance.

Simulations

The SAS code for each of the simulations are provided:

•Simulation 1: baseline case

•Simulation 2a: case-mix varies by age

•Simulation 2b: case-mix varies by predicted mortality

•Simulation 3: hospital performance varies

•Simulation 4a: case-mix varies by age and hospital performance varies

•Simulation 4b: case-mix varies by predicted mortality and hospital performance varies

•Simulation 5: hospital case volume varies

•Simulation 6a: case-mix varies by age and hospital case volume varies

•Simulation 6b: case-mix varies by predicted mortality and hospital case volume varies

•Simulation 7: hospital performance and hospital case volume varies

•Simulation 8a: case-mix varies by age, hospital performance varies and hospital case volume varies

•Simulation 8b: case-mix varies by predicted mortality, hospital performance varies and hospital case volume varies


Data Analysis

•The file "Analysis" contains the code for the main analyses (for the simulations) presented in the paper.

•The file "Analysis with real data" presents the code for the analysis using real IPEC data, with and without the post-match.

•The file "Scenario 8b with post-match" presents the code the for post-hoc analysis of scenario 8b with the post-match adjustment.
