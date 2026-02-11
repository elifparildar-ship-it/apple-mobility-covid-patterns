# Apple Mobility Trends: COVID-19 Mobility Patterns

## Overview
This project analyzes Apple Mobility Trends data to explore how mobility levels changed across countries during the COVID-19 period. 
The workflow combines time-series analysis, interactive visualization, GIS mapping, and similarity-based network methods.

## Research Questions
- How did average country-level mobility change over the course of 2020?
- Which countries show the strongest mobility reductions and recoveries?
- Are there clusters of countries with similar mobility patterns?

## Data
Apple Mobility Trends (public dataset). 
Raw data files are not included in this repository; the analysis script documents the expected file structure and processing steps.

## Methods
- Data reshaping (wide-to-long) and time filtering
- Interactive time-series visualization
- Country comparisons (top/bottom mobility patterns)
- GIS mapping using `leaflet`
- Similarity analysis and network/community detection

## Key Findings (Summary)
- Mobility dropped sharply in early 2020 across most countries, with substantial variation in recovery patterns.
- Countries can be grouped into meaningful clusters based on similarity in mobility trajectories.

## Files
- `apple_mobility_analysis.R`: main analysis script

### Course Context
This project was developed as part of the *Computational Social Science* course (IR421).  
The analysis builds on course materials and methodological guidance provided during the course, and was independently re-implemented, adapted, and extended by the author to explore mobility patterns using real-world data.
