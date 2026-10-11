# spotify-essay

A data essay analyzing my personal Spotify Wrapped 2025 data, built in R with Shiny, Plotly, and tidyverse. 

This project investigates how platform design shapes listening behavior, and how easily the same data can tell completely different stories depending on who's holding the pen.

Read it here: [I Analyzed My Spotify Data So You'd Think About Yours](https://nadia-bourne.shinyapps.io/spotify/)

## Files

- `app.R` — Shiny web application code, including UI layout, custom CSS, and server-side plot rendering
- `spotify.Rmd` — data cleaning and feature engineering script; handles type conversion, manual enrichment, and dataframe preparation
- `FINAL-spotify-data.csv` — cleaned and enriched dataset used by the app
- `spotlistr-exported-playlist.csv` — unmodified CSV as exported from Spotifylistr
