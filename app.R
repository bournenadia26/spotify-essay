library(tidyverse)
library(plotly)
library(shiny)
library(wesanderson)

song_list <- read_csv("data/FINAL-spotify-data.csv")

# Define global aesthetics
global_font <- list(family = "Nunito Sans", color = "#000000")
wes_colors1 <- wes_palette("GrandBudapest1", n = 4)
wes_colors2 <- wes_palette("GrandBudapest2", n = 4)
wes_colors3 <- wes_palette("Darjeeling1", n = 3)
genre_colors <- c("R&B/Soul" = "darkgoldenrod2",
                  "Pop" = wes_colors2[4],
                  "Hip Hop" = "olivedrab",
                  "Soundtrack" = wes_colors2[1],
                  "Pop Related Subgenre" = "darkslateblue")

song_list <- song_list |>
  mutate(text1 = paste0("<b>", Track.Name, "</b><br>",
                        Arist.s..Name, "<br>",
                        "Plays: ", plays, "<br>"),
         text3 = paste0( total_sec, " seconds long<br>"),
         text4 = paste0("<b>", Track.Name, "</b><br>", 
                        Arist.s..Name, "<br>",
                        "Length: ", round(total_sec / 60, 2), " min<br>", 
                        "Plays: ", plays),
         text5 = paste0("<b>", Track.Name, "</b><br>", Arist.s..Name, "<br>",
                        "Total time: ", round(total_mins_played, 1), " minutes<br>", 
                        "Plays: ", plays),
         text7 = paste0(Arist.s..Name, "<br>",
                        "Age: ", artist_age))

gender_df <- song_list |>
  count(gender) |>
  mutate(pct = n / sum(n),
         text2 = paste0("<b>", gender, "</b><br>", 
                        n, " songs (", scales::percent(pct, accuracy = 1), ")"))

artist_summary <- song_list |>
  mutate(Arist.s..Name = str_trim(Arist.s..Name, side = "right"),
         Arist.s..Name = case_when(grepl("Saja", Arist.s..Name) ~ "K-Pop Demon Hunters Cast",
                                   grepl("HUNTR/X", Arist.s..Name) ~ "K-Pop Demon Hunters Cast",
                                   TRUE ~ Arist.s..Name)) |>
  separate_rows(Arist.s..Name, sep= "; ") |>
  filter(!(Arist.s..Name %in% c("Anderson .Paak", "Erika Henningsen"))) |>
  mutate(genre = case_when(Arist.s..Name %in% c("TWICE", "K-Pop Demon Hunters Cast", "Maroon 5", "COIN", "Neon Trees", "KiNG MALA", "Bruno Mars") ~ "Pop Related Subgenre",
                           Arist.s..Name %in% c("Lady Gaga", "Sabrina Carpenter", "Kesha", "FLETCHER", "Chappell Roan", "Chrissy Chlapecka", "Charli xcx", "ELIO") ~ "Pop",
                           Arist.s..Name %in% c("Mercer Henderson", "Silk Sonic") ~ "R&B/Soul",
                           Arist.s..Name %in% c("bbno$", "Doechii", "Cardi B") ~ "Hip Hop",
                           Arist.s..Name == "Original Broadway Cast of Mean Girls" ~ "Soundtrack",
                           TRUE ~ NA_character_)) |>
  group_by(Arist.s..Name) |>
  summarise(n = n(),
            genre = first(genre)) |>
  filter(n >= 3) |>
  arrange(desc(n)) |> 
  mutate(text6 = paste0("<b>", Arist.s..Name, "</b><br>",
                        "Songs: ", n, "<br>",
                        "Genre: ", genre))

# UI
ui <- navbarPage(title = "Spotify Wrapped 2025 Analysis",
                 theme = bslib::bs_theme(bootswatch = "lux"),
                 
                 tags$head(tags$style(HTML(".navbar-default {
                                              background-color: #212529 !important;
                                            }
                                            .navbar-default .navbar-nav > li > a {
                                              color: #FFFFFF !important;
                                            }
                                            .navbar-default .navbar-nav > li > a:hover {
                                              color: #1DB954 !important;
                                            }
                                            .navbar-default .navbar-brand {
                                              color: #FFFFFF !important;
                                            }
                                            .callout-box {
                                              background-color: #212529;
                                              color: #FFFFFF;
                                              border-left: 4px solid #1DB954;
                                              padding: 15px;
                                              margin: 20px 0;
                                              border-radius: 4px;
                                            }
                                            
                                            .callout-box h4 {
                                              color: #1DB954;
                                              margin-top: 0;
                                            }
                                          "))),
                 
                 # Tab 1: Overview
                 tabPanel("Overview",
                          fluidRow(
                            column(width = 8, offset = 2,
                                   h2("Overview"),
                                   p("Spotify Wrapped told me I was an interesting and unique listener. I pulled the actual data and found I was, statistically, one of the most average consumers you've ever seen."),
                                   p("So I did what any reasonable data science student would do: I spent ", em("way"), " too long figuring out why."),
                                   p("This project is part interactive data essay, part critical thinking exercise, heavily inspired by The Pudding ", tags$a(href = "https://pudding.cool/", target = "_blank", "(go check them out!)."), " It uses my personal Spotify Wrapped 2025 data (100 songs, 252 hours of listening) to ask not just ", em("what"), " I listen to, but ", em("why"), " it ended up that way, and how much of it was actually my choice. 100% human-written! (Claude can rip the em dash from my cold, dead hands)"),
                                   
                                   h5("The short version of what I found:"),
                                   
                                   tags$ul(tags$li("My most-played artists were overwhelmingly women in their 20s and 30s, like me"),
                                           tags$li("Data tells whatever story you make it tell"),
                                           tags$li("Spotify's payment model probably shaped my data before I made a single choice")),
                                   
                                   p("For the full picture, head to the Essay tab. To learn more about the process, see the Pipeline tab.")
                                  
                            ),
                            hr(),
                            column(width = 12, align = "center",
                                   tags$div(
                                     style = "text-align: center; padding: 20px;",
                                     tags$img(
                                       src = "me-2.0.jpg",
                                       width = "180px",
                                       height = "180px",
                                       style = "border-radius: 50%; object-fit: cover; margin-bottom: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.2);"
                                     ),
                                     h4("Nadia Bourne"),
                                     p(tags$strong("Data Science & Technical Development")),
                                     p("Programmer, analyst, critical thinker, & creative writer. Currently pursuing a B.S. in Data Science, minor in Computer Science at UC Davis."),
                                     tags$div(
                                       style = "margin-top: 10px;",
                                       tags$a(href = "https://linkedin.com/in/nadia-bourne", 
                                              target = "_blank",
                                              class = "btn btn-primary",
                                              "LinkedIn",
                                              style = "margin-right: 15px;"),
                                       tags$a(href = "https://github.com/bournenadia26", 
                                              target = "_blank",
                                              class = "btn btn-primary",
                                              "GitHub"))))
                            
                          )),
                 
                 tabPanel("Essay",
                          fluidRow(column(width = 8, offset = 2,
                                          h2("I Analyzed My Spotify Data So You'd Think About Yours"),
                                          hr(),
                                          p("Spotify may be the only company that has successfully rebranded data stalking as fun. It’s something that’s meant to be posted the same way as a photo of somewhere you’ve been. A reflection of you! An ", em("interesting"), " and ", em("unique"), " reflection, of course. Millions of people (myself included) willingly become the distribution channel for their own surveillance."),
                                          p("When Spotify Wrapped 2025 released on December 3rd, I was as mildly excited as you probably were: I planned to check it out, maybe post it, and then completely forget about it a few weeks later. But that day I was seated in the lecture hall of one of the best classes I’ve ever had the pleasure of attending. It gave me my first experience with R, with making interactive visualizations, and with thinking critically with and about data. It hit me right there in that seat, in a sudden startling moment of awareness… This — Spotify Wrapped — is data, too. ", em("My"), " data. "),
                                          p("I wanted to quantify, in more ways than Spotify showed, what kind of listener was I? What kind of ", em("consumer"), " was I? So I extracted my data, loaded it up in R Studio, converted variable types and added new ones, conceptualized and built visualizations, just to be told what I already sort of knew:"),
                                          h4("I am an extremely average consumer."),
                                          p("Maybe the most average you have ever seen. It made me stop and think— what other factors influence a top 100, beyond a user’s individual taste? How has Spotify, as a platform, shaped the kind of music that dominated my Spotify Wrapped this year? And what can I glean about consumer trends by examining my own?"),
                                          p("So the new question I pose to you, and the one I’ve truly set out to answer is:"),
                                          tags$div(class = "callout-box", 
                                                   tags$h4("I am an extremely average consumer…"),
                                                   tags$p("…but is that entirely my fault?")),
                                          p("First, we set out to quantify and measure, in every way possible: what kind of consumer am I? I’ve already told you I’m an average one, but what exactly does that ", em("mean?"), " You’ve probably got a picture in your head already (if you checked my bio on the overview page). But it’s a picture that’s shaped by your conception of what a listener who looks like me might listen to. Isn’t that the same kind of picture Spotify would utilize to recommend music? "),
                                          p("Let’s spell it out on paper so we’re all on the same page. As of writing this, I am a 22 year old rising senior at the University of California, Davis. I am a white Hispanic woman. I program regularly, play video games, write stories… and I get most of my recommendations from actively engaging with the Spotify platform itself. I like ", em("all"), " music! Yes, even country. I regularly listen to pop, rock, metal, goth, grunge, k-pop, R&B, rap, broadway musical and video game soundtracks. I have no genre loyalty, and my top 5 artists change every year. So why did my Spotify Wrapped end up the way it did?"),
                                          p("Let’s start with the most basic of basics. Who exactly was I listening to?"),

                                          plotlyOutput("plot1", height = "600px"),
                                          tags$br(),
                                          plotlyOutput("plot7", height = "600px"),
                                          tags$br(),
                                          p("My top three songs were all made by women! You can listen to them below."),
                  
                          fluidRow(column(4,
                                          tags$iframe(src = "https://open.spotify.com/embed/track/1zhvxTuSha22nsUT5Nw8gE?utm_source=generator",
                                                      width = "100%",
                                                      height = "152",
                                                      frameborder = "0",
                                                      allowfullscreen = "",
                                                      allow = "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
                                                      loading = "lazy")),
                          column(4,
                                          tags$iframe(src = "https://open.spotify.com/embed/track/19KlZwqlT3fguP2BeHF1Q1?utm_source=generator",
                                                      width = "100%",
                                                      height = "152",
                                                      frameborder = "0",
                                                      allowfullscreen = "",
                                                      allow = "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
                                                      loading = "lazy")),
                          column(4,
                                          tags$iframe(src = "https://open.spotify.com/embed/track/5IoPnNiYAOvHHJpz13wzRL?utm_source=generator",
                                                      width = "100%",
                                                      height = "152",
                                                      frameborder = "0",
                                                      allowfullscreen = "",
                                                      allow = "autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture",
                                                      loading = "lazy"))),
                          
                                          hr(),
                                          p("Women. Mostly in their twenties and thirties, like me. Given my own demographic, you probably expected this. On some level, listeners like music they can relate to. But if you’re data-minded like myself, or very familiar with one or more of the artists that made my top 100, you might’ve realized there’s a problem with one of these graphs."),
                                          h4("What do I mean by an artist’s ‘age’?"),
                                          p("Their age when they wrote the song? Their age at the moment I wrote that code? Or when I scraped my Wrapped data? Am I updating this website every year on each artist’s birthday? How on earth did I get a singular age for Erika Henningsen and the original Broadway cast of ", em("Mean Girls?")),
                                          p("My method was this: for individual artists, I took the current age of the artist in 2025 (the year this particular Wrapped released). For groups or collaborations, I took the average age of every artist involved. I manually punched each number into my dataset, and haven’t touched it in the months since."),
                                          p("However, the method I chose was flawed. What about HUNTR/X, an entirely fictional K-pop group in a movie popular with little girls? What about Anika Noni Rose, the singer behind Tiana in Disney’s ", em("The Princess and the Frog"), " (2009), wouldn’t the age of the movie be more relevant than the age of the singer?"),
                                          p("Age is a classic demographic metric, but there’s no objective way to represent it. To paraphrase Catherine D’Ignazio and Lauren Klein in ", em("Data Feminism"), ", data is never neutral, nor does it speak for itself. I made an assumption, built a visualization on it, and made it ", em("look"), " like facts. Like ‘raw’ data. But there’s no such thing. A distribution or graphic is an opportunity to tell the kind of story you want your clients to understand."),
                          p("Let me show you in another way. Here are my top 11 artists, colored by genre:"),
                          
                          plotlyOutput("plot2", height = "600px"),
                          br(),
                          
                          p("If I’m an extremely average consumer, of ", em("course"), " my listening is dominated by pop music and its subgenres. Simple claim, backed up with evidence… but maybe you’re starting to notice the tricks I’m playing on you. Maybe you've seen through some of them, but not all."),
                          p("Before you keep reading, I want you to stop. Go back and look at that graph again. Re-read what I said. Really think about it — how subjective is this objective graphic?"),
                          p("…"),
                          p("Ready? Good."),
                          p("You probably noticed I never tell you what I mean by ‘Pop Related Subgenre’. You might’ve guessed (correctly) that I defined each artist’s genre myself. Did you realize I used similar colors for ‘Pop’ and ‘Pop Related Subgenre’ to conflate the two visually and make Pop music seem more dominant? What about how I defined ‘top 11 artists’? I used the number of their tracks that appeared in my top 100, not Spotify’s list of top artists. And 11 is an odd number to settle on isn’t it… I only chose it because artists 12 through 15 weren’t pop-related, and therefore muddied the message. Most glaringly, did you notice I broadly labeled each artist by one singular genre, yet the graphic itself counts the number of songs? An artist’s songs aren’t necessarily the same genre, or even Pop at all. For example, the vast majority of Bruno Mars’ songs on this list were collaborations with Cardi B (Hip Hop adjacent), or by Silk Sonic (R&B/Soul), yet I simplified his appearance to just ‘Pop Related Subgenre’, which is technically true, but misleading."),
                          p("So far, the tricks have been mine. But not all of them are."),
                          
                          plotlyOutput("plot3", height = "600px"),
                          br(),
                          
                          p("The trick this time comes from the platform itself, Spotify. Figure 2 of Habjanec and Bagić Babac (2025) shows song duration trending downward across decades since the shift to streaming — and you can see where it lands in 2025 is right around my own average."),
                          
                          tags$img(src = "quant-croatia.png", width = "100%"),
                          p(em("Song duration trends over decades. Reproduced from Habjanec & Bagić Babac (2025).")),
                          hr(),
                          
                          p("I primarily get my music by interacting with Spotify, or through social media like Instagram. I listen to Spotify’s recommended songs and curated playlists. When I find a new artist I like, I peruse the top 10 songs Spotify has given them… a ranking based on ", em("streams,"), " which is based on length. Additionally, consider the fact that Spotify Wrapped, the very dataset we’ve been working with this entire time, consists of a user’s top 100 tracks, AKA a number that directly correlates with decreased song length. Listening to a shorter track for the same amount of minutes as a longer one results in more plays. Of ", em("course"), " my top 100 ended up this way."),
                          p("Or at least, that’s part of the picture. Take a look at this distribution:"),
                          
                          plotlyOutput("plot4", height = "600px"),
                          br(),
                          
                          p("While the overall distribution of song lengths in my top 100 was shaped by streaming economics, song length had almost no bearing on which songs I actually chose to play over and over again. If anything, the slight positive slope suggests I replayed longer songs more, the opposite of what you might expect from someone whose dataset was built on a platform that rewards brevity."),
                          p("One of my unique tendencies as a consumer is that I’m a creature of habit. I’m a serial video re-watcher, game re-player, meal looper, and outfit repeater with a regimented daily schedule. I love the comfort of a known song. In this one regard, I would not call myself an average consumer. Yet even I hit a wall eventually."),
                          
                          plotlyOutput("plot5", height = "600px"),
                          br(),
                          plotlyOutput("plot6", height = "600px"),
                          br(),
                          
                          p("60 plays. 4 hours. At the most extreme, 119 plays or 7.36 hours. That’s it. For a crazy person like me, who pulled their own data off the platform and spent countless hours planning, creating, and analyzing her own data distributions. I spent more hours on this website than my top 5 songs total playtime combined."),
                          h4("…"),
                          h4("So. Is it my fault?"),
                          tags$div(class = "callout-box", 
                                   tags$h4("Partially, no."),
                                   tags$p("Spotify shaped the pool: the songs that exist, the lengths they run, the artists their algorithm surfaces. Within that pool, I worked with a deck they stacked.")),
                          tags$div(class = "callout-box", 
                                   tags$h4("But also; partially, yes."),
                                   tags$p("Just not in the way you’d expect. The ‘average’ you saw at the beginning of this essay? I built that. I chose the metrics, assigned the categories, picked the colors. I decided what ‘average’ looked like before I ever showed it to you.")),
                          h4("The platform influenced my listening."),
                          h2("But I influenced what you saw."),
                          br(),
                          hr(),
                          
                          br(),
                          h5("Sources Used:"),
                          tags$a(href = "https://data-feminism.mitpress.mit.edu/pub/czq9dfs5", target = "_blank", "D’Ignazio, C., & Klein, L. (2020). 6. The Numbers Don’t Speak for Themselves. In Data Feminism."),
                          br(),
                          br(),
                          tags$a(href = "https://hrcak.srce.hr/336418", target = "_blank", "Habjanec, P. & Bagić Babac, M. (2025). Quantitative Analysis of Songs With Over a Billion Streams. South Eastern European Journal of Communication, 7(1), 7–22.")
                          ))),
                 
                 tabPanel("Pipeline",
                          fluidRow(column(width = 8, offset = 2,
                                          h2("The behind-the-scenes"),
                                          hr(),
                                          p("As I explained in the essay, this project was born from the simple idea of pulling my Wrapped data and exploring it in R Studio. I used Spotifylistr to extract the data into a CSV. That CSV came with only three columns: Artist Name (a single string of names, separated by semicolons), Track Name (string of song title), and Length (a time string in HH:MM:SS format)."),
                                          p("Spotify actually has a platform specifically for developers that gives access to hidden statistics, including things that would be absolutely painstaking to do by hand, like quantifying the ‘energy’ of each song. However, in December 2025, when I was planning this project, I found out that only a few months earlier, Spotify had prohibited developer access for individuals like me."),
                                          tags$div(class = "callout-box", 
                                                   tags$h4("To quote their Feburary 6 2026 community post:"),
                                                   tags$p("“Over time, advances in automation and AI have fundamentally altered the usage patterns and risk profile of developer access, and at Spotify's current scale these risks now require more structured controls.”"),
                                                   tags$a(
                                                     href="https://developer.spotify.com/blog/2026-02-06-update-on-developer-access-and-platform-security", 
                                                     target="_blank", 
                                                     style = "color: #FFFFFF;",
                                                     "Link to full community post")
                                                   ),
                                          p("So I had to pivot. If I wanted more data, I’d have to populate it myself."),
                                          
                                          h4("Data Cleaning"),
                                          tags$ul(tags$li("Converted length from HH:MM:SS string format to numeric seconds"),
                                                  tags$li("Found and fixed an edge case where a song title contained commas, breaking the CSV structure (I had to manually peruse the file to locate it)"),
                                                  tags$li("Split semicolon-delimited artist name strings into individual entries for accurate per-artist attribution")),
                                          
                                          h4("Manual Enrichment"),
                                          tags$ul(tags$li("Play counts: entered sequentially from Spotify Wrapped's mobile playlist view for all 100 tracks"),
                                                  tags$li("Artist age: researched and populated individually; averaged across members for groups and collaborations"),
                                                  tags$li("Artist gender: coded as single letters during entry, then converted to full strings programmatically to reduce manual effort"),
                                                  tags$li("Genre: manually assigned to top 11 artists for the genre analysis"),
                                                  tags$li("Color schemes: applied manually throughout; several palettes sourced from the Wes Anderson R package")),
                                          
                                          h4("Feature Engineering"),
                                          tags$ul(tags$li("Calculated total minutes and total hours per track"),
                                                  tags$li("Created automated plotly hover descriptions via mutate() and paste0() concatenation"),
                                                  tags$li("Built secondary dataframes for specific visualizations like genre breakdowns")),
                                          
                                          h4("Deployment & Application Design"),
                                          tags$ul(tags$li("Built and deployed as a multi-tab Shiny web application hosted on shinyapps.io"),
                                                  tags$li("Wrote custom CSS for typography, layout, callout boxes, and color theming to move beyond default Shiny/Bootswatch styling"),
                                                  tags$li("Embedded interactive Spotify track previews via iframe for the three top songs"),
                                                  tags$li("Structured the app across three tabs: an overview page, the essay, and this methodology page"),
                                                  tags$li("Iterated through multiple layout configurations and visualization placements to support the essay's narrative flow"),
                                                  tags$li("Many visualizations were built and cut. Only a chosen few were picked to best serve the story")),
                                          
                                          p("You can view both the original and post-cleaning CSV’s, as well as the full R script including beta visualizations, and this website’s code on my GitHub."),
                                          br()
                                          
                          ))))

server <- function(input, output, session) {
  
  # Plot 1: Top 25 gender bar
  output$plot1 <- renderPlotly({
    
    plot_ly(data = gender_df,
            y = ~gender,
            x = ~pct,
            type = "bar",
            orientation = "h",
            marker = list(color = c(wes_colors1[1], wes_colors1[2], wes_colors1[3])),
            text = ~text2,
            hoverinfo = "text",
            textposition = "outside",
            texttemplate = "%{x:.0%}",
            textfont = global_font) |>
      layout(plot_bgcolor = "white",
             paper_bgcolor = wes_colors1[4],
             font = global_font,
             title = list(text = "<b>Women Dominated My Top 100 Tracks This Year</b><br><sub>(My Top 100 Songs, Spotify Wrapped 2025)</sub>",
                          font = global_font),
             xaxis = list(title = "Gender",
                          titlefont = global_font,
                          tickfont = global_font,
                          tickformat = ".0%",
                          range = c(0, 0.7)),
             yaxis = list(title = "",
                          titlefont = global_font,
                          tickfont = global_font),
             showlegend = FALSE,
             margin = list(t = 80, r = 50))
  })
  
  output$plot7 <- renderPlotly({
    p7 <- song_list |>
      group_by(artist_age, Arist.s..Name, text7) |>
      distinct(Arist.s..Name) |>
      ggplot(aes(x = artist_age, text = text7)) +
      geom_histogram(binwidth = 2, fill = wes_colors1[1]) +
      theme_bw()
    
    # Arrange ggplot & text into plotly
    ggplotly(p7, tooltip = "text") |>
      layout(plot_bgcolor = "white",
             paper_bgcolor = wes_colors1[4],
             font = global_font,
             title = list(text = "<b>Most Artists I Listen to Are in Their 20 & 30's</b><br><sub>(Top 100 Songs, Spotify Wrapped 2025)</sub>",
                          font = global_font),
             xaxis = list(title = "Artist Age (years)",
                          titlefont = global_font,
                          tickfont = global_font),
             yaxis = list(title = "Number of Artists",
                          titlefont = global_font,
                          tickfont = global_font),
             legend = list(font = global_font),
             margin = list(t = 100))
  })
  
  # Plot 2: Genre bar chart
  output$plot2 <- renderPlotly({
    
    plot_ly(data = artist_summary,
            x = ~reorder(Arist.s..Name, n),
            y = ~n,
            type = "bar",
            color = ~genre,
            colors = genre_colors, 
            text = ~text6,
            hoverinfo = "text") |>
      layout(plot_bgcolor = "white",
             paper_bgcolor = wes_colors2[2],
             font = global_font,
             title = list(text = "<b>I Listen to Mostly Pop and its Subgenres</b><br><sub>(Artists With at Least 3 Appearances — Top 100 Songs, Spotify Wrapped 2025)</sub>",
                          font = global_font),
             xaxis = list(title = "",
                          showticklabels = FALSE,
                          titlefont = global_font,
                          tickfont = global_font),
             yaxis = list(title = "Number of Songs",
                          titlefont = global_font,
                          tickfont = global_font),
             showlegend = TRUE,
             legend = list(title = list(text = "<b>Genre</b>"),
                           font = global_font),
             margin = list(t = 100))
  })
  
  # Plot 3: Song length histogram
  output$plot3 <- renderPlotly({
    
    p3 <- song_list |>
      ggplot(aes(x = total_sec, text = text3)) +
      geom_histogram(binwidth = 5, fill = wes_colors2[1]) +
      theme_bw()
    
    ggplotly(p3, tooltip = "text") |>
      layout(plot_bgcolor = "white",
             paper_bgcolor = wes_colors2[2],
             font = global_font,
             title = list(text = "<b>Most Songs I Listen To Are 2-4 Minutes Long</b><br><sub>(My Top 100 Songs, Spotify Wrapped 2025)</sub>",
                          font = global_font),
             xaxis = list(title = "Song Length (seconds)",
                          titlefont = global_font,
                          tickfont = global_font),
             yaxis = list(title = "Number of Songs",
                          titlefont = global_font,
                          tickfont = global_font),
             legend = list(font = global_font),
             margin = list(t = 100))
  })
  
  # Plot 4: Length vs plays scatter
  output$plot4 <- renderPlotly({
    
    p4 <- song_list |>
      ggplot(aes(x = total_sec, y = plays)) +
      geom_point(aes(text = text4),
                 color = wes_colors3[3],
                 alpha = 0.8,
                 size = 2) +
      geom_line(aes(y = fitted),
                color = wes_colors3[1],
                linewidth = 1) +
      theme_bw() +
      theme(panel.grid.minor = element_blank())
    
    ggplotly(p4, tooltip = "text") |>
      layout(plot_bgcolor = "white",
             paper_bgcolor = wes_colors3[2],
             font = global_font,
             title = list(text = "<b>Song Length Had Almost No Effect on Replay Behavior</b><br><sub>(Top 100 Songs, Spotify Wrapped 2025)</sub>",
                          font = global_font),
             xaxis = list(title = "Song Length (seconds)",
                          titlefont = global_font,
                          tickfont = global_font),
             yaxis = list(title = "Number of Plays",
                          titlefont = global_font,
                          tickfont = global_font),
             margin = list(t = 100))
  })
  
  # Plot 5: Plays histogram with cutoff
  output$plot5 <- renderPlotly({
    
    p5 <- song_list |>
      ggplot(aes(x = plays, text = text1)) +
      geom_histogram(binwidth = 2, fill = wes_colors3[3], alpha = 0.7, color = "white") +
      geom_vline(xintercept = 60,  # Add your cutoff value
                 linetype = "dashed",
                 linewidth = 0.8,
                 color = wes_colors3[1]) +
      theme_bw()
    
    ggplotly(p5, tooltip = "text") |>
      layout(plot_bgcolor = "white",
             paper_bgcolor = wes_colors3[2],
             font = global_font,
             title = list(text = "<b>Most Songs Were Not Played More than 60 Times</b><br><sub>(Cumulative play count per track — Top 100 Songs, Spotify Wrapped 2025)</sub>",
                          font = global_font),
             xaxis = list(title = "Plays",
                          titlefont = global_font,
                          tickfont = global_font),
             yaxis = list(title = "Number of Songs",
                          titlefont = global_font,
                          tickfont = global_font),
             showlegend = FALSE,
             margin = list(t = 100),
             annotations = list(list(x = 61,
                                     y = 0.75,
                                     yref = "paper",
                                     text = "60 plays",
                                     xanchor = "left",
                                     yanchor = "top",
                                     showarrow = FALSE,
                                     font = global_font)))
  })
  
  # Plot 6: Total time histogram
  output$plot6 <- renderPlotly({
    
    p6 <- song_list |>
      ggplot(aes(x = total_mins_played, text = text5)) +
      geom_histogram(binwidth = 30,
                     fill = wes_colors3[3],
                     alpha = 0.7,
                     color = "white") +
      geom_vline(xintercept = 240,
                 linetype = "dashed",
                 linewidth = 0.8,
                 color = wes_colors3[1]) +
      theme_bw() +
      theme(panel.grid.minor = element_blank())
    
    ggplotly(p6, tooltip = "text") |>
      layout(plot_bgcolor = "white",
             paper_bgcolor = wes_colors3[2],
             font = global_font,
             title = list(text = "<b>Most Songs Were Not Played for More Than Four Hours</b><br><sub>(Cumulative listening time per track — Top 100 Songs, Spotify Wrapped 2025)</sub>",
                          font = global_font),
             xaxis = list(title = "Total Minutes Listened per Song",
                          titlefont = global_font,
                          tickfont = global_font),
             yaxis = list(title = "Number of Songs",
                          titlefont = global_font,
                          tickfont = global_font),
             margin = list(t = 100),
             annotations = list(list(x = 245,
                                     y = 0.75,
                                     yref = "paper",
                                     text = "4 hours",
                                     xanchor = "left",
                                     yanchor = "top",
                                     showarrow = FALSE,
                                     font = global_font)))
  })
}

shinyApp(ui = ui, server = server)