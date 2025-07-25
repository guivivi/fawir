% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_scrape_pesmaster.R
\name{do_scrape_pesmaster}
\alias{do_scrape_pesmaster}
\title{Scrape PES Master data}
\usage{
do_scrape_pesmaster(url_player, url_pes_base, df_players_links, 
                    pes_skills, pes_ai, analyst_name, verbose)
}
\arguments{
\item{url_player}{PES Master url for a certain player.}

\item{url_pes_base}{PES Master baseline url.}

\item{df_players_links}{Data with the details needed to scrape.}

\item{pes_skills}{Data with the skills definition.}

\item{pes_ai}{Data with the AI playing styles.}

\item{analyst_name}{Name to identify the user when doing web scraping. 
This is a polite way to do web scraping and certify that the user 
is working as transparently as possible with a research purpose.}

\item{verbose}{Should R report information on progress? TRUE or FALSE.}
}
\value{
A data frame with the player data.
}
\description{
Obtain numeric and text descriptions from football players included in 
the PES Master database (\url{https://www.pesmaster.com/efootball-2022/}).
}
\note{
This function is devoted to friendly web scraping. It respects the 
information provided by the PES Master website:
\url{https://www.pesmaster.com/robots.txt} and 
\url{https://www.pesmaster.com/privacy-policy/}

Please be polite and respectful when doing web scraping.
Also include the command \code{Sys.sleep(x)} to pause between 
requests for x seconds if data are obtained iteratively.
}
\examples{
\dontrun{
library(dplyr)

# Prepare the data with the details needed to scrape:
df_players_links <- data.frame(league = "Premier League",
                               team = "Arsenal",
                               position = "CB",
                               name = "William Saliba",
                               link = "/william-saliba/efootball-2022/player/126689/")
                               
# Prepare the data with the skills definition:                                
pes_skills <- data.frame(english = c("Heading", "Weighted Pass", "Man Marking", 
                                     "Interception", "Blocker", "Aerial Superiority", 
                                     "Acrobatic Clearance"),
                         description = c("Improve the accuracy of headers", 
                                         "Apply back-spin when playing lofted passes", 
                                         "Stick to chase down an opponent", 
                                         "Better interception skills", 
                                         "Player’s chances to block passes and shots", 
                                         "Player’s chances to win aerial balls", 
                                         "Clear the ball even from awkward positions"))                                

# Prepare the data with the AI playing styles:
pes_ai <- data.frame(english = "Long Ball Expert",
                     description = "A player who frequently plays the long ball")

url_player <- df_players_links \%>\% pull(link)

url_pes_base <- "https://www.pesmaster.com"
analyst_name <- "Guillermo Vinue"

data_pes <- do_scrape_pesmaster(url_player, url_pes_base, df_players_links, 
                                pes_skills, pes_ai, analyst_name, TRUE)
}                           

}
\author{
Guillermo Vinue
}
