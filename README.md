# Materials for workshop "A Primer to Web Scraping and Text Mining with R"


This repository provides materials for the workshop "A Primer to Web Scraping and Text Mining with R". 

## Course contents

The rapid growth of the World Wide Web over the past two decades tremendously changed  the way we share, collect and publish data. Firms, public institutions and private users provide every imaginable type of information and new channels of communication generate vast amounts of data on human behavior. What was once a fundamental problem for the social sciences -- the scarcity and inaccessibility of observations -- is quickly turning into an abundance of data. This turn of events does not come without problems. For example, traditional techniques for collecting and analyzing data may no longer suffice to overcome the tangled masses of data. One consequence of the need to make sense of such data has been the inception of "data scientists", who sift through data and are greatly sought after by research and business alike.

But how to efficiently collect data from the Internet and process the collected (text) data with statistical software? On the first day, we will learn how to scrape content from static and dynamic web pages, connect to APIs from popular web services to read out and process user data, and set up automatically working scraper programs. The second day is focused on quantitative text analysis. In particular, we learn how to work with text data in R, supervised text classification, and unsupervised topic modeling.


## Date and Venue

July 16/17, 2018, Geschwister-Scholl-Institut, LMU Munich


## Instructor

Simon Munzert ([website](https://simonmunzert.github.io), [Twitter](https://twitter.com/simonsaysnothin))


## Requirements

This course assumes prior experience using R. Please bring a laptop with the latest version of R and Rstudio installed (see more below for the technical setup). 


## Time schedule


***Day 1***

|  Time | Topic |
|-------------------------|-------------------------------|
| Mon, 09.00 - 09.45 | Introduction; a first encounter with the Web using R |
| Mon, 09.45 - 10:45 | Regular expressions and string manipulation |
| Mon, 10.45 - 11.00 | Coffee break |
| Mon, 11.00 - 12.15 | Scraping static webpages I |
| Mon, 12.15 - 13.30 | Lunch break|
| Mon, 13.30 - 14.30 | Scraping static webpages II |
| Mon, 14.30 - 15.30 | Scraping dynamic webpages |
| Mon, 15.30 - 15.45 | Coffee break |
| Mon, 15.45 - 17.00 | Tapping APIs and gathering social media data |

***Day 2***

|  Time | Topic |
|-------------------------|-------------------------------|
| Tue, 09.00 - 10.15 | Scraping ethics and workflow |
| Tue, 10.15 - 10.30 | Coffee break |
| Tue, 10.30 - 12.15 | Working with text data in R |
| Tue, 12.15 - 13.30 | Lunch break|
| Tue, 13.30 - 14.15 | Sentiment analysis and (dis)similarity measures |
| Tue, 14.15 - 15.30 | Supervised classification |
| Tue, 15.30 - 15.45 | Coffee break |
| Tue, 15.45 - 17.00 | Unsupervised topic modeling |




## Accompanying book
Together with Christian Rubba, Peter Meissner, and Dominic Nyhuis, I've written a book on [Automated Data Collection with R](http://r-datacollection.com). Participants of the course might find it useful to consult it as further reading after the course. As a documentation of the course work, this code archive and the accompanying reader is fully sufficient though.


## Technical setup for the course

Please make sure that the current version of R is installed. If not, update from here: [https://cran.r-project.org/](https://cran.r-project.org/)

Obviously, feel free to choose the coding environment you feel most comfortable with. I'll use RStudio in the course. You might want to use it, too: [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)



**Online resources**

| Area | URL | Short description |
|---------------------------|-------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| Web technologies, general | http://www.w3.org/ | Base of the World Wide Web Consortium (W3C), also provides access to standards and drafts of web technologies |
|  | http://w3schools.com | Great tutorial playground to learn web technologies interactively |
|  | https://w3techs.com/technologies | Overview of all kinds of web technologies |
| XML and XPath | http://selectorgadget.com/ | Probably the most useful tool for generating CSS selectors and XPath expressions with a simple point-and-click approach |
|  | http://www.xmlvalidation.com/ | Online XML validator |
|  | http://www.rssboard.org/ | Information about the Really Simple Syndication standard |
| CSS selectors | http://www.w3schools.com/cssref/css_selectors.asp | W3 Schools CSS reference |
|  | http://flukeout.github.io/ | Interactive CSS selectors tutorial |
| JSON | http://www.json.org/ | Base of the JSON data interchange standard |
|  | http://jsonformatter.curiousconcept.com | Formatting tool for JSON content |
| HTTP | http://httpbin.org | HTTP Request and Response Service; useful to debug HTTP queries |
|  | http://useragentstring.com | Tool to figure out what's behind a User-agent string |
|  | http://curl.haxx.se/libcurl/ | Documentation of the libcurl library |
|  | http://www.robotstxt.org/ | Information about  robots.txt |
| OAuth | http://oauth.net | Information about the Oauth authorization standard |
|  | http://hueniverse.com/oauth | Great overview of Oauth 1.0 |
| Database technologies | http://db-engines.com | Compendium of existing database management systems |
|  | https://www.thoughtworks.com/insights/blog/nosql-databases-overview | Intro to NoSQL databases |
| Regular expressions | http://www.pcre.org/ | Description of Perl Compatible Regular Expressions |
|  | https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html | Regular Expressions as used in base R |
|  | http://regexone.com/ | Online regex tutorial |
|  | http://regex101.com | Regex testing environment |
|  | http://www.regexplanet.com/ | Another regex testing environment |
|  | http://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454 | The truth about HTML parsing with regular expressions |
|  | https://www.youtube.com/watch?v=Cv2DpwSCgRw | Yes, there's a regex song |
| Selenium | http://docs.seleniumhq.org | Selenium documentation |
| APIs | http://www.programmableweb.com/apis | Overview of many existing web APIs |
|  | http://ropensci.org/ | Platform for R packages that provide access to science data repositories |
| R | http://cran.r-project.org/web/views/WebTechnologies.html | CRAN Task View on Web Technologies and Services - useful to stay in the loop of what's possible with R |
|  | http://tryr.codeschool.com/ | An excellent interactive primer for learning R |
|  | http://www.r-bloggers.com/ | Blog aggregator which collects entries from many R-related blogs |
|  | http://planetr.stderr.org | Blog aggregator providing information about new R packages and scientific work related to R |
|  | http://dirk.eddelbuettel.com/cranberries/ | Dirk Eddelbuetttel's CRANberries blog keeps you up-to-date on new and updated R packages |
|  | http://www.omegahat.org/ | Home of the "Omega Project for Statistical Computing"; documentation of many important R packages dealing with web-based data |
|  | https://github.com/ropensci/user2016-tutorial#extracting-data-from-the-web-apis-and-beyond | Web API tutorial from useR 2016 conference by Scott Chamberlain, Karthik Ram, and Garrett Grolemund |
| General web scraping | http://r-datacollection.com | Probably the most useful resource of all |
|  | [http://www.stata-datacollection.com](https://www.youtube.com/watch?v=WOdjCb4LwQY) | Now let's see if that works… |
| Legal issues | http://www.eff.org/ | Electronic Frontier Foundation, a non-profit organisation which advocates digital rights |
|  | http://blawgsearch.justia.com/ | Search engine for law blogs -- useful if you want to stay informed about recent jurisdiction on digital issues |
|  | http://en.wikipedia.org/wiki/Web_scraping | See the section on "Legal issues" |



