# News-Transaction-Mining
Basic association mining script illustrating the Transactional News-Mining approach via rvest, httr and arules in R.

# Project initialization
- 1. Run renv::init()
- 2. Run renv::activate() to create the virtual environment for the project
- 3. Run renv::update() to meet the renv.lock requirements and to make the project runable
 
Main scriot is the news_mining.R. Inserted are example URLs with a complete data-pre cleaning and reformatting to use the arules apriori algorithm to generate news topic connections for further investigation.
