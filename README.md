# spry

The Spry dashboard will live here and allow users with access to run the dash on their local. Before we get started, you must first have R installed. Please follow these instructions:
1. Go to this site(Windows: https://cran.r-project.org/bin/windows/base/, Mac: https://cran.r-project.org/bin/macosx/) and download R. Make sure to select the one that is compatible with your computer. The downloads are in the links in blue (I know the site looks super old).
2. Follow the instructions until you drag the R icon into your Applications folder. 

Alright, CS Dashboard users! To get the dashboard to your local, please follow these steps:

1. In your terminal:
`git clone https://github.com/wangmallory/spry`
2. After you've cloned the repo: `cd spry`
3. Once you're in the spry directory: `sh run_CSshiny.sh`
This should cause your terminal to run a bunch of stuff but at the end you will see something like:
`Listening on http://127.0.0.1:5830`
Copy and paste the http address (something like `http://127.0.0.1:5830`) into a browser and voila!

If this is your first time accessing the dash, you may need to manually install some packages into `R`. Please do the following:
1. Open R.
2. Copy, paste, and run the following into R:
   - `install.packages("DBI")`
   - `install.packages("DT")`
   - `install.packages("RPostgreSQL")`
   - `install.packages("shinydashboard")`
   - `install.packages("hrbrthemes")`
   - `install.packages("tidyr")`
   - `install.packages("dplyr")`
   - `install.packages("ggplot2")`
3. Open your terminal and make sure you are in the Spry folder: `cd spry`
4. Once you're in the spry directory: `sh run_CSshiny.sh`
From there, you should get the http link and use it in your browser!

If I ever email you that you need to update your local, please do the following in your terminal:
1. Update your local repo: 'git pull'
2. Run your shell as you've done before `sh run_CSshiny.sh` -- make sure that you're in the repo first `cd spry`

If you have any questions, please reach out to me, Mallory via email or Slack! Happy dashboarding!
