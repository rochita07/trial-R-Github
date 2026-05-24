# git config --global user.name 'rochita07'
# git config --global user.email 'rochita.das.stat@gmail.com'


install.packages("usethis")
library(usethis)

create_github_token()

install.packages("gitcreds")
gitcreds::gitcreds_set()

# RStudio -> GitHub
library(usethis)
use_github()

# GitHub -> RStudio
# git remote add origin https://github.com/YOUR-USERNAME/YOUR-REPO-NAME.git
# git push -u origin master