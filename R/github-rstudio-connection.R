install.packages("usethis")
library(usethis)

create_github_token()

install.packages("gitcreds")
gitcreds::gitcreds_set()


library(usethis)
use_github()
