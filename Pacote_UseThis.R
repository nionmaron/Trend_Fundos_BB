
library(usethis)

# Especificar local de atualização
#usethis::use_git_remote("origin", url = "https://github.com/nionmaron/Extracting-Color-Palettes-With-R.git", overwrite = TRUE)

# abrir local
usethis::browse_github()

# Obter informações sobre o estado do repositório Git
git_sitrep()

# Obter o diretório-raiz do projeto atual
usethis::proj_get()

# Inicializar o repositório Git
usethis::use_git()

# Associar o diretório do projeto a um repositório GitHub
usethis::use_github()

# Criar read.me
usethis::use_readme_md()

#usethis::use_mit_license("My Name")

# pens global (user-level) gitignore file and ensures its path is declared in your global Git config.
usethis:: edit_git_ignore()

# opens RStudio's preference file.
usethis::edit_rstudio_prefs()

# 
usethis::edit_git_config()

# opens .Renviron
usethis::edit_r_environ()

usethis::pr_init(branch)

system("git push origin master")

usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)
pr_init(branch)
usethis::pr_fetch()
usethis::pr_init()
usethis::pr_init(branch = "teste")

usethis::pr_fetch()

pr_resume(branch = NULL)
pr_finish(number = NULL, target = c("source", "primary"))