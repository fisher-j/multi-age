git log --pretty=format:"## %%cs %%n - %%s" | awk "!seen[$0]++" | awk '/^## /{print ""}1' > _git_log.md
