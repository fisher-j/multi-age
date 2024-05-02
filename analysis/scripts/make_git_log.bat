rem format git log as markdown headings with list, remove duplicate dates add newline
rem to separate blocks
git log --pretty=format:"## %%cs %%n - %%s" | awk "!seen[$0]++" | awk '/^## /{print ""}1' > _git_log.md
