# Spatial Economics
# 
# Folder structure: 
#   
# R-folder all codes:  e.g. data preparation, testing etc.
# 
# data folder save the data, in gitignore folder we specified that data will be
# ignored and not pushed on github, so store data locally and specify it in the data preparation
# 
# will work like this:   save(dataset_to_save, file='./data/name_of_dataset_in_folder.rda',compress = 'xz') 
# where .rda and compress will specify the data format
# we need all to execute this code, to have the data locally in our projects
#    
#    if you want to commit follow these steps:  1: save the project
#                                               2: click in the top bar in r-studio on git
#                                               3: click commit
#                                               4: comment the commit
#                                               5: click on push(right top corner in the commit window)
#                                               
#                                               6: everytime you start working on the project click on the git bar and push
#                                               so you will get the most up-to-date version of the project
#                                               
#                                               alternative: in the r-window where you find the environment you will also find 
#                                               a git tab, you can also use this