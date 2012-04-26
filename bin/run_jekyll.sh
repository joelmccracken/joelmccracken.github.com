#!/bin/bash

#SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#cd $SCRIPT_DIR



#jekyll swallows errors that happen when cmds are run 
# if it is being served
# so sometimes want to run w/o serve

if [ "$1" = "serve" ]; then
    ~/.rvm/bin/rvm 1.9.3 do bundle exec jekyll --server --serve 9000 --auto
else
    ~/.rvm/bin/rvm 1.9.3 do bundle exec jekyll
fi
