#!/bin/sh

emacs -Q --daemon -L /org-1.2.6/lisp -l /org-convert.el -f start-compile-server
while true; do
    echo "sleeping..."
    sleep 100 || exit 10;
done
