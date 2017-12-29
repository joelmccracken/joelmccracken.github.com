#!/usr/bin/env bash
set -e
docker-compose -f ./docker-compose.prod.yml build
docker-compose -f ./docker-compose.prod.yml down
docker-compose -f ./docker-compose.prod.yml up emacs-org-compiler -d
docker-compose -f ./docker-compose.prod.yml jekyll build
octopress deploy
