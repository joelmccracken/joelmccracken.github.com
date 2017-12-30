#!/usr/bin/env bash
set -e
docker-compose -f ./docker-compose.drafts.yml build
docker-compose -f ./docker-compose.drafts.yml down
docker-compose -f ./docker-compose.drafts.yml up
