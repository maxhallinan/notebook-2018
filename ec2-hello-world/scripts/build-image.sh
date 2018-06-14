#!/usr/bin/env bash

BASE_TAG='maxhallinan/ec2-hello-world'
GIT_TAG="$BASE_TAG:$(git rev-parse --short HEAD)"
LATEST_TAG="$BASE_TAG:latest"
docker build --tag $GIT_TAG .
docker tag $GIT_TAG $LATEST_TAG
docker push $GIT_TAG
docker push $LATEST_TAG
