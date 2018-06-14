#!/usr/bin/env bash

docker run \
  --publish 3000:3000 \
  --restart=always \
  maxhallinan/ec2-hello-world
