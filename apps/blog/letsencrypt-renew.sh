#!/bin/sh
cd letsencrypt
git pull
./letsencrypt-auto renew --quiet
