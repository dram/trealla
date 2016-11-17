#!/bin/sh
git clone https://github.com/letsencrypt/letsencrypt
cd letsencrypt
./letsencrypt-auto certonly --stand-alone --agree-tos --email admin@DOMAIN --domains DOMAIN,dev.DOMAIN
