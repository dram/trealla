#!/bin/sh
git clone https://github.com/letsencrypt/letsencrypt
cd letsencrypt
./letsencrypt-auto webroot --agree-tos --email admin@DOMAIN -w ./localhost --domains DOMAIN,www.DOMAIN
