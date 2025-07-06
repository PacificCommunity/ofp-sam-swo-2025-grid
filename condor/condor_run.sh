#!/bin/bash

export PATH=.:$PATH
tar -xzf Start.tar.gz
chmod 755 ss3
ss3
tar -czf End.tar.gz --exclude '*.tar.gz' --exclude '_condor_*' *
