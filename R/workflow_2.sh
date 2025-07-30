#!/bin/bash

for i in {61..120}; do Rscript worker.R $i; done
