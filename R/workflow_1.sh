#!/bin/bash

for i in {1..90}; do Rscript worker.R $i; done
