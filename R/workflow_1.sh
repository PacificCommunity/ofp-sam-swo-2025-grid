#!/bin/bash

for i in {1..60}; do Rscript worker.R $i; done
