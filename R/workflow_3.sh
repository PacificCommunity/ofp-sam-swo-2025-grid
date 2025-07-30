#!/bin/bash

for i in {121..180}; do Rscript worker.R $i; done
