#!/bin/bash

for i in {181..270}; do Rscript worker.R $i; done
