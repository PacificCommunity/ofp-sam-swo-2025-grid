#!/bin/bash

for i in {181..240}; do Rscript worker.R $i; done
