#!/bin/bash

for i in {91..180}; do Rscript worker.R $i; done
