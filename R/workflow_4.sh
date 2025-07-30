#!/bin/bash

for i in {271..360}; do Rscript worker.R $i; done
