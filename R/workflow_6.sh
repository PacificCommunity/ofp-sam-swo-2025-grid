#!/bin/bash

for i in {301..360}; do Rscript worker.R $i; done
