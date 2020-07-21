#!/usr/bin/env bash
ls *.hs|entr -c "stack exec shake"
