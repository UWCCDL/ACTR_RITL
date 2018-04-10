#!/bin/bash
echo -n "Bilingual HP: " 
ls ./bilingual/hp*.txt | wc
echo -n "Monolingual HP: "
ls ./monolingual/hp*.txt | wc
