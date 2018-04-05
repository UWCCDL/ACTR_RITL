#!/bin/bash

sed -i 's/INCORRECT/difficult/g' $1
sed -i 's/CORRECT/easy/g' $1
