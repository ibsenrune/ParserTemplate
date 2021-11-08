#!/bin/bash

targetname=$1
targetdir="../$targetname"

#Copy files to target directory
mkdir $targetdir
cp ./{*.fs,*.fsproj,.gitignore} $targetdir

cd $targetdir
mv *.fsproj "$targetname.Console.fsproj"
