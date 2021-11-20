#!/bin/bash

DATE=`date`

# Deploy the generated documentation to github pages.
mv docs .docs
git checkout gh-pages
git rm -rf docs
mv .docs docs
git add docs
git commit -m "Deploy docs on $DATE"
git push
git checkout -
