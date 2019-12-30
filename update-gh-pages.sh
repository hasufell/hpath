#!/bin/bash

SOURCE_BRANCH="master"
TARGET_BRANCH="gh-pages"
REPO="https://${GH_TOKEN}@github.com/hasufell/hpath"

if [ -z "${UPDATE_GH_PAGES}" ] ; then
	exit 0
fi


# Pull requests and commits to other branches shouldn't try to deploy,
# just build to verify
if [ "$TRAVIS_PULL_REQUEST" != "false" -o "$TRAVIS_BRANCH" != "$SOURCE_BRANCH" ]; then
    echo "Skipping docs deploy."
    exit 0
fi


cd "$HOME"
git config --global user.email "travis@travis-ci.org"
git config --global user.name "travis-ci"
git clone --branch=${TARGET_BRANCH} ${REPO} ${TARGET_BRANCH} || exit 1

# docs
cd ${TARGET_BRANCH} || exit 1
echo "Removing old docs."
rm -rf *
echo "Adding new docs."
cp -rf "${TRAVIS_BUILD_DIR}"/dist-newstyle/build/x86_64-linux/ghc-*/hpath-*/doc/html/hpath/* . || exit 1

# If there are no changes to the compiled out (e.g. this is a README update)
# then just bail.
if [ -z "`git diff --exit-code`" ]; then
    echo "No changes to the output on this push; exiting."
    exit 0
fi

git add -- .

if [[ -e ./index.html ]] ; then
	echo "Commiting docs."
	git commit -m "Lastest docs updated

travis build: $TRAVIS_BUILD_NUMBER
commit: $TRAVIS_COMMIT
auto-pushed to gh-pages"

	git push origin $TARGET_BRANCH
	echo "Published docs to gh-pages."
else
	echo "Error: docs are empty."
	exit 1
fi

