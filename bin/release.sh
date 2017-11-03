version=$1

if [[ -z $version ]]
then
    echo "USAGE: release.sh '<version>'"
    exit 1
fi

git checkout master


sed -i -r -e "s/\"version\": \"[0-9]+\.[0-9]+\.[0-9]+\"/\"version\": \"$version\"/g" elm-package.json
sed -i -r -e "s/\"version\": \"[0-9]+\.[0-9]+\.[0-9]+\"/\"version\": \"$version\"/g" chrome-extension/manifest.json

./bin/build.sh && ./bin/build-chrome-extension.sh

git commit -a -m "Bump version to $version"
git tag -a $version -m $version
git push origin master --follow-tags

git checkout gh-pages
git merge --no-edit master
./bin/build.sh
git commit -a -m "Build $version"
git push origin gh-pages

git checkout master


