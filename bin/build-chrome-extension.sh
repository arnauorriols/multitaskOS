CHROME_EXTENSION_DIR='chrome-extension'

REPOSITORY_ROOT=$(git rev-parse --show-toplevel)

(cd $REPOSITORY_ROOT/$CHROME_EXTENSION_DIR && find . -maxdepth 1 -type l -delete && ln -s ../* .)
