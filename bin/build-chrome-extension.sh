CHROME_EXTENSION_DIR='chrome-extension'

REPOSITORY_ROOT=$(git rev-parse --show-toplevel)

(cd $REPOSITORY_ROOT/$CHROME_EXTENSION_DIR && find . -maxdepth 1 -type l -delete && ln -s ../* . && rm -rf $CHROME_EXTENSION_DIR && rm -rf docs && rm -rf dist/ && mkdir dist && cd .. && zip -r $CHROME_EXTENSION_DIR/dist/multitaskos.zip $CHROME_EXTENSION_DIR)
