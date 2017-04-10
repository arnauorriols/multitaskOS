ELM_ENTRYPOINT='src/Main.elm'
JS_OUTPUT='App.js'
DOCS_OUTPUT='docs.json'

REPOSITORY_ROOT=$(git rev-parse --show-toplevel)

(cd $REPOSITORY_ROOT && elm-make --warn --output $JS_OUTPUT --docs $DOCS_OUTPUT $ELM_ENTRYPOINT)
