ELM_ENTRYPOINT='src/Main.elm'
JS_OUTPUT='App.js'
DOCS_OUTPUT='docs.json'

REPOSITORY_ROOT=$(git rev-parse --show-toplevel)

executable=elm-make
executable_opts="--warn --output $JS_OUTPUT --docs $DOCS_OUTPUT $ELM_ENTRYPOINT"

while [[ $# -gt 0 ]]
do
key="$1"
case $key in
    -l|--live)
    executable=elm-live
    ;;
    -d|--debug)
    executable_opts="$executable_opts --debug"
    ;;
esac
shift # past argument or value
done

(cd $REPOSITORY_ROOT && $executable $executable_opts)


