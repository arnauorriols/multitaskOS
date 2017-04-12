document.addEventListener('DOMContentLoaded', function() {
    function compat(model) {
        for (props of [['thread', 'job'], ['threadQueue', 'jobQueue'], ['newThread', 'newJob']]) {
            var oldProp = props[0];
            var newProp = props[1];
            if (model.hasOwnProperty(oldProp)) {
                model[newProp] = model[oldProp];
                delete model[oldProp];
            }
        }
        for (prop of [['hotkeysPressed', []]]) {
            var key = prop[0];
            var value = prop[1];
            if (!model.hasOwnProperty(prop)) {
              model[key] = value;
            }
        }
        return model;
    }
    var STORAGE_KEY = 'MultitaskOS-Model';
    var storageContent = localStorage.getItem(STORAGE_KEY);
    var savedModel =
        storageContent ?
          compat(JSON.parse(storageContent)):
          null;
    var multitaskos = Elm.Main.embed(document.getElementById('elm-app'), savedModel);
    multitaskos.ports.persistModel.subscribe(function (model) {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(model));
    });
});
