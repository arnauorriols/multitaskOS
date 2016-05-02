var KEY = 'multitaskOS-state';
Elm.Native.Main = {
  make : function make(elm) {
    elm.Native = elm.Native || {};
    elm.Native.Main = elm.Native.Main || {};
    if (elm.Native.Main.values) return elm.Native.Main.values;
    var Maybe = Elm.Maybe.make(elm);
    return elm.Native.Main.values = {
      persistModel: function (model) {
        localStorage.setItem(KEY, JSON.stringify(model));
        return model;
      },
      retrievePersistedModel: function () {
        var model = localStorage.getItem(KEY);
        return model ?
          Maybe.Just(JSON.parse(model)):
          Maybe.Nothing;
      }
    }
  }
};
