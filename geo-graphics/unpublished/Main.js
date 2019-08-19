exports.setAttribute = function (name) {
  return function (value) {
    return function (element) {
      return function () {
        element.setAttribute(name, value);
        return {};
      };
    };
  };
};


exports.setBodyBackground = function (name) {
      return function () {
        document.querySelector("body").setAttribute("style", "background:" + name);
        return {};
  };
};
