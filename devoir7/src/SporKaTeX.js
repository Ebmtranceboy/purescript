exports.render = function (content){
    return function (node){
    return function(){
      katex.render(content,node);
    };
  };
};

exports.renderToString = function (content){
  return function (){
    return katex.renderToString(content);
  };
};
