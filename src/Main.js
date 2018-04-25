"use strict";

/*
    1) Animation
    2) Mouse events

    1)=============
    Callback => invoke callback repeatedly
    Callback :: forall e. Context2D -> Number -> Eff (canvas :: CANVAS | e) Unit

    foreign import refresh :: Context2D -> (Context2D -> Number -> Eff(canvas :: CANVAS) Unit) -> Eff(canvas :: CANVAS)Unit
*/

exports.refresh = function(ctx) {
  return function(callback) {
    function loop(timeStamp) {
      callback(ctx)();
      window.requestAnimationFrame(loop);
    }

    window.requestAnimationFrame(loop);
    return function () {}
  }
}

exports.logMe = function (x) {
  console.log(x);
  return {};
}

// foreign import addEventListener :: CanvasElement -> String ->( Event -> Eff(canvas :: CANVAS) Unit)->Eff(canvas :: CANVAS)Unit
exports.addeventListener = function (ctx){
    return function (eventType) {
        return function(callback) {
            function eventHandler(e) {
                callback(e)();
                // console.log(e);
            }
          ctx.addEventListener(eventType, eventHandler);
            return function () {}
        }
    }
}
