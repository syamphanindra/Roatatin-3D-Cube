module Main where
import Control.Monad.Eff (Eff)
import Helper
import DOM.Event.Types (Event)
import Data.Maybe (Maybe(..))
import Prelude
import FFI.Util (property, setProperty)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, fillRect, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, stroke, beginPath, closePath)
import Partial.Unsafe (unsafePartial)
foreign import addeventListener ::forall e. CanvasElement -> String -> (Event -> Eff(canvas :: CANVAS|e) Unit )-> Eff(canvas :: CANVAS|e) Unit
foreign import refresh ::forall e. Context2D ->  (Context2D-> Eff (canvas ::CANVAS|e)Unit) -> Eff( canvas :: CANVAS|e) Unit
foreign import logMe :: forall a. a -> Unit


onmouseDown:: forall e.Event -> Eff(canvas :: CANVAS|e)Unit
onmouseDown e = do
	_ <- pure $ setProperty dragging "0" true
	pure unit
onmouseUp :: forall e. Event -> Eff(canvas::CANVAS|e) Unit
onmouseUp e = do
	_ <- pure (setProperty dragging "0" false)
	pure unit
onmouseMove :: forall e. Event -> Eff(canvas :: CANVAS|e) Unit
onmouseMove e  = do
	let (diffx :: Number) = property e "clientX"
	let (diffy :: Number) = property e "clientY"
	if(property dragging "0" ) then do
		let (presx:: Number) = property previousCursor "0"
		let (prey :: Number) = property previousCursor "1"
		let (newX :: Number ) = (presx-diffx)
		let (newY :: Number) = (prey-diffy)
		_ <- pure $ setProperty presentCursor "0" newX
		_ <- pure $ setProperty presentCursor "1" newY
		let ac = (property accelaration "0") + 4.0
		_ <-pure   $ setProperty accelaration "0" ac
		_ <- pure $ setProperty previousCursor "0" diffx
		_ <- pure $ setProperty previousCursor "1" diffy
    -- _ <- pure $ logMe "ac"
    -- _ <- pure $ logMe ac
		pure unit
		else do
			pure unit
	_ <- pure $ setProperty previousCursor"0" diffx
	_ <- pure $ setProperty previousCursor "1" diffy
	pure unit

drawLine :: Context2D -> XY -> XY -> Eff(canvas :: CANVAS) Unit
drawLine ctx (XY ab) (XY cd) = do
	_ <- setStrokeStyle "#000000" ctx
	_ <- beginPath ctx
	_ <- moveTo ctx ab.x  ab.y
	_ <- lineTo ctx cd.x cd.y
	_ <- closePath ctx
	_ <- stroke ctx
	pure unit

drawcube ::forall eff. Context2D -> Number -> Number -> Eff(canvas:: CANVAS) Unit
drawcube  ctx  (e::Number) (f::Number) =  do
	clearScreen ctx
	let v0 = convertDimension $ totalrotate (XYZ { x: -1.0, y: 1.0, z: 1.0 }) e f
	let v1 = convertDimension $ totalrotate (XYZ { x: 1.0, y: 1.0, z: 1.0}) e f
	let v2 = convertDimension $ totalrotate (XYZ { x: 1.0, y: -1.0, z: 1.0}) e f
	let v3 = convertDimension $ totalrotate (XYZ { x: -1.0, y: -1.0, z: 1.0 })  e f
	let v4 = convertDimension $ totalrotate (XYZ { x: -1.0, y: 1.0, z: -1.0 }) e f
	let v5 = convertDimension $ totalrotate (XYZ { x: 1.0, y: 1.0, z: -1.0 }) e f
	let v6 = convertDimension $ totalrotate (XYZ { x: 1.0, y: -1.0, z: -1.0 }) e f
	let v7 = convertDimension $ totalrotate (XYZ { x: -1.0, y: -1.0, z: -1.0 }) e f
     -- Back face

	drawLine ctx v0 v1
	drawLine ctx v1 v2
	drawLine ctx v2 v3
	drawLine ctx v3 v0

     --front face
	drawLine ctx v4 v5
	drawLine ctx v5 v6
	drawLine ctx v6 v7
	drawLine ctx v7 v4

     -- left Top face
	drawLine ctx v4 v0
	drawLine ctx v0 v3
	drawLine ctx v3 v7
	drawLine ctx v7 v4
     --right top
	drawLine ctx v1 v2
	drawLine ctx v2 v6
	drawLine ctx v6 v5
	drawLine ctx v5 v1
     --bottom
	drawLine ctx v0 v1
	drawLine ctx v1 v5
	drawLine ctx v5 v4
	drawLine ctx v4 v0
     --bottom other
	drawLine ctx v3 v2
	drawLine ctx v2 v6
	drawLine ctx v6 v7
	drawLine ctx v7 v3
	pure unit

clearScreen :: Context2D -> Eff(canvas :: CANVAS) Unit
clearScreen ctx = do
	_ <- pure $ logMe "Clear Screen"
	_ <- setFillStyle "#ffffff" ctx
	_ <- fillRect ctx ({x:0.0 ,y:0.0 ,w:700.0 ,h:700.0})
	pure unit

main ::forall e. Eff (canvas:: CANVAS) Unit
main = void $ unsafePartial do
	Just canvas <- getCanvasElementById "canvas"
	ctx <- getContext2D canvas
	let r1 =  0.0
	let r2 =  0.0
	addeventListener canvas "mouseup" onmouseUp
	addeventListener canvas "mousedown" onmouseDown
	addeventListener canvas "mousemove" onmouseMove
	drawcube ctx 0.0 0.0
	refresh ctx redraw
	pure unit

currentAngle:: Array Number
currentAngle = [0.0,0.0]

redraw :: forall e.Context2D -> Eff(canvas :: CANVAS)  Unit
redraw ctx  = do
	let (r :: Number) = property presentCursor "0"
	let (p :: Number ) = property presentCursor "1"
	let (ac :: Number) = property accelaration "0"
	if (ac > 0.0) then do
		_ <- pure (setProperty accelaration "0" (ac - 2.0))
		_ <- pure (setProperty currentAngle "0" (((property currentAngle "0") + p) ))
		_ <- pure (setProperty currentAngle "1" ((property currentAngle "1") + r))
		let (ro::Number) = property currentAngle "0"
		let (po::Number) = property currentAngle "1"
		_ <- pure $ logMe ("ro po")
		_ <- pure $ logMe (ro)
		_ <- pure $ logMe (po)
		drawcube ctx ro po
		else pure unit
