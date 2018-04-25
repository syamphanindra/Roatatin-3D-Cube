module Helper where
import Prelude 
import Math (cos, pi, sin)

newtype XY = XY
	{ x::Number
	, y::Number
	}
newtype XYZ = XYZ
	{ x :: Number
	,y :: Number
	,z :: Number
	}
rad :: Number -> Number
rad ang = ang * pi /180.0

dragging :: Array Boolean
dragging = [false]

accelaration :: Array Number
accelaration = [0.0]

previousCursor :: Array Number
previousCursor = [0.0,0.0]

presentCursor :: Array Number
presentCursor = [0.0,0.0]
--let x y z be u v w

type Angle = Number
rotateOverX :: XYZ -> Angle -> XYZ
rotateOverX (XYZ ve) (tet) = do
	let teta = rad tet
	let u= ve.x
	let v=ve.y*cos(teta)-ve.z*sin(teta)
	let w =ve.y*sin(teta)+ve.z*cos(teta)
	XYZ {
		x:u,y:v,z:w
	}
rotateOverY :: XYZ -> Angle -> XYZ
rotateOverY (XYZ ve) ( tet) = do
	let teta = rad tet
	let u= ve.z*sin(teta)+ve.x*cos(teta)
	let v=ve.y
	let w =ve.z*cos(teta)-ve.x*sin(teta)
	XYZ {
		x:u,y:v,z:w
	}
totalrotate :: XYZ -> Angle -> Angle -> XYZ
totalrotate (XYZ a) (xe) ( ye) = rotateOverY (rotateOverX (XYZ a) (xe)) ( ye)

convertDimension :: XYZ -> XY
convertDimension (XYZ a) = XY
	{ x: (a.x/ (a.z + 4.0)) * 300.0 + 350.0
	, y:(-a.y/ (a.z + 4.0)) * 300.0 + 350.0
	}
