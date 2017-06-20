module railway_control

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

derive class iTask Network
derive class iTask Position
derive class iTask Orientation
derive class iTask Section
derive class iTask Point
derive class iTask TempSection
derive class iTask TempPoint
derive class iTask Train
derive class iTask TrainState

:: TempSection =
	{ sLabels :: String
	, sPositions :: Position
	, sLeftSignals :: Bool
	, sRightSignals :: Bool
	}

:: TempPoint = 
	{ pLabels :: String
	, pPositions :: Position
	, pOrientations :: Orientation
	}
	
// if sAct is True than signal is green

:: Section =
	{ sLabel :: String
	, sPosition :: Position
	, sLeftSignal :: Bool
	, sRightSignal :: Bool
	, sAct :: Bool
	}
	
// if pAct is True than path to NE, SE, SW or NW is enabled instead of regular straight forward path on Point element
	
:: Point = 
	{ pLabel :: String
	, pPosition :: Position
	, pOrientation :: Orientation
	, pAct :: Bool
	}
	
:: Position = {x :: Int, y :: Int}

:: Orientation = NE | SE | SW | NW

:: Network =
	{ sections :: [Section]
	, points :: [Point]
	, click :: Bool
	, trains :: [Train]
	}
	
:: Train =
	{ driver :: String
	, current :: Position
	, target :: Position
	, toRight :: Bool
	, trainState :: TrainState}
	
:: TrainState = Wait | Go | Arrived | Derailed

//CONSTANTS

width  = 90.0
height = 90.0
ratio  = 0.8
radius = 15.0
lineWidth = 3.0
wheelSize = 5.0
wheelSpaceSize = 2.0
tWidth = 20.0
tHeight = 15.0
ne = NE 
nw = NW
sw = SW
se = SE
wait = Wait
go = Go
arrived = Arrived
derailed = Derailed

//*****************************************************************************

network :: Shared Network
network = sharedStore "Network" {sections = [], points = [], click = False, trains = []}

showNet :: Task Network
showNet = viewSharedInformation "Network" [] network

points :: Shared [Point]
points = sharedStore "Points" []

updPoint :: Task [Point]
updPoint = viewInformation "Update" [S \a->a] []

:: ViewAs a = E.v: S (a->v)

updNet :: [Point] -> Task Network
updNet p = upd (\n.{sections = n.sections
						,points = p
						,click = n.click
						,trains = n.trains})
						network

play :: Task Network
play = (updPoint -|| showNet) ||- updNet []

//*****************************************************************************

Start :: *World -> *World
Start world = startEngine [publish "/" (WebApp []) (\_ -> start)] world

start :: Task Network
start = (get network) >>= \n -> (startTask n.trains)

startTask :: [Train] -> Task Network
startTask t = viewInformation "Select role" [] "" 
				>>* [ OnAction (Action "Designer" []) (always designerTask)
					, OnAction (Action "Controller" []) (always controllerTask)
					] ++ trainActions t
					
trainActions :: [Train] -> [TaskCont String (Task Network)]
trainActions trains = [createActionFor t \\ t <- trains]

createActionFor :: Train -> (TaskCont String (Task Network))
createActionFor t = (OnAction (Action ("Driver " +++ t.driver) []) (always (driverTask t)))

driverTask :: Train -> Task Network
driverTask t = (driveTrain t) -||- imageTask False
				
driveTrain :: Train -> Task Network
driveTrain t = viewInformation ("Train driven by " +++ t.driver) [] t
				>>*  [ OnAction (Action "Go" []) (always (moveTrain t))]

moveTrain :: Train -> Task Network
moveTrain t = upd (\n.{sections = n.sections
					 , points = n.points
					 , click = False
					 , trains = changeTrains t n.trains n.sections n.points}) 
					 network
					 >>= \_.driveTrain t

changeTrains :: Train [Train] [Section] [Point] -> [Train]
changeTrains t trains sections points = [checkTrain t tr sections points \\ tr <- trains]

checkTrain :: Train Train [Section] [Point] -> Train
checkTrain t tr sections points
	| t.driver <> tr.driver 	= tr
	| otherwise 				= move t sections points
		
//After compiling I have a warning (alternative will never match) here that is connected to case analysis
//I am not sure why but I think it is because compiler thinks that "pOrientation = ne" and "pOrientation = se" is the same thing
move :: Train [Section] [Point] -> Train
move t s p
	| t.toRight 	= case getSection {x=pos.x+1, y=pos.y} s of
						Just {sLabel = _, sPosition = _, sLeftSignal = _, sRightSignal = _, sAct = _} = newTrain t {x=pos.x+1,y=pos.y} Go
						_ 	= case getPoint {x=pos.x+1, y=pos.y} p of	
								Just {pLabel = _, pPosition = _, pOrientation = _, pAct = False}  = newTrain t {x=pos.x+2,y=pos.y} Go
								Just {pLabel = _, pPosition = _, pOrientation = ne, pAct = True}  = newTrain t {x=pos.x+2,y=pos.y+1} Go
								Just {pLabel = _, pPosition = _, pOrientation = se, pAct = True}  = newTrain t {x=pos.x+2,y=pos.y-1} Go
								_ 																  = newTrain t pos Derailed
	| otherwise 	= case getSection {x=pos.x-1, y=pos.y} s of
						Just {sLabel = _, sPosition = _, sLeftSignal = _, sRightSignal = _, sAct = _} = newTrain t {x=pos.x-1,y=pos.y} Go
						_ 	= case getPoint {x=pos.x-1, y=pos.y} p of	
								Just {pLabel = _, pPosition = _, pOrientation = _, pAct = False}  = newTrain t {x=pos.x-2,y=pos.y} Go
								Just {pLabel = _, pPosition = _, pOrientation = nw, pAct = True}  = newTrain t {x=pos.x-2,y=pos.y+1} Go
								Just {pLabel = _, pPosition = _, pOrientation = sw, pAct = True}  = newTrain t {x=pos.x-2,y=pos.y-1} Go
								_ 																  = newTrain t pos Derailed
where pos = t.current

newTrain :: Train Position TrainState -> Train
newTrain t p s
	| t.current == t.target = {driver = t.driver, current = p, target = t.target, toRight = t.toRight, trainState = Arrived}
	| otherwise 			= {driver = t.driver, current = p, target = t.target, toRight = t.toRight, trainState = s}

getSection :: Position [Section] -> Maybe Section
getSection p [s:r]
	| s.sPosition == p 	= Just s
	| otherwise 		= getSection p r
getSection p []	 		= Nothing
	
getPoint :: Position [Point] -> Maybe Point
getPoint p [po:r]
	| po.pPosition == p	= Just po
	| otherwise			= getPoint p r
getPoint p [] 			= Nothing

designerTask :: Task Network
designerTask 	= upd (\n.{sections = n.sections, points = n.points, click = False, trains = n.trains}) network 
				-||- editSectionsTask 
				-||- editPointsTask 
				-||- imageTask False

controllerTask :: Task Network
controllerTask 	= upd (\n.{sections = n.sections, points = n.points, click = True, trains = n.trains}) network 
				-||- (editTrains) 
				-||- imageTask True

instance == Position where
	(==) p1 p2
		| p1.x == p2.x
			| p1.y == p2.y 	= True
			| otherwise 	= False
		| otherwise 		= False

instance == TrainState where
	(==) Go Go 				= True
	(==) Arrived Arrived 	= True
	(==) Derailed Derailed	= True
	(==) Wait Wait			= True
	(==) _ _				= False

instance == Orientation where
	(==) NE NE	= True
	(==) NW NW	= True
	(==) SW SW 	= True
	(==) SE SE	= True
	(==) _  _	= False

editTrains :: Task Network
editTrains = updateInformation "Edit Trains" [] [{driver = "", current = {x=0,y=0}, target = {x=0,y=0}, toRight = True, trainState = Wait}]
					>>* [OnAction ActionOk (ifValue isValid (\v->return(v)
							>>= \t.upd (\n.{sections = n.sections
												,points = n.points
												,click = n.click
												,trains = t ++ n.trains})
												network
							>>= \_.editTrains))
						, OnAction (Action "Delete All Trains" []) 
							(ifValue isValid (\v->return(v) >>= \_.deleteAllTrains >>= \_.editTrains))
						]
where isValid s = True

deleteAllTrains :: Task Network
deleteAllTrains = upd (\n.{sections = n.sections, points = n.points, click = n.click, trains = []}) network 
					
editSectionsTask :: Task Network
editSectionsTask = updateInformation "Edit sections" [] {sLabels = "", sPositions = {x=0,y=0}, sLeftSignals = False, sRightSignals = False}
					>>* [OnAction ActionOk (ifValue isValid (\v->return(v)
							>>= \sec.upd (\n.{sections 
											= [
											{sLabel=sec.sLabels, sPosition=sec.sPositions, sLeftSignal=sec.sLeftSignals, sRightSignal=sec.sRightSignals, sAct=False}]++n.sections
											, points = n.points
											, click = n.click
											, trains = n.trains}) 
											network
							>>= \_.editSectionsTask))
						, OnAction (Action "Delete All Sections" []) 
							(ifValue isValid (\v->return(v) >>= \_.deleteAllSections >>= \_.editSectionsTask))
						]
where isValid s = True

deleteAllSections :: Task Network
deleteAllSections = upd (\n.{sections = [], points = n.points, click = n.click, trains = n.trains}) network 

editPointsTask :: Task Network
editPointsTask = updateInformation "Edit Points" [] {pLabels = "", pPositions = {x=0,y=0}, pOrientations=NE}
					>>* [OnAction ActionOk (ifValue isValid (\v->return(v)
							>>= \point.upd (\n.{sections
												 = n.sections
												 , points = [{pLabel=point.pLabels, pPosition=point.pPositions, pOrientation=point.pOrientations, pAct=False}:n.points]
												 , click = n.click
												 , trains = n.trains}) 
												 network
							>>= \_.editPointsTask))
							, OnAction (Action "Delete All Sections" []) 
								(ifValue isValid (\v->return(v) >>= \_.deleteAllPoints >>= \_.editPointsTask))
							]
where isValid s = True

deleteAllPoints :: Task Network
deleteAllPoints = upd (\n.{sections = n.sections, points = [], click = n.click, trains = n.trains}) network 
	
drawBox :: Real Real String String Bool -> Image Network
drawBox w h s l c = rect (px w) (px h) 
							<@< {fill = toSVGColor s}
							<@< {onclick 
							=   \i n.{sections 	= changeSections l n.sections c
									, points 	= changePoints l n.points c
									, click 	= n.click
									, trains	= n.trains}
									, local 	= False}		
							
changeSections :: String [Section] Bool -> [Section]
changeSections label sections b 
			| b			= [invertSignalSection s label \\ s <- sections]
			| otherwise = sections

changePoints :: String [Point] Bool -> [Point]
changePoints label points b	
			| b			= [invertSignalPoint p label \\ p <- points]
			| otherwise = points

invertSignalSection :: Section String -> Section
invertSignalSection s label		={sLabel = s.sLabel
								, sPosition = s.sPosition
								, sLeftSignal = s.sLeftSignal
								, sRightSignal = s.sRightSignal
								, sAct = b}
where b | label == s.sLabel = not s.sAct
		| otherwise 		= s.sAct
						
invertSignalPoint :: Point String -> Point
invertSignalPoint p label 		={pLabel = p.pLabel
								, pPosition = p.pPosition
								, pOrientation = p.pOrientation
								, pAct = b}
where b | label == p.pLabel = not p.pAct
		| otherwise			= p.pAct
						
imageAlignList :: Int -> [ImageAlign]
imageAlignList n = [(AtLeft, AtBottom) \\ p <- [1..n]]

marginBox :: Int Int Real Real (Image Network) -> Image Network
marginBox x y w h image = margin (px 0.0, px 0.0, px ((toReal y)*h), px ((toReal x)*w)) image
					
myCircle :: Real String -> Image Network
myCircle r s = circle (px r) <@< {fill = toSVGColor s}

myLine :: Image Network
myLine = margin (px (ratio*height), px 0.0, px 0.0) (xline Nothing (px width) <@< {strokewidth = px lineWidth})

myLabel :: String -> Image Network
myLabel s = text (normalFontDef "Times New Roman" 15.0) s
				
drawSection :: Section Bool -> Image Network
drawSection s click
	| s.sLeftSignal
		| s.sRightSignal
			| signal		= marginBox s.sPosition.x s.sPosition.y width height
				    		(overlay 
				    		[(AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtBottom)] 
				    		[] 
				    		[myCircle radius "green", myCircle radius "green", myLabel s.sLabel, myLine] 
				    		(Just (drawBox width height "yellow" s.sLabel click))
				    		)						
			| otherwise		= marginBox s.sPosition.x s.sPosition.y width height
							(overlay 
							[(AtLeft, AtTop), (AtRight, AtTop), (AtMiddleX, AtBottom)] 
							[] 
							[myCircle radius "red", myCircle radius "red", myLabel s.sLabel, myLine] 
							(Just (drawBox width height "yellow" s.sLabel click))
							)
		| otherwise 
			| signal		= marginBox s.sPosition.x s.sPosition.y width height
							(overlay 
							[(AtLeft, AtTop), (AtMiddleX, AtBottom)] 
							[] 
							[myCircle radius "green", myLabel s.sLabel, myLine] 
							(Just (drawBox width height "yellow" s.sLabel click))
							)
			| otherwise		= marginBox s.sPosition.x s.sPosition.y width height
							(overlay 
							[(AtLeft, AtTop), (AtMiddleX, AtBottom)] 
							[] 
							[myCircle radius "red", myLabel s.sLabel, myLine] 
							(Just (drawBox width height "yellow" s.sLabel click))
							)
	| s.sRightSignal
		| signal			= marginBox s.sPosition.x s.sPosition.y width height
							(overlay 
							[(AtRight, AtTop), (AtMiddleX, AtBottom)] 
							[] 
							[myCircle radius "green", myLabel s.sLabel, myLine] 
							(Just (drawBox width height "yellow" s.sLabel click))
							)
		| otherwise			= marginBox s.sPosition.x s.sPosition.y width height
							(overlay 
							[(AtRight, AtTop), (AtMiddleX, AtBottom)] 
							[] 
							[myCircle radius "red", myLabel s.sLabel, myLine] 
							(Just (drawBox width height "yellow" s.sLabel click))
							)
	| otherwise 			= marginBox s.sPosition.x s.sPosition.y width height
							(overlay 
							[(AtMiddleX, AtBottom)] 
							[] 
							[myLabel s.sLabel, myLine] 
							(Just (drawBox width height "yellow" s.sLabel click))
							)
where signal = s.sAct

drawSections :: [Section] Bool -> [Image Network]
drawSections l click = [drawSection s click \\ s <- l]

drawPoints :: [Point] Bool -> [Image Network]
drawPoints l click = [drawPoint p click \\ p <- l]

drawPoint :: Point Bool -> Image Network
drawPoint p click = marginBox p.pPosition.x p.pPosition.y width (2.0*height)
					(overlay [(AtMiddleX, AtBottom)]
					[] 
					[myLabel p.pLabel, pointLine p.pOrientation signal, margin (px (ratio*height), px 0.0, px 0.0) (oriLine p.pOrientation signal)] 
					(Just (drawBox width (2.0*height) "yellow" p.pLabel click)))
where signal = p.pAct

myXLine :: Bool -> Image Network
myXLine signal
	| signal 	= xline Nothing (px width)
	| otherwise = xline Nothing (px width) <@< {strokewidth = px lineWidth}

pointLine :: Orientation Bool -> Image Network
pointLine o signal
	| o == ne 	= margin (px (height + ratio*height), px 0.0, px 0.0) (myXLine signal)
	| o == nw 	= margin (px (height + ratio*height), px 0.0, px 0.0) (myXLine signal)
	| o == sw 	= margin (px (ratio*height), px 0.0, px 0.0) (myXLine signal)
	| o == se 	= margin (px (ratio*height), px 0.0, px 0.0) (myXLine signal)
	| otherwise = margin (px (height + ratio*height - 20.0), px 0.0, px 0.0) (myXLine signal)

oriLine :: Orientation Bool -> Image Network
oriLine o signal
	| o == ne 	= line Nothing Slash (px width) (px height) <@< {strokewidth = px lw}
	| o == nw	= line Nothing Backslash (px width) (px height) <@< {strokewidth = px lw}
	| o == sw 	= line Nothing Slash (px width) (px height) <@< {strokewidth = px lw}
	| o == se 	= line Nothing Backslash (px width) (px height) <@< {strokewidth = px lw}
	| otherwise = line Nothing Slash (px width) (px height) <@< {strokewidth = px lw}
where lw | signal = lineWidth
		 | otherwise = 1.0
	
//if Bool value "click" is True than image will react on user's clicks, otherwise it won't (designer/controller difference)
displayNetwork :: Network Bool -> Image Network
displayNetwork n click
		= overlay 
		(imageAlignList ((length n.sections) + (length n.points) + (length n.trains))) 
		[] 
		((drawSections n.sections click) ++ (drawPoints n.points click) ++ (drawTrains n.trains)) 
		Nothing
		
drawTrains :: [Train] -> [Image Network]
drawTrains trains = [drawTrain t \\ t <- trains]

drawTrain :: Train -> Image Network
drawTrain t = train t.current t.toRight
		
//drawing train **************************************************************************
wheel :: Image Network
wheel = circle (px wheelSize)

wheelSpace :: Image Network
wheelSpace = empty (px wheelSpaceSize) (px wheelSpaceSize)

wheels :: Image Network
wheels = beside [] [] [wheel, wheelSpace, wheel, wheelSpace, wheel] Nothing

house :: Image Network
house = rect (px 20.0) (px 15.0) <@< {fill = toSVGColor "grey"}

train :: Position Bool -> Image Network
train p r = margin (px 0.0, px 0.0, px ((toReal p.y)*height + (1.0-ratio)*height), px ((toReal p.x)*width + width/2.0 - tWidth/2.0)) 
				   (if r flipx id (above (repeat AtMiddleX) [] [house, wheels] Nothing))
//****************************************************************************************

imageTask :: Bool -> Task Network
imageTask click =
	updateSharedInformation
		(Title "Image")
		[imageUpdate
			id										// server state (share) and view are identical
			(\s v tags -> displayNetwork s click)	// generate image
			(\s v -> v)								// update view when state changes
			(\s v -> s)								// update state when view changes
			(\_ s -> Nothing)						// no conflict handling
			(\o n.n)								// always select the new state
		]
		network