module skeleton5_2

import iTasks
import StdArray

:: Idea =
	{ title :: String
	, descr :: String }

:: NamedIdea	=
	{ idea :: Idea
	, owner :: Name
	, ideaNumber :: Display Int }

:: Name	:== String

derive class iTask Idea, NamedIdea

doIdentified :: (Name -> Task x) -> Task x | iTask x
doIdentified task
	=   viewInformation "Enter your name" [] "Juraj"
	>>= task

addInformation :: Name -> [Idea] -> [NamedIdea]
addInformation name = addInformation` 1
	where
		addInformation` n [] = []
		addInformation` n [x:xs] 
			| size x.descr > 5 = [{ idea = x, owner = name, ideaNumber = Display n} : addInformation` (n+1) xs]
			| otherwise = addInformation` n xs

editIdeas :: Name -> Task [NamedIdea]
editIdeas name = enterInformation "Add your ideas" [EnterWith \x -> addInformation name x]

mainTask =   doIdentified editIdeas
		 >>= viewInformation "The result" []
	
Start :: *World -> *World
Start world = startEngine mainTask world