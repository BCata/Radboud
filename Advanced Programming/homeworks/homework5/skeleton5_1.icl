module skeleton5_1

/*
	Pieter Koopman, pieter@cs.ru.nl
	Advanced Programming.
	Skeleton for assignment 5.
	* To be used in a project with the environment iTasks.
	* The executable must be inside the iTask-SDK directory of the Clean distribution, 
	  or one of its subdirectories. You can either put your program there, or use the
	  set executable option of the Project Options.
	* You can also use the -sdk commandline flag to set the path.
	  Example: -sdk C:\Users\johndoe\Desktop\Clean2.4\iTasks-SDK

*/

import iTasks
import StdArray // for the size of a String

derive class iTask Idea
derive class iTask NamedIdea

:: Idea	= {title :: String, description :: Maybe Note}
:: Name	:== String

:: NamedIdea = {name :: Name, idea :: Idea}

ideaStore :: Shared [Idea]
ideaStore = sharedStore "Ideas" []

editStoredIdeas :: Task [Idea]
editStoredIdeas = updateSharedInformation "Edit ideas" [] ideaStore

viewStoredIdeas :: Task [Idea]
viewStoredIdeas = viewSharedInformation "Stored ideas" [] ideaStore

displayHeader :: Name -> Task String
displayHeader name = viewInformation "Current user:" [] name

myTasks :: Task [Idea]
myTasks = viewInformation "Enter your name" [] "Juraj"
				>>= \name.(displayHeader name ||- editStoredIdeas ||- viewStoredIdeas)
		 
ideasList :: Task [Idea]
ideasList = enterInformation "Enter your list of ideas" []

doIdentified :: (Name -> Task x) -> Task x | iTask x
doIdentified task
	=   viewInformation "Enter your name" [] "Juraj"
	>>= task // >>= for sequential composition of tasks

Start :: *World -> *World
Start world = startEngine myTasks world
